require 'yaml'
require 'pp'

class AcuForce

DEBUG = false

##DEPENDENCIES##
TRAVERSAL_ORDER = [:project, :sprint, :issue, :change_list, :file]

VIEW_JMP_TBL = {:project => :load_sprints, :sprint => :load_issues, :issue => :load_change_lists, :change_list => :load_files}
EDIT_OPS_TBL = {:project => {}, :sprint => {}, :issue => {:delete => :delete_issue, :tag => :tag_issue, :change_status => :cs_issue}}
NEW_OPS_TBL = {:project => :new_project, :sprint => :new_sprint, :issue => :new_issue}

##LOGIN##
SESSION_FILE = "acuforce.session"
LOGIN_URL = "https://acunote.cashnetusa.com/login"
HOME_URL = "https://acunote.cashnetusa.com/"
LOGIN_FIELDS = ['login[username]', 'login[password]']
LOGIN_FORM_NAME = "login_form"


##PAGE RETRIEVAL##
GOTO_URL = "https://acunote.cashnetusa.com/tasks/goto/<s/i>"

URL_MATCHERS = {:project =>  /https:\/\/acunote.cashnetusa.com\/projects\/\d*\/sprints\Z/,
                :sprint => /https:\/\/acunote.cashnetusa.com\/projects\/\d*\/sprints\/\d*\/show\Z/,
                :issue => /https:\/\/acunote.cashnetusa.com\/projects\/\d*\/tasks\/\d*\Z/,
                :change_list => /place holder/}


##SCRAPING##
#
#file scraping
P4_FILE_MATCHER= "grep -E \"^\.{3} \/\/\""

#cl scraping
SOURCE_CONTROL_HREF_MATCHER = /source_control\/show/
CHANGESET_MATCHER = /Changeset \[(\d*)\]/
P4_DESC_MATCHER= "sed '1,/^Description\/d'"

#issue scraping
ISSUE_ID_MATCHER = /task_item_contents_(\d*)/

#sprint scraping
PROJECT_URL = "https://acunote.cashnetusa.com/projects/<project>/sprints"
SPRINT_ID_MATCHER = /sprint_(\d*)/
SEP_ROW_MATCHERS = [/Backlogs/, /Present Sprints/, /Future Sprints/, /Past Sprints/]


##OUTPUT##
#
#The array below represents the key contents to display for the leaf nodes of
#the data tree in non verbose output mode, this array should not hold more than
#one key of the leaf node's hash. Non verbose output is purely for being able to
#pipe specific information into external applications. All Error/Attn messages
#are printed to STDERR so these should not break chaining of output.
OUTPUT_KEYS = [:number, :path] 
NO_SEPARATOR = [:file]
NO_HEADING = [:path, :state]
IN_LINE = [:path]
SEP_CHAR = " "
INDENT_CHAR = "  "
UNDERLINE_CHAR = "-"
SEP_LENGTH = 80
UNDERLINE_LENGTH = 80


##FILTERING##
#
#Filters for any key in any data struct can be added here. They should have the
#form "filter_name" => [:pos/:neg,  ['regexp','regexp',...]], notice that all
#regular expressions are in single quotes. When applied, a positive filter will
#return all items which match any of the regular expressions in the filter. A
#negative filter will return all items which match none of the regular
#expressions. The behavior is equivalent to the '-v' option in grep.
#
#filter guidelines:
# -- A filter type (negative or positive) is required 
# -- No mixing of (neg/pos) matchers is allowed within a single filter
# -- Filtering only works on text fields
# -- Custom filters can be added here or to FILTER_FILE (yaml format)
FILTER_FILE = "acuforce.filters"
PREDEFINED_FILTERS = 
  {:no_integrates => [:neg, ['\<-','(R|r)ebase','(I|i)ntegrate']]} 
                

###############################################################################

def initialize(options)
  @a = Mechanize.new
  @options = options
  @filters = parse_filters
  @edits = parse_edits
end


#Magic happens here. With this setup info about the root node can't be
#retrieved, maybe I'll fix that later.
def process(start_node)
  r = @options[:recurse] || 1

  #define and populate initial tree structure
  tree = { start_node => [] }
  @options[start_node].each { |sn| tree[start_node].push({:number => sn}) }
  arr = tree[start_node]
 
  trv_idx = TRAVERSAL_ORDER.index(start_node)
  #super unreadable breadth first creation of data tree
  while (r -= 1) >= 0 && (nxt_node = TRAVERSAL_ORDER[trv_idx += 1])  
    node = TRAVERSAL_ORDER[trv_idx - 1]
    strm_arr = []
    arr.each do |h|
      STDERR.puts "Processing #{nxt_node} for #{node} number #{h[:number]}" if DEBUG
      h[nxt_node] = send(VIEW_JMP_TBL[node], h[:number])
      #perform filtering
      available_filters = @filters[nxt_node]
      (h[nxt_node] = filter(h[nxt_node], available_filters)) if available_filters
      strm_arr << h[nxt_node]
    end
    arr = strm_arr.flatten
    #check for edit ops
  end

  end_node = TRAVERSAL_ORDER[trv_idx - 1]
  if @options[:verbose]
    output(tree)
  else
    output({node => arr})
  end
end


#Performs filtering on an array of hashes. 'items' is an array of data hashes
#as returned by one of load_* methods with the following format: 
#[{:number => "", :description => "", ...}, {...}], 
#'filters' is a hash which should contain a subset of the data hash keys, it has 
#the following format: 
#{:number => [//,//...], :description => [//,//...], ...}
def filter(items, filters)
  #positive filtering
  pos_filters = filters[:pos]
  (items = items.select {|i| matches_any?(i, pos_filters) }) unless pos_filters.empty?
  #negative filtering
  neg_filters = filters[:neg]
  (items = items.reject {|i| matches_any?(i, neg_filters) }) unless neg_filters.empty?

  items
end


#Produces output based on a number of arguments. When no options are specified,
#only the leaf node data is printed. This data can be made unique by passing the
#-u option. Verbose mode can be enable with the -v option, it pretty prints the 
#full data tree. The -d [FILE] option will dump the full data tree to a yaml file. 
def output(tree)
  #printing
  if @options[:verbose]
    pretty_print2([tree])
    #pp tree
  else
    arr = tree.values.first
    #no leaf nodes
    if arr && arr.empty? && !@options[:verbose]
      STDERR.puts "Attn: No leaf nodes returned, try the --verbose option for more details"
      return
    end
    k = OUTPUT_KEYS.find { |k| arr.first[k] }
    #make output unique
    if @options[:unique] 
      arr = arr.inject([]) { |a, h| a << h[k] }.uniq
    end
    #print hash
    arr.each do |ln|
      ln.is_a?(Hash) ? puts(ln[k]) : puts(ln.to_s)
    end
  end

  #dump to yaml
  if @options[:dump]
    File.open(@options[:dump], "w") do |f|
      f << tree.to_yaml         
    end
  end
end


#Pretty ugly pretty print
def pretty_print2(tree, level = 0, parent = nil)
  indent = (INDENT_CHAR * level)
  separator = (!SEP_CHAR.empty?) ? indent + SEP_CHAR * (SEP_LENGTH - indent.length/SEP_CHAR.length) : ""
  underline = (!UNDERLINE_CHAR.empty?) ? indent + UNDERLINE_CHAR * (UNDERLINE_LENGTH - indent.length/UNDERLINE_CHAR.length) : ""
  no_sep = NO_SEPARATOR.include?(parent)
  #used to avoid printing markup during stack collapse
  rec_flg = false
  
  tree.each do |h|
    #sort the hash by the length of the values while placing the child array at
    #the end.
    a = h.sort do |kv1,kv2| 
      if kv1[1].is_a?(Array) || kv2[1].nil? 
        1
      elsif kv2[1].is_a?(Array) || kv1[1].nil?
        -1
      else 
        #sort hash keys in alph order
        kv1[0].to_s <=> kv2[0].to_s
      end
    end

    a.each do |(k,v)|
      printf indent
      if v.is_a? Array
        rec_flg = true
        puts "#{pluralize(to_heading(k))}:\n#{underline}" unless NO_HEADING.include?(k)
        #print children
        pretty_print2(v, level + 1, k)
      else
        printf "#{to_heading(k)}: " unless NO_HEADING.include?(k)
        #trim value to max length and print       
        content = v ? (v.split("\n").first) : "" 
        IN_LINE.include?(k) ? printf("#{content.strip}") : puts(content)
      end
    end
    #record separator
    (puts separator unless no_sep) unless rec_flg
  end
  #list separator
  puts separator unless rec_flg
end


def login(force = false)
  return true if @logged_in

  #Going to assume the session is good to save time here. The session will be
  #discarded and a force login will be performed if get_page fails.
  if !force && File.exists?(SESSION_FILE) && @a.cookie_jar = YAML.load_file(SESSION_FILE) 
    STDERR.puts "Loaded session file" if DEBUG
    return @logged_in = true
  end

  #In case force login is called without credentials
  return false unless @options[:username] && @options[:password]

  #try to log in
  p = get_page(LOGIN_URL) 
  STDERR.puts "Navigated to '#{p.title}'" if DEBUG

  form = p.forms.first
  form[LOGIN_FIELDS[0]] = @options[:username]
  form[LOGIN_FIELDS[1]] = @options[:password]
  p = form.submit(form.buttons.first)

  unless p.uri.to_s ==  HOME_URL
    STDERR.puts "Error: Bad login!"
    exit
  end
  STDERR.puts "Navigated to '#{p.title}'" if DEBUG

  #serialize session and save for later reuse
  File.open(SESSION_FILE, "w") do |f|
    f << @a.cookie_jar.to_yaml
  end
  @logged_in = true
end


#Grabs all sprints for a specific project  
def load_sprints(project_num)
  (return [] unless login) unless @logged_in
  sprints = []
  sep_rows = []

## get project page
  project_url = PROJECT_URL.gsub('<project>', project_num)
  p = get_page(project_url, URL_MATCHERS[:project])
  return [] unless p
  STDERR.puts "Navigated to '#{p.title}'" if DEBUG

## get active sprints
  rows = p.search("//tr")
  rows.each_with_index do |row, i|
    sep_rows << i if SEP_ROW_MATCHERS.any? { |m| row.text =~ m } 
  end

## I only care about present sprints
  #backlog_rows = rows[sep_rows[0]+1...sep_rows[1]]
  present_rows = rows[sep_rows[1]+1...sep_rows[2]]
  #future_rows = rows[sep_rows[2]+1...sep_rows[3]]
  #past_rows = rows[sep_rows[3]+1...-1]
  
  present_rows.each do | r|
    if r['id'] =~ SPRINT_ID_MATCHER
      sprints <<  {:number => $1, :description => r.children[4].text}
    end
  end
  
  #sprints.each do |s| puts "#{s[:number]} - #{s[:description]}" end
  sprints
end


#Grabs all issues for a specific sprint
def load_issues(sprint_num)
  (return [] unless login) unless @logged_in
  issues = []

## get sprint page
  sprint_url = GOTO_URL.gsub('<s/i>', "s" + sprint_num) 
  p = get_page(sprint_url, URL_MATCHERS[:sprint])
  return [] unless p
  STDERR.puts "Navigated to '#{p.title}'" if DEBUG

  rows = p.search("//tr")
  rows.each do |r|
    if r['id'] =~ ISSUE_ID_MATCHER
      issue_number_container = p.search("//a[@id='issue_number_for_#{$1}']")
      desc_container = p.search("//div[@id='task_description_link_#{$1}']/span")
      owner_container = p.search("//div[@id='task_owner_id_link_#{$1}']")
      status_container = p.search("//div[@id='task_status_#{$1}']")
      issues <<  {:number => issue_number_container ? issue_number_container.text : nil, 
                  :description => (desc_container && desc_container.children[1]) ? desc_container.children[1].text.strip : nil,
                  :owner =>  owner_container ? owner_container.text.strip : nil,
                  :status => status_container ? status_container.text.strip : nil}
    end
  end

  #issues.each do |i| puts "#{i[:number]} - #{i[:description]}" end
  issues
end


#Grabs all change lists for a specific issue
def load_change_lists(issue_num)
  (return [] unless login) unless @logged_in
  change_lists = []

## get issue page
  issue_url = GOTO_URL.gsub('<s/i>', issue_num)
  p = get_page(issue_url, URL_MATCHERS[:issue])
  return [] unless p
  STDERR.puts "Navigated to '#{p.title}'" if DEBUG

## get CL links and extract CL numbers
  cl_links = p.links_with(:href => SOURCE_CONTROL_HREF_MATCHER)
  cl_nums = cl_links.inject([]) do |cls, l|
    l.text.match(CHANGESET_MATCHER)
    cls << $1
  end.compact if cl_links

## pull p4 data for CLs 
  cl_nums.each do |cl|
    if @options[:p4]
      p4_desc = `p4 change -o #{cl} | #{P4_DESC_MATCHER}`
      p4_desc.strip! if p4_desc
    else
      #TODO: implement scraper
      nil
    end
    change_lists << {:number => cl, :description => p4_desc}
  end if cl_nums

## print
  #change_lists.each do |cl|
    #puts "CL #{cl[:number]}"
    #cl[:files].each do |f| puts "  #{f}" end
  #end
  change_lists
end


#Grabs all files from a specific change list
def load_files(cl_number)
  if @options[:p4]
    p4_lines =  `p4 describe #{cl_number} | #{P4_FILE_MATCHER}`.split("\n")

    #cleanup lines
    cl_files = p4_lines.inject([]) do |cl_files, l|
      l =~ /(\/\/.*)#\d*\s*(.*)/
      path = $1
      state = $2.strip
      path ? (cl_files << {:path => path, :state => state}) : cl_files
    end
  else 
    #TODO: implement scraper 
    []
  end
end


#Places valid filters into a manageable struct. There isn't much syntax
#checking here so follow the filter guidelines for consistent results.
def parse_filters
  if raw_filters = @options[:filter]
      filters = {}
      #load predefined filters
      predef_filters = (File.exists?(FILTER_FILE) ? YAML.load_file(FILTER_FILE).merge(PREDEFINED_FILTERS) : PREDEFINED_FILTERS)

      raw_filters.each do |f|
        f.strip!
        #only the first colon matters here
        split_pos = f.index(":")
        node_key, raw_regexps = f[0...split_pos], f[split_pos + 1..-1]
        
        node = TRAVERSAL_ORDER.find {|n| node_key.index(n.to_s) == 0 }.to_s
        #TODO: make a lookup table for the keys to avoid syntax checks
        key =  node_key[node.length + 1..-1]
        unless node && raw_regexps
          STDERR.puts "Attn: invalid filter <#{f}>"
          next
        end

        #sort positive and negative filters
        node = node.to_sym
        filters[node] ||= {:pos => {}, :neg => {}}

        #check for predefined filters
        if raw_regexps =~ /\A\/([^\/]*\/)*\Z/
          regexps = raw_regexps.split("/").delete_if {|s| s.empty?}
          if key[-1..-1] == '^'
            filters[node][:neg][key[0...-1].to_sym] = regexps
          else
            filters[node][:pos][key.to_sym] = regexps
          end
        elsif predef_filter = PREDEFINED_FILTERS[raw_regexps.to_sym]
          filters[node][predef_filter[0]][key.to_sym] = predef_filter[1]
        else next
        end
      end

      if DEBUG
        puts "Raw filters: " 
        pp raw_filters
        puts "Parsed filters: "
        pp filters 
      end

      filters
    else {}
    end
end
private :parse_filters


#validate edit operations 
def parse_edits
  edits = {}
  if raw_edits = @options[:edit]
    raw_edits.each do |re|
      re.strip!
      #only the first colon matters here
      split_pos = re.index(":")
      node_op, data = re[0...split_pos], re[split_pos + 1..-1]

      #lookup node and operation in appropriate tables, this avoids syntax checks
      node = TRAVERSAL_ORDER.find {|n| node_op.index(n.to_s) == 0 }
      op =  node ? EDIT_OPS_TBL[node].keys.find {|op| op == node_op[node.to_s.length + 1..-1].to_sym} : nil

      edits[node] = {op => data} if node && op
    end

    if DEBUG
      puts "Raw edits: " 
      pp raw_edits
      puts "Parsed edits: "
      pp edits
    end
  else {}
  end
end


#Retrieves the requested page and verifies destination url to make sure there
#was no innapropriate redirect. If redirected, a force login will be performed
#(assuming credentials are passed in as arguments) and the page will be retrieved 
#again. 
def get_page(url, matcher = /.*/, retry_count = 1)
  creds = @options[:username] && @options[:password]

  begin
    page = @a.get(url)
    if page.uri.to_s =~ matcher
      page
    else
      #try a force login and retry once (in case the session is stale)
      if creds && retry_count > 0 && login(true)
        STDERR.puts "Attn: get_page problem, overwrote stale session, retrying..."
        get_page(url, matcher, retry_count - 1)
      else STDERR.puts "Error: Can't retrieve valid response page for <#{url}>"
      end
    end
  rescue Mechanize::ResponseCodeError => e
    STDERR.puts "ResponseError!"
  end
end
private :get_page


#Returns true if the value (data string) for each key matches any of the regexps in the
#filter for that key.
def matches_any?(i, filters)
  i.all? do |k,v|
     if regexps = filters[k]
       regexps.any? do |r|
         v =~ /#{r}/
       end
     else true
     end
  end
end
private :matches_any?


def to_heading(sym)
  sym.to_s.split("_").map {|s| s.capitalize}.join(" ")
end
private :to_heading


def pluralize(word)
  word.to_s + "s"
end
private :pluralize


end
