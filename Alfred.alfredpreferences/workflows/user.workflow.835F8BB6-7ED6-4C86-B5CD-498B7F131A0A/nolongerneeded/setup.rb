Encoding.default_external = Encoding::UTF_8 if defined? Encoding

require 'yaml'
require 'fileutils'

#global_config_file="config/setup.yml"
#until we know otherwise force config location (make sure this always has a trailing slash... we assume it will)
default_config_location="~/Library/Application Support/Alfred 2/Workflow Data/com.clintonstrong.SearchTabs/"
example_sites_config="config/sites_example.yml"
example_setup_config="config/setup_example.yml"
setup_file_name="setup.yml"
sites_file_name="sites.yml"

query = ARGV[0].strip.downcase
tabs = BrowserTabs.tabs
fb = Feedback.new

def copy_with_path(src, dst)
  FileUtils.mkdir_p(File.dirname(dst))
  FileUtils.cp(src, dst)
end

if !File.file?(File.expand_path(default_config_location + setup_file_name)) 
setup_config_destination = File.expand_path(default_config_location + setup_file_name)
sites_config_destination = File.expand_path(default_config_location + sites_file_name)
copy_with_path(example_setup_config, setup_config_destination)
copy_with_path(example_sites_config, sites_config_destination)
else
global_config = begin
  YAML.load(File.open(File.expand_path(default_config_location + setup_file_name)))
rescue ArgumentError => e
  puts "Could not parse YAML: #{e.message}"
end

default_config_location=global_config['config_location']

end

if File.exist?(File.expand_path('"' + query + '"'))

puts "Path Exists"

#    data = YAML.load_file File.open(File.expand_path(default_config_location + setup_file_name))
#    data["config_location"] = "{query}"
#    File.open(File.expand_path(default_config_location + setup_file_name), 'w') { |f| YAML.dump(data, f) }
    
