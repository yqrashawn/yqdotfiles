# Dropzone Action Info
# Name: Copy to lmv
# Description: Copy to lmv/sampleModels/gbim
# Handles: Files
# Creator: yqrashawn
# URL: http://yqrashawn.com
# Events: Clicked, Dragged
# KeyModifiers: Command, Option, Control, Shift
# SkipConfig: No
# RunsSandboxed: Yes
# Version: 1.0
# MinDropzoneVersion: 3.0

require 'find'
require "rubygems"
require "json"
require "pathname"

$configFile = ENV["HOME"] +'/.config/dropzone.d'
$config = File.read($configFile) # string
$config = JSON.parse($config) # ruby data structure
# puts $config
if $config['Lmv_sampleModels_gbim']['path'].start_with?('~')
  $config['Lmv_sampleModels_gbim']['path']['~'] = ENV["HOME"]
end
$cpPath =  $config['Lmv_sampleModels_gbim']['path']
$relative_level =  $config['Lmv_sampleModels_gbim']['relative_level']
# puts $cpPath


def dragged
  # Welcome to the Dropzone 3 API! It helps to know a little Ruby before playing in here.
  # If you haven't coded in Ruby before, there's an excellent introduction at http://www.codecademy.com/tracks/ruby

  # Each meta option at the top of this file is described in detail in the Dropzone API docs at https://github.com/aptonic/dropzone3-actions/blob/master/README.md#dropzone-3-api
  # You can edit these meta options as required to fit your action.
  # You can force a reload of the meta data by clicking the Dropzone status item and pressing Cmd+R

  # This is a Ruby method that gets called when a user drags items onto your action.
  # You can access the received items using the $items global variable e.g.
  # puts $items

  # puts File.read($items)
  # The above line will list the dropped items in the debug console. You can access this console from the Dropzone settings menu
  # or by pressing Command+Shift+D after clicking the Dropzone status item
  # Printing things to the debug console with puts is a good way to debug your script.
  # Output printed with puts will be shown in red in the console

  # You mostly issue commands to Dropzone to do things like update the status - for example the line below tells Dropzone to show
  # the text "Starting some task" and show a progress bar in the grid. If you drag a file onto this action now you'll see what I mean
  # All the possible $dz methods are described fully in the API docs (linked up above)
  $dz.begin("Starting some task...")
  $items.each do |dir|
    # puts dir
    cpPath = $cpPath + dir.slice(dir.rindex('/') + 1, dir.length - 1)
    # puts cpPath # /Users/yqrashawn/workspace/OFFICE/gltflmvviewer/sampleModels/gbim/test
    project_path = cpPath
    $relative_level.times do
      project_path = project_path.slice(0, project_path.rindex('/'))
    end
    project_path = project_path.slice(0, project_path.rindex('/') + 1)
    puts project_path # /Users/yqrashawn/workspace/OFFICE/gltflmvviewer/
    FileUtils.copy_entry(dir, cpPath)
    gbimAry = Array.new
    Find.find(cpPath) do |e|
      if !File.directory?(e) and File.extname(e) == '.gbim'
      # if !File.directory?(e)
        # puts e
        gbim = Pathname.new(e)
        gbim = gbim.relative_path_from(Pathname.new(project_path)).to_path
        # puts gbim
        gbimAry.push('./' + gbim)
      end
    end
    # puts gbimAry.inspect
    $infoStr = JSON.pretty_generate(JSON.parse(gbimAry.to_json))
    File.open(cpPath + '/info.json', 'w+') { |file| file.write($infoStr) }
  end

  # Below line switches the progress display to determinate mode so we can show progress
  $dz.determinate(true)

  # Below lines tell Dropzone to update the progress bar display
  $dz.percent(50)
  sleep(1)
  $dz.percent(100)

  # The below line tells Dropzone to end with a notification center notification with the text "Task Complete"
  $dz.finish("Task Complete")

  # You should always call $dz.url or $dz.text last in your script. The below $dz.text line places text on the clipboard.
  # If you don't want to place anything on the clipboard you should still call $dz.url(false)
  $dz.text($infoStr)
end

def clicked
  # This method gets called when a user clicks on your action
  $dz.finish("Open sampleModels folder")
  $dz.url(false)
  %x`open #{$cpPath}`
end
