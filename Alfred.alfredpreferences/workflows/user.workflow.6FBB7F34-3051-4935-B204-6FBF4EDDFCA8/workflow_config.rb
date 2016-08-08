require_relative 'bundle/bundler/setup'
require 'alfred'
require 'yaml'

class WorkflowConfig

  @displays

  @file

  def initialize
    @file = 'display_resolutions.yml'
    load_display_resolutions
  end

  def get_displays
    @displays
  end

  def get_current_resolution display
    current = `./resolution-cli current #{display}`
    width, height, bits, hidpi = current.match(/^(\d+) x (\d+) @ (\d+) bits ?(HiDPI)?$/).captures
    mode = {
        width: width,
        height: height,
        bits: bits,
        hidpi: !!hidpi,
        dpi: !!hidpi ? 'HiDPI' : 'normal resolution',
        id: "#{display}@#{width}x#{height}@#{bits}#{!!hidpi ? 'h' : ''}"
    }

  end

  def get_resolutions display
    @displays[display][:modes]
  end

  def get_resolution(display, id)
    resolution = @displays[display][:modes].detect do |resolution|
      resolution[:id] == "#{display}@#{id}"
    end
    resolution
  end

  def remove_resolution(display, id)
    @displays[display][:modes].reject! do |resolution|
      true if resolution[:id] == "#{display}@#{id}"
    end

    # write config back
    write_config
  end


  def load_display_resolutions
    @displays = YAML.load(File.open @file) if File.exist? @file
    rebuild_resolutions if !@displays
  end

  def rebuild_resolutions
    # get full resolution list from cli app, and parse data into array of hashes
    list = `./resolution-cli list`
    list.strip!

    @displays = list.scan(/^\d+: (.+)$/).map { |d| {:name => d[0], :modes => [], :current_mode => ''} }
    list.split(/^\d+: .+$/)[1..-1].each_with_index do |modes_string, display_index|
      modes_string.strip.split("\n").each do |mode_string|
        match = /^(?<current>>>>)?\s*(?<width>\d+)\s+x\s+(?<height>\d+) @ (?<bits>\d+) bits ?(?<dpi>HiDPI)?$/.match(mode_string)
        mode = {
            width: match[:width],
            height: match[:height],
            bits: match[:bits],
            hidpi: !!match[:dpi],
            dpi: !!match[:dpi] ? 'HiDPI' : 'normal resolution',
            id: "#{display_index}@#{match[:width]}x#{match[:height]}@#{match[:bits]}#{!!match[:dpi] ? 'h' : ''}"
        }
        @displays[display_index][:modes] << mode
        @displays[display_index][:current_mode] = mode[:id] if match[:current]
      end
    end

    # removes 16 bits resolutions
    @displays.each do |display|
      display[:modes].reject! do |mode|
        true if mode[:bits] == '16'
      end
    end



    # removes normal resolutions that are available as HiDPI
    @displays.each do |display|
      display[:modes].reject! do |mode|
        false if mode[:hidpi]
        true if !mode[:hidpi] && display[:modes].any? { |m| m[:id] == "#{mode[:id]}h"}
      end
    end

    # remove displays with only one option
    @displays.reject! { |display| true if display[:modes].length <= 1 }

    # write config back to yaml
    write_config
  end


  def write_config
    File.open(@file, 'w') { |f| f.write(@displays.to_yaml) }
  end

  private :load_display_resolutions, :write_config
end