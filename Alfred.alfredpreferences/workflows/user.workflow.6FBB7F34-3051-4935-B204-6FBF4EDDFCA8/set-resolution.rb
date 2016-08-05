#!/usr/bin/env ruby
# encoding: utf-8

query = ARGV[0]

if query =~ /^\d+$/
  `osascript -e 'tell app "Alfred 2" to search "setresolution #{query} "'`
else
  # extract values
  display, width, height, bits, hidpi = ARGV[0].match(/(\d+)@(\d+)x(\d+)@(\d+)(h?)/).captures

  `./resolution-cli set #{display} #{width}x#{height}@#{bits}#{hidpi}`; result= $?.success?

  unless result
    `/usr/bin/afplay /System/Library/Sounds/Funk.aiff`
    exit 1
  end

  print "#{width}x#{height}"
end