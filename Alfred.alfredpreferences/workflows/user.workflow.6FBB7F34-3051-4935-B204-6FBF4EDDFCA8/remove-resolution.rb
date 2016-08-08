#!/usr/bin/env ruby
# encoding: utf-8

require_relative 'workflow_config.rb'

config = WorkflowConfig.new
display, id = ARGV[0].match(/(\d+)@(.+)/).captures
display = display.to_i
resolution = config.get_resolution display, id
config.remove_resolution display, id

print "#{resolution[:width]}x#{resolution[:height]}"
