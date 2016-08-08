#!/usr/bin/env ruby
# encoding: utf-8

require_relative 'workflow_config.rb'

config = WorkflowConfig.new
config.rebuild_resolutions

puts 1
