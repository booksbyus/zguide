#!/usr/bin/env ruby

#
#  Report 0MQ version
#

require 'rubygems'
require 'ffi-rzmq'

version = LibZMQ.version
puts "Current 0MQ version is %d.%d.%d\n" \
    % [version[:major], version[:minor], version[:patch]]
