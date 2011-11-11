#
#  Report 0MQ version
#

require 'rubygems'
require 'ffi-rzmq'

puts "Current 0MQ version is %d.%d.%d\n" % ZMQ::Util.version
