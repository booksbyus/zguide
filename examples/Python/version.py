#
#   Report 0MQ version
#

import zmq

version = zmq.core.version.pyzmq_version()
print "Current 0MQ version is", version
