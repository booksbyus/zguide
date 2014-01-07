# Report 0MQ version
#
# Author: Lev Givon <lev(at)columbia(dot)edu>

import zmq

print("Current libzmq version is %s" % zmq.zmq_version())
print("Current  pyzmq version is %s" % zmq.__version__)
