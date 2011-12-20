<?php
/* Report 0MQ version
 *
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

if(class_exists("ZMQ") && defined("ZMQ::LIBZMQ_VER")) {
	echo ZMQ::LIBZMQ_VER, PHP_EOL;
}