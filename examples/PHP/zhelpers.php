<?php

/**
 * zhelpers is not exposed to PHP, so here is a brief equivalent.
 */
function s_dump($socket) {
	echo "----------------------------------------", PHP_EOL;
	while(true) {
		$message = $socket->recv();
		$size = strlen($message);
		printf ("[%03d] ", $size);
		$is_text = true;
		for($i = 0; $i < $size; $i++) {
			if(ord($message[$i]) < 32 || ord($message[$i]) > 127) {
				$message = bin2hex($message);
				break;
			}
		}
		
		echo $message . PHP_EOL;
		
		if(!$socket->getSockOpt(ZMQ::SOCKOPT_RCVMORE)) {
			break;
		}
	}
}