<?php
/*
 *  Interrupt in PHP
 *  Shows how to handle CTRL+C
 * @author Nicolas Van Eenaeme <nicolas(at)poison(dot)be>
 */

declare(ticks=1); // PHP internal, make signal handling work
if (!function_exists('pcntl_signal'))
{
    printf("Error, you need to enable the pcntl extension in your php binary, see http://www.php.net/manual/en/pcntl.installation.php for more info%s", PHP_EOL);
    exit(1);
}

$running = true;
function signalHandler($signo)
{
    global $running;
    $running = false;
    printf("Warning: interrupt received, killing server...%s", PHP_EOL);
}
pcntl_signal(SIGINT, 'signalHandler');

$context = new ZMQContext();

//  Socket to talk to clients
$responder = new ZMQSocket($context, ZMQ::SOCKET_REP);
$responder->bind("tcp://*:5558");

while ($running)
{
    //  Wait for next request from client
    try
    {
        $string = $responder->recv(); //  The recv call will throw an ZMQSocketException when interrupted
        // PHP Fatal error:  Uncaught exception 'ZMQSocketException' with message 'Failed to receive message: Interrupted system call' in interrupt.php:35
    }
    catch (ZMQSocketException $e)
    {
        if ($e->getCode() == 4) //  4 == EINTR, interrupted system call (Ctrl+C will interrupt the blocking call as well)
        {
            usleep(1); //  Don't just continue, otherwise the ticks function won't be processed, and the signal will be ignored, try it!
            continue; //  Ignore it, if our signal handler caught the interrupt as well, the $running flag will be set to false, so we'll break out
        }

        throw $e; //  It's another exception, don't hide it to the user
    }

    printf("Received request: [%s]%s", $string, PHP_EOL);

    //  Do some 'work'
    sleep(1);

    //  Send reply back to client
    $responder->send("World");
}

//  Do here all the cleanup that needs to be done
printf("Program ended cleanly%s", PHP_EOL);
