<?php
/*  =========================================================================
    zmsg.php

    Multipart message class for example applications.

    Follows the ZFL class conventions and is further developed as the ZFL
    zfl_msg class.  See http://zfl.zeromq.org for more details.

    -------------------------------------------------------------------------
    Copyright (c) 1991-2010 iMatix Corporation <www.imatix.com>
    Copyright other contributors as noted in the AUTHORS file.

    This file is part of the ZeroMQ Guide: http://zguide.zeromq.org

    This is free software; you can redistribute it and/or modify it under the
    terms of the GNU Lesser General Public License as published by the Free
    Software Foundation; either version 3 of the License, or (at your option)
    any later version.

    This software is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABIL-
    ITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General
    Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
    =========================================================================

   @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
*/

class zmsg
{
    /**
     * Store the parts of the message
     *
     * @var array
     */
    private $_parts = array();

    /**
     * Socket to send and receive via
     *
     * @var ZMQSocket
     */
    private $_socket;

    /**
     * Constructor, accepts optional socket for sending/receiving.
     *
     * @param ZMQSocket $socket
     */
    public function __construct($socket = null)
    {
        $this->_socket = $socket;
    }

    /**
     * Formats 17-byte UUID as 33-char string starting with '@'
     * Lets us print UUIDs as C strings and use them as addresses
     *
     * @param  string $data
     * @return string
     */
    public function s_encode_uuid($data)
    {
        return "@" . bin2hex($data);
    }

    /**
     * Format the hex string back into a packed int.
     *
     * @param  string $data
     * @return string
     */
    public function s_decode_uuid($data)
    {
        return pack("H*", substr($data, 1));
    }

    /**
     * Set the internal socket to use for sending or receiving.
     *
     * @param  ZMQSocket $socket
     * @return Zmsg
     */
    public function set_socket(ZMQSocket $socket)
    {
        $this->_socket = $socket;

        return $this;
    }

    /**
     *  Receive message from socket
     *  Creates a new message and returns it
     *  Blocks on recv if socket is not ready for input
     *
     * @throws Exception if no socket present
     * @return Zmsg
     */
    public function recv()
    {
        if (!isset($this->_socket)) {
            throw new Exception("No socket supplied");
        }
        $this->_parts = array();
        while (true) {
            $this->_parts[] = $this->_socket->recv();
            if (!$this->_socket->getSockOpt(ZMQ::SOCKOPT_RCVMORE)) {
                break;
            }
        }

        return $this;
    }

    /**
     * Send message to socket. Destroys message after sending.
     *
     * @throws Exception if no socket present
     * @param  boolean   $clear
     * @return Zmsg
     */
    public function send($clear = true)
    {
        if (!isset($this->_socket)) {
            throw new Exception("No socket supplied");
        }
        $count = count($this->_parts);
        $i = 1;
        foreach ($this->_parts as $part) {
            $mode = $i++ == $count ? null : ZMQ::MODE_SNDMORE;
            $this->_socket->send($part, $mode);
        }
        if ($clear) {
            unset($this->_parts);
            $this->_parts = array();
        }

        return $this;
    }

    /**
     * Report size of message
     *
     * @return int
     */
    public function parts()
    {
        return count($this->_parts);
    }

    /**
     * Return the last part of the message
     *
     * @return string
     */
    public function last()
    {
        return $this->_parts[count($this->_parts)-1];
    }

    /**
     * Set the last part of the message
     *
     * @param string $set
     */
    public function set_last($set)
    {
        $this->_parts[count($this->_parts)-1] = $set;
    }

    /**
     * Return the body
     *
     * @return string
     */
    public function body()
    {
        return $this->_parts[count($this->_parts) -1];
    }

    /**
     * Set message body to provided string.
     *
     * @param  string $body
     * @return Zmsg
     */
    public function body_set($body)
    {
        $pos = count($this->_parts);
        if ($pos > 0) {
            $pos = $pos - 1;
        }
        $this->_parts[$pos] = $body;

        return $this;
    }

    /**
     * Set message body using printf format
     *
     * @return void
     */
    public function body_fmt()
    {
        $args = func_get_args();
        $this->body_set(vsprintf(array_shift($args), $args));

        return $this;
    }

    /**
     * Push message part to front
     *
     * @param  string $part
     * @return void
     */
    public function push($part)
    {
        array_unshift($this->_parts, $part);
    }

    /**
     * Pop message part off front of message parts
     *
     * @return string
     * @author Ian Barber
     */
    public function pop()
    {
        return array_shift($this->_parts);
    }

    /**
     * Return the address of the message
     *
     * @return void
     * @author Ian Barber
     */
    public function address()
    {
        $address = count($this->_parts) ? $this->_parts[0] : null;

        return (strlen($address) == 17 && $address[0] == 0) ? $this->s_encode_uuid($address) : $address;
    }

    /**
     * Wraps message in new address envelope.
     * If delim is not null, creates two part envelope.
     *
     * @param  string $address
     * @param  string $delim
     * @return void
     */
    public function wrap($address, $delim = null)
    {
        if ($delim !== null) {
            $this->push($delim);
        }
        if ($address[0] == '@' && strlen($address) == 33) {
            $address = $this->s_decode_uuid($address);
        }
        $this->push($address);

        return $this;
    }

    /**
     * Unwraps outer message envelope and returns address
     * Discards empty message part after address, if any
     *
     * @return void
     * @author Ian Barber
     */
    public function unwrap()
    {
        $address = $this->pop();
        if (!$this->address()) {
            $this->pop();
        }

        return $address;
    }

    /**
     * Dump the contents to a string, for debugging and tracing.
     *
     * @return string
     */
    public function __toString()
    {
        $string = "--------------------------------------" . PHP_EOL;
        foreach ($this->_parts as $index => $part) {
            $len = strlen($part);
            if ($len == 17 && $part[0] == 0) {
                $part = $this->s_encode_uuid($part);
                $len = strlen($part);
            }
            $string .= sprintf ("[%03d] %s %s",  $len, $part, PHP_EOL);
        }

        return $string;
    }

    /**
     * Run a self test of the Zmsg class.
     *
     * @return boolean
     * @todo See if assert returns
     */
    public static function test()
    {
        $result = true;
        $context = new ZMQContext();
        $output = new ZMQSocket($context, ZMQ::SOCKET_DEALER);
        $output->bind("inproc://zmsg_selftest");

        $input = new ZMQSocket($context, ZMQ::SOCKET_ROUTER);
        $input->connect("inproc://zmsg_selftest");

        //  Test send and receive of single-part message
        $zmsgo = new Zmsg($output);
        $zmsgo->body_set("Hello");
        $result &= assert($zmsgo->body() == "Hello");
        $zmsgo->send();

        $zmsgi = new Zmsg($input);
        $zmsgi->recv();
        $result &= assert($zmsgi->parts() == 2);
        $result &= assert($zmsgi->body() == "Hello");
        echo $zmsgi;

        //  Test send and receive of multi-part message
        $zmsgo = new Zmsg($output);
        $zmsgo->body_set("Hello");
        $zmsgo->wrap("address1", "");
        $zmsgo->wrap("address2");
        $result &= assert($zmsgo->parts() == 4);
        echo $zmsgo;
        $zmsgo->send();

        $zmsgi = new Zmsg($input);
        $zmsgi->recv();
        $result &= assert($zmsgi->parts() == 5);
        $zmsgi->unwrap();
        $result &= assert($zmsgi->unwrap() == "address2");

        $zmsgi->body_fmt("%s%s", 'W', "orld");
        $result &= assert($zmsgi->body() == "World");

        //  Pull off address 1, check that empty part was dropped
        $zmsgi->unwrap();
        $result &= assert($zmsgi->parts() == 1);

        //  Check that message body was correctly modified
        $part = $zmsgi->pop();
        $result &= assert ($part == "World");
        $result &= assert($zmsgi->parts() == 0);

        // Test load and save
        $zmsg = new Zmsg();
        $zmsg->body_set("Hello");
        $zmsg->wrap("address1", "");
        $zmsg->wrap("address2");
        $result &= assert($zmsg->parts() == 4);
        $fh = fopen(sys_get_temp_dir() . "/zmsgtest.zmsg", 'w');
        $zmsg->save($fh);
        fclose($fh);
        $fh = fopen(sys_get_temp_dir() . "/zmsgtest.zmsg", 'r');
        $zmsg2 = new Zmsg();
        $zmsg2->load($fh);
        assert($zmsg2->last() == $zmsg->last());
        fclose($fh);
        $result &= assert($zmsg2->parts() == 4);
        echo ($result ? "OK" : "FAIL"), PHP_EOL;

        return $result;
    }

    /**
     * Save a msg to a file
     *
     * @param  filehandle $fh
     * @return this
     */
    public function save($fh)
    {
        foreach ($this->_parts as $part) {
            fwrite($fh, chr(strlen($part)));
            if (strlen($part) > 0) {
                fwrite($fh, $part);
            }
        }

        return $this;
    }

    /**
     * Load a message saved with the save function
     *
     * @param  filehandle $fh
     * @return this
     */
    public function load($fh)
    {
        while (!feof($fh) && $size = fread($fh, 1)) {
            $this->_parts[] = ord($size) > 0 ? fread($fh, ord($size)) : '';
        }

        return $this;
    }
}
