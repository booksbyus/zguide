def s_dump(sock)
  puts "------------------------------------"
  # Build an array to hold all the parts
  messages = []
  sock.recvmsgs(messages)
  # messages is an array of ZMQ::Message objects
  messages.each do |msg|
    if msg == messages[0]
      # identity - Naive implementation
      msg.size == 17 ? puts("Identity: #{msg.copy_out_string.unpack('H*')[0]}") : puts("Identity: #{msg.copy_out_string}")
    else
      # body
      puts "Data: #{msg.copy_out_string}"
    end
  end
end
