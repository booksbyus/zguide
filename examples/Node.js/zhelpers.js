// Return the buffer's length as a three-character,
// zero-padded string (e.g. printf's `%03d`)
function bufferLength(buffer) {
  var lenStr = "" + buffer.length;
  while (lenStr.length < 3) {
    lenStr = "0" + lenStr;
  }

  return lenStr;
}

// Return the buffer's contents as printable text if every
// character is printable, or as hexadecimal otherwise
function formatBuffer(buffer) {
  for (var i = 0; i < buffer.length; i++) {
    if (buffer[i] < 32 || buffer[i] > 127) {
      return buffer.toString("hex")
    }
  }

  return buffer.toString("utf8");
}

module.exports = {
  dumpFrames: function() {
    var frames;
    if (arguments.length == 1) {
      var arg = arguments[0];
      if (Array.isArray(arg)) {
        // Single argument is an array of frames (buffers)
        frames = arg;
      } else {
        // Single argument is a single frame (buffer)
        frames = [arg];
      }
    } else {
      // Multiple arguments; each is a frame (buffer)
      frames = Array.prototype.slice.call(arguments);
    }

    console.log("----------------------------------------");
    frames.forEach(function(frame) {
      console.log("[%s] %s", bufferLength(frame), formatBuffer(frame));
    });
  }
};
