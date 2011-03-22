--
--  Ported zmsg object from zmsg.h
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--

require"zhelpers"

local zmq = require"zmq"

local setmetatable = setmetatable
local tinsert = table.insert
local tremove = table.remove
local char = string.char
local tonumber = tonumber
local assert = assert
local stderr = io.stderr
local fprintf = fprintf

-- hex <-> binary conversion tables.
hex_to_byte_map = {}
byte_to_hex_map = {}
for i=0,255 do
    local byte = char(i)
    local hex = ('%02X'):format(i)
    -- byte to hex
    byte_to_hex_map[byte] = hex
    -- hex to byte
    hex_to_byte_map[hex] = byte
end

--  --------------------------------------------------------------------------
--  Formats 17-byte UUID as 33-char string starting with '@'
--  Lets us print UUIDs as C strings and use them as addresses
--
local function s_encode_uuid(data)
    assert(data:byte(1) == 0)
    -- remove first 0 byte
    data = data:sub(2)
    return '@' .. data:gsub("(.)", byte_to_hex_map)
end


--  --------------------------------------------------------------------------
--  Formats 17-byte UUID as 33-char string starting with '@'
--  Lets us print UUIDs as C strings and use them as addresses
--
local function s_decode_uuid(uuidstr)
    assert(uuidstr:sub(1,1) == '@')
    -- remove '@'
    uuidstr = uuidstr:sub(2)
    -- uppercase
    uuidstr = uuidstr:upper()
    return '\0' .. uuidstr:gsub("(%x%x)", hex_to_byte_map)
end


local msg_mt = {}
msg_mt.__index = msg_mt

--  --------------------------------------------------------------------------
--  Constructor, sets initial body if provided
local function zmsg_new(body)
    return setmetatable({body}, msg_mt)
end


--  --------------------------------------------------------------------------
--  Compare two messages

function msg_mt:__eq(msg2)
    local len = #self
    -- different lengths?
    if len ~= #msg2 then return false end
    for i=1,len do
        -- different parts?
        if self[i] ~= msg2[i] then
            return false
        end
    end
    -- same message contents.
    return true
end


--  --------------------------------------------------------------------------
--  Duplicate message

function msg_mt:dup()
    local dup = zmsg_new()

    for i=1,#self do
        dup[i] = self[i]
    end
    return dup
end


--  --------------------------------------------------------------------------
--  Send message to socket
--  Destroys message after sending

function msg_mt:send(socket)
    assert(socket)
    local len = #self

    for i=1,len do
        local part = self[i]

        --  Unmangle 0MQ identities for writing to the socket
        if (#part == 33 and part:sub(1,1) == '@') then
            part = s_decode_uuid (part)
        end
        socket:send(part, (i < len) and zmq.SNDMORE or 0)
        -- clear zmsg
        self[i] = nil
    end
end


--  --------------------------------------------------------------------------
--  Report size of message

function msg_mt:parts()
    return #self
end


--  --------------------------------------------------------------------------
--  Return pointer to message body, if any
--  Caller should not modify the provided data

function msg_mt:body()
    return self[#self]
end


--  --------------------------------------------------------------------------
--  Set message body as copy of provided string
--  If message is empty, creates a new message body

function msg_mt:body_set(body)
    self[#self] = body
end


--  --------------------------------------------------------------------------
--  Set message body using printf format
--  If message is empty, creates a new message body
--  Hard-coded to max. 255 characters for this simplified class

function msg_mt:body_fmt(fmt, ...)
    self[#self] = fmt:format(...)
end


--  --------------------------------------------------------------------------
--  Push message part to front of message parts

function msg_mt:push(part)
    tinsert(self, 1, part)
end


--  --------------------------------------------------------------------------
--  Pop message part off front of message parts
--  Caller should free returned string when finished with it

function msg_mt:pop()
    return tremove(self, 1)
end


--  --------------------------------------------------------------------------
--  Append message part to end of message parts

function msg_mt:append(part)
    self[#self + 1] = part
end


--  --------------------------------------------------------------------------
--  Return pointer to outer message address, if any
--  Caller should not modify the provided data

function msg_mt:address()
    return self[1]
end


--  --------------------------------------------------------------------------
--  Wraps message in new address envelope
--  If delim is not null, creates two-part envelope
--  Call this _after_ zmq_body_set(), not before

function msg_mt:wrap(address, delim)
    assert(address)

    --  Push optional delimiter and then address
    if delim then
        tinsert(self, 1, delim)
    end
    tinsert(self, 1, address)
end


--  --------------------------------------------------------------------------
--  Unwraps outer message envelope and returns address
--  Discards empty message part after address, if any
--  Caller should free returned string when finished with it

function msg_mt:unwrap()
    assert(self)

    local address = tremove(self, 1)
    -- check for deliminator
    local peek = self[1]
    if (peek and #peek == 0) then
        -- pop deliminator
        tremove(self, 1)
    end
    return address
end


--  --------------------------------------------------------------------------
--  Save message to a file

function msg_mt:save(file)
    assert(file)

    for i=1,#self do
        local part = self[i]
        -- prefix each message part with: "<8 digit hex number>:"
        file:write(("%08X:"):format(#part))
        file:write(part)
    end
end


--  --------------------------------------------------------------------------
--  Dump message to stderr, for debugging and tracing

function msg_mt:dump()
    fprintf(stderr, "--------------------------------------\n")
    for i=1,#self do
        local data = self[i]

        --  Dump the message as text or binary
        local is_text = true
        for i=1,#data do
            local c = data:byte(i)
            if (c < 32 or c > 127) then
                is_text = false
                break
            end
        end

        fprintf(stderr, "[%03d] ", #data)
        if (is_text) then
            stderr:write(data)
        else
            for i=1,#data do
                fprintf(stderr, "%02X", data:byte(i))
            end
        end
        fprintf(stderr, "\n")
    end
    stderr:flush()
end

module(...)

-- export zmsg_new()
_M.new = zmsg_new


--  --------------------------------------------------------------------------
--  Receive message from socket
--  Creates a new message and returns it
--  Blocks on recv if socket is not ready for input

function recv(socket)
    assert(socket)
    local self = zmsg_new()

    while true do
        local data, err = socket:recv()
        if not data then
            return self, err
        end
        --  We handle 0MQ UUIDs as printable strings
        if (#data == 17 and data:byte(1) == 0) then
            --  Store message part as string uuid
            data = s_encode_uuid (data)
        end
        self[#self + 1] = data

        if (socket:getopt(zmq.RCVMORE) == 0) then
            break      --  Last message part
        end
    end
    return self
end


--  --------------------------------------------------------------------------
--  Load message from file
--  Creates a new message and returns as many parts as can be read.

function load(file)
    assert(file)
    local self = zmsg_new ()

    while true do
        -- Read part prefix
        local size = file:read(9)
        if not size then break end
        -- decode part size to number
        size = tonumber(size:sub(1,8), 16)
        -- Read part from file
        local part = file:read(size)
        if not part then break end
        self[#self + 1] = part
    end
    return self
end


