//Multipart message class for example applications.

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Collections.Generic;
using System.Text;
using ZMQ;

namespace ZMsg {

    public class ZMessage {
        Encoding _encoding = Encoding.Unicode;
        List<byte[]> _msgParts = new List<byte[]>();

        public ZMessage() { }

        public ZMessage(Socket skt) {
            Recv(skt);
        }

        public ZMessage(string body, Encoding encoding) {
            _encoding = encoding;
            _msgParts.Add(_encoding.GetBytes(body));
        }

        public ZMessage(string body) {
            _msgParts.Add(_encoding.GetBytes(body));
        }

        public ZMessage(byte[] body) {
            _msgParts.Add(body);
        }

        public void Recv(Socket socket) {
            Queue<byte[]> msgParts = socket.RecvAll();

            while (msgParts.Count > 0) {
                _msgParts.Insert(0, (msgParts.Dequeue()));
            }
        }

        public void Send(Socket socket) {
            for (int index = _msgParts.Count - 1; index > 0; index--) {
                socket.SendMore(_msgParts[index]);
            }
            socket.Send(_msgParts[0]);
        }

        public string BodyToString() {
            return _encoding.GetString(Body);
        }

        public void StringToBody(string body) {
            Body = _encoding.GetBytes(body);
        }

        public void Append(byte[] data) {
            _msgParts.Insert(0, data);
        }

        public byte[] Pop() {
            byte[] data = _msgParts[_msgParts.Count - 1];
            _msgParts.RemoveAt(_msgParts.Count - 1);
            return data;
        }

        public void Push(byte[] data) {
            _msgParts.Add(data);
        }

        public void Wrap(byte[] address, byte[] delim) {
            if (delim != null) {
                _msgParts.Add(delim);
            }
            _msgParts.Add(address);
        }

        public byte[] Unwrap() {
            byte[] addr = Pop();
            if (Address.Length == 0) {
                Pop();
            }
            return addr;
        }

        public int PartCount {
            get { return _msgParts.Count; }
        }

        public byte[] Address {
            get { return _msgParts[_msgParts.Count - 1]; }
            set { _msgParts.Add(value); }
        }

        public byte[] Body {
            get { return _msgParts[0]; }
            set { _msgParts[0] = value; }
        }
    }
}

