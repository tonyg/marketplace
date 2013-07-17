from __future__ import with_statement
from SocketServer import ThreadingMixIn, TCPServer, StreamRequestHandler
from threading import RLock

active_connections = set()
active_connection_mutex = RLock()

class ChatServer(ThreadingMixIn, TCPServer):
    allow_reuse_address = True

counter = 0

class ConnectionHandler(StreamRequestHandler):
    def handle(self):
        global counter
        self.connection_id = 'user' + str(counter)
        counter = counter + 1
        try:
            self.arrive()
            while True:
                line = self.rfile.readline()
                if not line: break
                for c in active_connections.copy():
                    c.announce('%s says %s' % (self.connection_id, line.strip()))
        finally:
            self.depart()

    def arrive(self):
        self.announce('you are %s' % (self.connection_id,))
        with active_connection_mutex:
            for c in active_connections:
                self.announce('%s arrived' % (c.connection_id,))
                c.announce('%s arrived' % (self.connection_id,))
            active_connections.add(self)

    def depart(self):
        with active_connection_mutex:
            active_connections.discard(self)
            for c in active_connections:
                c.announce('%s departed' % (self.connection_id,))

    def announce(self, s):
        try:
            self.wfile.write(s + '\n')
        except IOError:
            self.depart()

if __name__ == '__main__':
    s = ChatServer(('localhost', 5999), ConnectionHandler)
    s.serve_forever()
