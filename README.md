# ErlChat

Simple client-server chat, that can be used on separated Erlang nodes.

**Server features:**

- start() - start server and register it as 'chatServer'
- stop() - stop current server

**Client features:**

- connect(Name) -> Pid - to local server with name
- connect(Host, Name) -> Pid - to remote node with name
- disconnect(Pid) - disconnect from server
- who_is_here(Pid) - retrieve all connected users
- message(Pid, Message) - send any message to all clients
