% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
-record(client_st, {gui,nick,client,is_conn,server,channels}).

% This record defines the structure of the server process.
% Add whatever other fields you need.
-record(server_st, {servers, conn, channels}).

% Record for the channel process
-record(ch_st, {name, channel}).
