-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { gui = list_to_atom(GUIName), server=ina}.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
    Data = {join_server,St#client_st.gui},
    io:fwrite("Client is sending: ~p~n", [Data]),
    ServerAtom = list_to_atom(Server),
    Response = genserver:request(ServerAtom, Data),
    io:fwrite("Client received: ~p~n", [Response]),
    case Response of
        already_in_server ->
	     {reply, {error, not_implemented, atom_to_list(Response)}, St};
	joined_server ->
	      {reply, ok, St#client_st{server=ServerAtom}}
	      end;

%% Disconnect from server
handle(St, disconnect) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

% Join channel
%TODO: deny join channel if not in server.
handle(St, {join, Channel}) ->
    Data={join_channel, list_to_atom(Channel), St#client_st.gui},
     io:format("Client is sending: ~w~n",[Data]),
    Response = genserver:request(St#client_st.server,Data),
    io:format("Client received: ~w~n",[Response]),
    case Response of
        joined_channel ->
	       {reply, ok, St};
	cant_join_channel ->
	       {reply, {error, not_implemented, "Already in channel"}, St}
	end;

%% Leave channel
handle(St, {leave, Channel}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
handle(St, whoami) ->
    % {reply, "nick", St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
