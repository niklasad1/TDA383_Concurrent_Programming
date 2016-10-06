-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").
-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    ?LOG({"initialState",Nick,GUIName}),
    #client_st {gui = GUIName, nick = Nick, is_conn=false, server=false}.
    
%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
    ?LOG({handleConnect,St,Server}),
    ServerAtom = list_to_atom(Server),
    Response = genserver:request(ServerAtom, {connect, St#client_st.nick}),
    case Response of
      ok -> 
        NewSt = St#client_st{is_conn=true, server=ServerAtom}, 
        {reply, ok, NewSt};
      user_already_connected -> {reply, {error,user_already_connected,"ALREADY CONNECTED"}, St}
    end;

%% Disconnect from server
handle(St, disconnect) ->
    ?LOG({handleDisconnect,St}),
    io:fwrite("server: ~p~n", [St#client_st.server]),
    Response = genserver:request(St#client_st.server, {disconnect, St#client_st.nick}),
    case Response of
      ok -> 
        NewSt = St#client_st{is_conn=false, server=false},
        {reply, ok, NewSt};
      _ -> {reply, {error,user_already_connected,"ALREADY CONNECTED"}, St}
    end;

% Join channel
handle(St, {join, Channel}) ->
    ?LOG({handleJoin,St,Channel}),
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Leave channel
handle(St, {leave, Channel}) ->
    ?LOG({handleLeave,St,Channel}),
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    ?LOG({handleMsgFromGui,St,Channel,Msg}),
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
handle(St, whoami) ->
    ?LOG({handleWhoami,St}),
    {reply, St#client_st.nick, St} ;
    % {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    ?LOG({"handleNick",St,Nick}),
    % TODO, fix so this can only be perfomed "offline"
    NewSt = St#client_st{nick = Nick}, 
    {reply, ok, NewSt};

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    ?LOG({"handleIncomingMsg",Channel,Name,Msg}),
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
        {reply, ok, St}.
