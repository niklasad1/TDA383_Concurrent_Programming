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
    #client_st {gui = list_to_atom(GUIName), nick = list_to_atom(Nick),
                is_conn=false, server=shire}.
    
%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) when St#client_st.is_conn =:= false ->
    ?LOG({clientConnect,St,Server}),
    ServerAtom = list_to_atom(Server),
    Response = genserver:request(ServerAtom, {connect, St#client_st.gui, St#client_st.nick}),
    case Response of
      ok -> 
        NewSt = St#client_st{is_conn=true, server=ServerAtom}, 
        {reply, ok, NewSt};
      user_already_connected -> {reply, {error,user_already_connected,"ALREADY CONNECTED"}, St}
    end;

handle(St, {connect, _}) ->
  {reply, {error, user_already_connected, "ALREADY CONNECTED"}, St};

%% Disconnect from server
handle(St, disconnect) when St#client_st.is_conn =:= true ->
    ?LOG({clientDisconnect,St}),
    io:fwrite("server: ~p~n", [St#client_st.server]),
    Response = genserver:request(St#client_st.server, {disconnect, St#client_st.gui, St#client_st.nick}),
    case Response of
      ok -> 
        NewSt = St#client_st{is_conn=false, server=false},
        {reply, ok, NewSt};
      _ -> {reply, {error,leave_channels_first,"ALREADY DISCONNECTED"}, St}
    end;

handle(St, disconnect) ->
    {reply, {error,user_not_connected,"ALREADY DISCONNECTED"}, St};

% Join channel
handle(St, {join, Channel}) ->
    Data={join_channel, list_to_atom(Channel), St#client_st.gui, St#client_st.nick},
    ?LOG({"clientJoin", Data}),
    Response = genserver:request(St#client_st.server,Data),
    case Response of
        joined_channel ->
	       {reply, ok, St};
	      cant_join_channel ->
         {reply, {error, user_already_joined, "Not connected or already joined"}, St}
	end;

%% Leave channel
handle(St, {leave, Channel}) ->
    Data={exit_channel, list_to_atom(Channel), St#client_st.gui, St#client_st.nick},
    ?LOG({"clientLeave", Data}),
    Response = genserver:request(St#client_st.server,Data),
    case Response of
        success_exit_channel ->
	       {reply, ok, St};
	      failed_exit_channel ->
        {reply, {error, user_not_joined, "User not in channel"}, St}
    end;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    Data={msg_from_GUI, list_to_atom(Channel), St#client_st.nick ,Msg, St#client_st.gui},
    ?LOG({"handleMsgFromGui", Data}),
    Response = genserver:request(St#client_st.server, Data),
    case Response of
	      ok -> {reply, ok, St};
        _ -> {reply, error, user_not_joined, "Tried to write message to channel when not connected"}
    end;

%% Get current nick
handle(St, whoami) ->
    ?LOG({handleWhoami,St}),
    {reply, atom_to_list(St#client_st.nick), St} ;
    % {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Change nick
handle(St, {nick, Nick}) when St#client_st.is_conn =:= false ->
    ?LOG({"handleNick",St,Nick}),
    % TODO, fix so this can only be perfomed "offline"
    NewSt = St#client_st{nick = list_to_atom(Nick)}, 
    {reply, ok, NewSt};

handle(St, {nick, _}) ->
  {reply, {error, not_implemented, "nick is only allowed to be changed offline"},St};

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    ?LOG({"handleIncomingMsg",Channel,Name,Msg}),
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
        {reply, ok, St}.
