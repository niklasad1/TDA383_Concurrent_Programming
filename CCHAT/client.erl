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
    #client_st {gui = list_to_atom(GUIName), nick = list_to_atom(Nick), is_conn=false, server=[], channels=[]}.
    
%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
    ?LOG({clientConnect,St,Server}),
    ServerAtom = list_to_atom(Server),
    try genserver:request(ServerAtom, {connect, self(),St#client_st.nick}) of
      ok -> 
        NewSt = St#client_st{is_conn=true, server=ServerAtom}, 
        {reply, ok, NewSt};
      user_already_connected ->
        {reply, {error,nick_taken, "Nick taken"},St}
      catch
        _:_ -> {reply, {error,server_not_reached,"Server Timeout"}, St}
    end;


%% Disconnect from server
handle(St, disconnect) when St#client_st.is_conn =:= true ->
    ?LOG({clientDisconnect,St}),
    Data = {disconnect, self(), St#client_st.nick},
    if
      St#client_st.channels =:= [] -> 
        try genserver:request(St#client_st.server,Data) of
          ok ->
	          NewSt = St#client_st{is_conn=false, server=false},
            {reply, ok, NewSt};
          true -> {reply, {error, user_not_joined, "User not in channel"}, St}
          catch
            _:_ ->
            {reply, {error, server_not_reached, "Server Timeout"}, St}
        end;
      true ->
        {reply, {error, leave_channels_first, "User still in channels"}, St}
    end;


handle(St, disconnect) ->
    {reply, {error,user_not_connected,"ALREADY DISCONNECTED"}, St};

% Join channel
handle(St, {join, Channel}) ->
    Data={join_channel, list_to_atom(Channel), self(), St#client_st.nick},
    ?LOG({"clientJoin", Data}),
    case lists:member(Channel, St#client_st.channels) of
      false ->
        try genserver:request(St#client_st.server,Data) of
          joined_channel ->
	          NewSt = St#client_st{channels = [Channel|St#client_st.channels]},
            {reply, ok, NewSt};
          cant_join_channel -> {reply, {error, user_not_joined, "User not in channel"}, St}
          catch
            _:_ ->
            {reply, {error, server_not_reached, "Server Timeout"}, St}
        end;
      true -> {reply, {error, user_already_joined, "User already in channel"}, St} 
    end;

%% Leave channel
handle(St, {leave, Channel}) ->
    Data={exit_channel, list_to_atom(Channel), self(), St#client_st.nick},
    ?LOG({"clientLeave", Data}),
    case lists:member(Channel, St#client_st.channels) of
      true ->
        try genserver:request(St#client_st.server,Data) of
          success_exit_channel ->
	          NewSt = St#client_st{channels = lists:delete(Channel, St#client_st.channels)},
            {reply, ok, NewSt};
          failed_exit_channel -> {reply, {error, user_not_joined, "User not in channel"}, St}
          catch
            _:_->
            {reply, {error, server_not_reached, "Server Timeout"}, St}
        end;
      false -> {reply, {error, user_not_joined, "User not in channel"}, St} 
    end;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    Data={send_msg, {self(),St#client_st.nick,list_to_atom(Msg)}},
    case lists:member(Channel, St#client_st.channels) of
      true ->
        % send direct to channel
        try genserver:request(list_to_atom(Channel),Data) of
          ok -> {reply, ok, St};
          true -> {reply, {error, user_not_joined, "SHould never happend"}, St}
          catch
            _:_ ->
            {reply, {error, error_not_implemented, "Server Timeout"}, St}
        end;
      false -> {reply, {error, user_not_joined, "User not in channel"}, St} 
    end;
    
    
%% Get current nick
handle(St, whoami) ->
    ?LOG({handleWhoami,St}),
    {reply, atom_to_list(St#client_st.nick), St} ;

%% Change nick
handle(St, {nick, Nick}) when St#client_st.is_conn =:= false ->
    ?LOG({"handleNick",St,Nick}),
    NewSt = St#client_st{nick = list_to_atom(Nick)}, 
    {reply, ok, NewSt};

handle(St, {nick, _}) ->
  {reply, {error, user_already_connected, "nick is only allowed to be changed offline"},St};

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    ?LOG({"handleIncomingMsg",Channel,Name,Msg,GUIName}),
    gen_server:call(GUIName, {msg_to_GUI,atom_to_list(Channel),atom_to_list(Name)++"> "++atom_to_list(Msg)}),
    {reply, ok, St}.
