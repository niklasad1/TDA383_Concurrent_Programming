-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").
-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.
%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{server = ServerName, conn = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients
%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, {connect, Client}) ->
  ?LOG({"serverConnect", Client}),
  case find(St#server_st.conn, Client) of
    not_found -> 
      NewSt = St#server_st{conn = St#server_st.conn ++ [Client]}, 
      {reply,ok,NewSt};
    found -> {reply, user_already_connected ,St}
  end; 

handle(St, {disconnect, Client}) ->
  ?LOG({"serverDisConnect", Client}),
  case find(St#server_st.conn, Client) of
     found -> 
      NewSt = St#server_st{conn = list:delete(St#server_st.conn, Client)},
      {reply,ok,NewSt};
     not_found -> {reply, user_not_conneted ,St}
  end; 

handle(St, {join, Client, Channel} ) ->
  ?LOG({"serverJoinChannel", Client}),
    % TODO check ch state etc, check if user is already connect or not
    % if not in channel list -> add channel + user, 
    % elif channel in channelist but not user add user
    % else user and ch in channel list ret, "user_already_joined"
    case found of 
      found -> {reply,ok,St};
      not_found -> {reply, user_already_joined ,St}
  end; 

handle(St, {msg_from_GUI, Client, Channel} ) ->
  ?LOG({"serverMsgFromGUI", Client}),
      {reply, error, St};

handle(St, {leave, Client, Channel} ) ->
  ?LOG({"serverLeaveChannel", Client}),
      {reply, error, St}.

find([First|Rest], A) ->
    if
      First =:= A -> found;
      true -> find(Rest,A)
    end;
find([],_) ->
    not_found.

