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
      NewSt = St#server_st{conn = delete(St#server_st.conn, Client)},
      {reply,ok,NewSt};
     not_found -> {reply, user_not_conneted ,St}
  end; 

handle(St, Request) ->
    ?LOG({"serverHandle", St,Request}),
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, Response, St}.

find([First|Rest], A) ->
    if
      First =:= A -> found;
      true -> find(Rest,A)
    end;
find([],_) ->
    not_found.

delete([First|Rest],A) ->
    if
      First=:=A ->
        Rest;
      true ->
        [First|delete(Rest,A)]
    end;
 
delete([], A) ->
    [].

