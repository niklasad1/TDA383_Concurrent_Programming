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
    #server_st{servers = ServerName, conn = [], channels = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients
%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, {connect, ClientName}) ->
  ?LOG({"serverConnect", ClientName}),
  case find(St#server_st.conn, ClientName) of
    not_found -> 
      NewSt = St#server_st{conn = St#server_st.conn ++ [ClientName]}, 
      {reply,ok,NewSt};
    found -> {reply, user_already_connected ,St}
  end; 

handle(St, {disconnect, ClientName}) ->
  ?LOG({"serverDisconnect", ClientName}),
  case find(St#server_st.conn, ClientName) of
     found -> 
      io:format("FOUND IN LIST TIME TO DELETE"),
      NewSt = St#server_st{conn = delete(St#server_st.conn, ClientName)},
      {reply,ok,NewSt};
     not_found -> {reply, user_not_conneted ,St}
  end; 

handle(St,{join_server,ClientName}) ->
io:format("check1  ~w~n",[ClientName]),
    case find(St#server_st.servers,ClientName) of
         {found,_} ->
	       {reply, already_in_server,St#server_st{}};
	 {not_found,_} ->
	       A=St#server_st{servers=St#server_st.servers++[ClientName]},
	       io:format("check  ~w~n",[A]),
	       {reply, joined_server, A#server_st{}}
	       end;
	       
handle(St,{join_channel,PotentialChannel,ClientName}) ->
     PriorList=St#server_st.channels,
     PostList=do(PriorList,PotentialChannel, ClientName, join),
     case PostList=:=PriorList of
            true ->
	         {reply, cant_join_channel, St#server_st{}};
            false ->
	         {reply, joined_channel, St#server_st{channels=PostList}}
     end;

handle(St,{exit_channel, PotentialChannel, ClientName}) ->
     PriorList=St#server_st.channels,
     PostList=do(PriorList, PotentialChannel, ClientName, exit),
     case PostList=:=PriorList of
          true ->
              {reply, failed_exit_channel, St#server_st{}};
          false ->
	            {reply, success_exit_channel, St#server_st{channels=PostList}}
     end;


handle(St, {msg_from_GUI, Channel, ClientName, Msg} ) ->
  ?LOG({"serverMsgFromGUI", Channel, ClientName, Msg}),
      % TODO!!!!!! 
      % find all clients, by unique name(PID), broadcast to all connect clients
      % in chatroom, think about concurrency!!!!, may spawn a process for each
      % client
      % Possiby store messages in buffer until sent
      {reply, ok, St}.


find([First|Rest], A) ->
    if
      First =:= A -> found;
      true -> find(Rest,A)
    end;
find([],_) ->
    not_found.

% add and do (temp names) adds a gui to a channel (or server) if not already in it
add([F|R],ClientName, join) ->
     case F==ClientName of
          true ->
	       [F|R];
	  false ->
	       [F] ++ add(R,ClientName, join)
	       end;
add([F|R],ClientName,exit) ->
     case F==ClientName of
          true ->
	      R;
	  false ->
	      [F] ++ add(R,ClientName, exit)
     end;
add([], ClientName, join) ->
  [ClientName];
add([],_, exit) ->
  [].
do([{Channel,L}|Rest],PotentialChannel, ClientName, join) ->
      case Channel==PotentialChannel of
            true ->
	         [{Channel,add(L, ClientName,join)}] ++ Rest;
            false ->
	         [{Channel,L}]++do(Rest,PotentialChannel,ClientName,join)
      end;
do([{Channel,L}|Rest],PotentialChannel, ClientName, exit) ->
      case Channel==PotentialChannel of
            true ->
	         [{Channel, add(L,ClientName,exit)}] ++ Rest;
            false ->
	         [{Channel, L}]++do(Rest,PotentialChannel,gui,exit)
      end;
do([],PotentialChannel,ClientName, join) ->
[{PotentialChannel,[ClientName]}];
do([], _, _, exit) ->
[].

delete([First|Rest],A) ->
    if
      First=:=A ->  
        Rest;
      true ->
        [First|delete(Rest,A)]
    end;
delete([], _) ->
    [].

