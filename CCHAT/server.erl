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

handle(St, {connect, Gui, Nick}) ->
  ?LOG({"serverConnect", Gui, Nick}),
  case lists:member(Nick,St#server_st.conn) of
    false -> 
      % timer:sleep(3000),
      NewSt = St#server_st{conn = [Nick|St#server_st.conn]},
      io:format("~w~n",[NewSt#server_st.conn]),
      {reply,ok,NewSt};
    true -> {reply, user_already_connected ,St}
  end; 

handle(St, {disconnect, Gui, Nick}) ->
  ?LOG({"serverDisconnect", Gui, Nick}),
  case lists:member(Nick,St#server_st.conn) of
     false -> {reply, user_not_connected ,St};
     true -> 
      NewSt = St#server_st{conn = lists:delete(Nick,St#server_st.conn)},
      {reply,ok,NewSt}
  end; 
	       
handle(St,{join_channel, Ch, Gui, Nick}) ->
    PriorList=St#server_st.channels,
    NewCh = do(St#server_st.channels, Ch, Gui,Nick, join),
    case NewCh =:=PriorList of
            true ->
	         {reply, cant_join_channel, St#server_st{}};
            false ->
	         {reply, joined_channel, St#server_st{channels=NewCh}}
    end;

handle(St,{exit_channel, Ch, Gui, Nick}) ->
    PriorList=St#server_st.channels,
     % St#server_st.channels append to 
     % if nick is in channels ret user_already in channel
     % else append to channels []
% do([{Channel,ChL}|Rest],Ch, Gui, Nick, join) ->
    NewCh = do(St#server_st.channels, Ch, Gui, Nick, exit),
    case NewCh =:=PriorList of
            true ->
	         {reply, failed_exit_channel, St#server_st{}};
            false ->
	         {reply, success_exit_channel, St#server_st{channels=NewCh}}
    end;

handle(St, {msg_from_GUI, Channel, Nick, Msg, GuiName}) ->
    ?LOG({"serverMsgFromGUI", Channel, Nick, Msg, GuiName}),
    L=findchannel_list(St#server_st.channels, Channel),
    case L of
     does_not_exist -> {reply, {error, error, "TODO"},St};
     _ ->    
        % channel exist, then find nick
        sendmessages(L, {Channel, Nick, Msg, GuiName}),
        {reply, ok, St}
    end. 
     
findchannel_list([{FirstChannel,L}|Rest], InputChannel) ->
     case FirstChannel=:=InputChannel of
         true ->
	      L;
	 false ->
	      findchannel_list(Rest,InputChannel)
     end;

findchannel_list([], _) ->
     does_not_exist.

sendmessages([{G,_}|Rest], {Channel, Nick, Msg, MessageSender}) ->
      case G =:= MessageSender of
          true ->
	          sendmessages(Rest, {Channel, Nick, Msg, MessageSender});
	        false ->
	          spawn_link(fun() ->
	          gen_server:call(G, {msg_to_GUI, atom_to_list(Channel), atom_to_list(Nick)++"> "++Msg}) end),
	          sendmessages(Rest,{Channel, Nick, Msg, MessageSender})
       end; 

sendmessages([],_) ->
    ok.

% add and do (temp names) adds a gui to a channel (or server) if not already in it
add([{G,N}|Rest], {Gui, Nick}, join) ->
    ?LOG({"addJoin", {G, N, Rest}}),
    % {gui, nick} - ChL 
    case N=:=Nick of
          true ->
         [{G,N}|Rest];
    false ->
         [{G,N}] ++ add(Rest,{Gui,Nick}, join)
         end;
add([{G,N}|Rest], {Gui,Nick}, exit) ->
     case N=:=Nick of
          true ->
        Rest;
    false ->
        [{G,N}] ++ add(Rest,{Gui,Nick}, exit)
     end;
add([], {G,N}, join) ->
  io:format("Add to ch list ~p ~p ~n",[G,N]),
  [{G,N}];
add([],_, exit) ->
  [].

do([{Channel,ChL}|Rest],Ch, Gui, Nick, join) ->
      case Channel=:=Ch of
            true ->
	         [{Channel,add(ChL, {Gui,Nick},join)}] ++ Rest;
            false ->
	         [{Channel,ChL}]++do(Rest,Ch,Gui, Nick, join)
      end;
do([{Channel,L}|Rest],PotentialChannel, Gui,Nick, exit) ->
      case Channel==PotentialChannel of
            true ->
	         [{Channel, add(L,{Gui,Nick},exit)}] ++ Rest;
            false ->
	         [{Channel, L}]++do(Rest,PotentialChannel,Gui,Nick,exit)
      end;
do([],PotentialChannel,Gui,Nick, join) ->
[{PotentialChannel,[{Gui,Nick}]}];
do([], _, _, _, exit) ->
[].

