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
  case find(St#server_st.conn, Gui) of
    not_found -> 
      NewSt = St#server_st{conn = St#server_st.conn ++ [Gui]},
      io:format("~w~n",[NewSt#server_st.conn]),
      {reply,ok,NewSt};
    found -> {reply, user_already_connected ,St}
  end; 

handle(St, {disconnect, Gui, Nick}) ->
  ?LOG({"serverDisconnect", Gui, Nick}),
  % search channel list for joined channels
  % 
  X = users_in_channel(St#server_st.channels, Nick),
  io:format("User in channel ~p ~n",[X]),
  case X of
      found -> {reply, leave_channels_first,St#server_st{}};
      not_found -> reply_handler(St, Gui, disconnect)
  end;
	       
handle(St,{join_channel, Ch, Gui, Nick}) ->
    X = client_connect(St, Gui),
    ?LOG({"serverJoin", St#server_st.conn, X}),
    case X of
      not_connected -> {reply, cant_join_channel,St#server_st{}};
      connected -> reply_handler(St, Ch, Gui,Nick, join)
    end;

handle(St,{exit_channel, Ch, Gui, Nick}) ->
    X = client_connect(St, Gui),
    ?LOG({"serverExit", St#server_st.conn, X}),
    case X of
      not_connected -> {reply, failed_exit_channel,St#server_st{}};
      connected -> reply_handler(St, Ch, Gui,Nick, exit)
    end;

handle(St, {msg_from_GUI, Channel, Nick, Msg, GuiName}) ->
    X = client_connect(St, GuiName),
    ?LOG({"serverExit", St#server_st.conn, X}),
    case X of
      not_connected -> {reply, error,St#server_st{}};
      connected -> reply_handler(St, Channel,Nick,Msg,GuiName)
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

find([First|Rest], A) ->
    case First=:=A of
       true ->
           found;
       false ->
           find(Rest,A)
    end;
find([],_) ->
    not_found.


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

delete([First|Rest],A) ->
    if
      First=:=A ->  
        Rest;
      true ->
        [First|delete(Rest,A)]
    end;
delete([], _) ->
    [].

% client_connect????? 
client_connect(St,Gui) ->
 % loop through St#conn if found return true else return false
 case find(St#server_st.conn, Gui) of 
    found -> connected;
    not_found -> not_connected
 end.

reply_handler(St, Ch, Gui,Nick, join) ->
    PriorList=St#server_st.channels,
    NewCh = do(St#server_st.channels, Ch, Gui,Nick, join),
    case NewCh =:=PriorList of
            true ->
	         {reply, cant_join_channel, St#server_st{}};
            false ->
	         {reply, joined_channel, St#server_st{channels=NewCh}}
    end;
reply_handler(St, Ch, Gui,Nick, exit) ->
    PriorList=St#server_st.channels,
    NewCh = do(St#server_st.channels, Ch, Gui,Nick, exit),
    case NewCh =:=PriorList of
            true ->
	         {reply, failed_exit_channel, St#server_st{}};
            false ->
	         {reply,success_exit_channel, St#server_st{channels=NewCh}}
    end;

reply_handler(St, Channel,Nick,Msg,GuiName) ->
    L=findchannel_list(St#server_st.channels, Channel),
    case L of
     does_not_exist -> {reply, {error, error, "TODO"},St};
     _ ->
        % channel exist, then find nick
        % if nick in channel then do ->
        sendmessages(L, {Channel, Nick, Msg, GuiName}),
        {reply, ok, St}
    end.
reply_handler(St, Gui, disconnect) ->
  case find(St#server_st.conn, Gui) of
         found -> 
            NewSt = St#server_st{conn = delete(St#server_st.conn, Gui)},
                  {reply,ok,NewSt};
         not_found -> {reply, user_not_conneted ,St}
  end. 



% {channel,[{gui,nick}]}
users_in_channel([{_,{Gui,Nick}}|Rest], N) ->
     case Nick=:=N of
         true -> found;
	       false -> users_in_channel(Rest,N)
     end;
users_in_channel([], _) ->
     not_found.

