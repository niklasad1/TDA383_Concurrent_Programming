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
    #server_st{servers = ServerName, conn = [], channels = maps:new()}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients
%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, {connect, Pid, Nick}) ->
  ?LOG({"serverConnect", Pid, Nick}),
  case lists:member(Nick,St#server_st.conn) of
    false -> 
      % timer:sleep(3000),
      NewSt = St#server_st{conn = [Nick|St#server_st.conn]},
      io:format("~w~n",[NewSt#server_st.conn]),
      {reply,ok,NewSt};
    true -> {reply, user_already_connected ,St}
  end; 

handle(St, {disconnect, Pid, Nick}) ->
  ?LOG({"serverDisconnect", Pid, Nick}),
  case lists:member(Nick,St#server_st.conn) of
     false -> {reply, user_not_connected ,St};
     true -> 
      NewSt = St#server_st{conn = lists:delete(Nick,St#server_st.conn)},
      {reply,ok,NewSt}
  end; 
	       
% handle(St,{join_channel, Ch, Gui, Nick}) ->
%     PriorList=St#server_st.channels,
%     NewCh = do(St#server_st.channels, Ch, Gui,Nick, join),
%     case NewCh =:=PriorList of
%             true ->
%            {reply, cant_join_channel, St#server_st{}};
%             false ->
%            {reply, joined_channel, St#server_st{channels=NewCh}}
%     end;

handle(St,{join_channel,Ch,Pid,Nick}) ->
  ?LOG({"serverJoinChannel", St}),
  case maps:is_key(Ch,St#server_st.channels) of
    false ->
      NewSt = St#server_st{channels =
                           maps:put(Ch,[{Pid,Nick}],St#server_st.channels)},
      {reply, joined_channel, NewSt};
    true -> 
      io:format("ISKEY TRUE~n"),
      List = maps:get(Ch,St#server_st.channels),
      ?LOG({"serverKey_ISKEY", List}),
      case lists:member({Pid,Nick},List) of
           false -> 
              NewSt = maps:update(Ch,[{Pid,Nick}|List],St#server_st.channels),
              io:format("NewSt key already in channel ~p ~n", [NewSt]),
              {reply, joined_channel, St#server_st{channels = NewSt}};
           true ->
              {reply, cant_join_channel, St#server_st{}}
      end
  end;
      %
      % case lists:key({Gui,Nick},St#server_st.channels) of
      %      false -> ok
      % end;

% handle(St,{exit_channel, Ch, Gui, Nick}) ->
%     PriorList=St#server_st.channels,
%      % St#server_st.channels append to
%      % if nick is in channels ret user_already in channel
%      % else append to channels []
% % do([{Channel,ChL}|Rest],Ch, Gui, Nick, join) ->
%     NewCh = do(St#server_st.channels, Ch, Gui, Nick, exit),
%     case NewCh =:=PriorList of
%             true ->
%            {reply, failed_exit_channel, St#server_st{}};
%             false ->
%            {reply, success_exit_channel, St#server_st{channels=NewCh}}
%     end;

handle(St,{exit_channel,Ch,Pid,Nick}) ->
  ?LOG({"serverExitChannel", St}),
  case maps:is_key(Ch,St#server_st.channels) of
    false ->
      {reply, failed_exit_channel, St};
    true -> 
      List = maps:get(Ch,St#server_st.channels),
      ?LOG({"serverExitChannel_ISKEY", List}),
      case lists:member({Pid,Nick},List) of
           false -> 
              io:format("NOT IN CHANNEL~n"),
              {reply, failed_exit_channel, St};
           true ->
              io:format("IN CHANNEL REMOVE"),
              NewSt = maps:update(Ch,lists:delete({Pid,Nick},List),St#server_st.channels),
              {reply, success_exit_channel, St#server_st{channels = NewSt}}
      end
  end;

% handle(St, {msg_from_GUI, Channel, Nick, Msg, GuiName}) ->
%     ?LOG({"serverMsgFromGUI", Channel, Nick, Msg, GuiName}),
%     L=findchannel_list(St#server_st.channels, Channel),
%     case L of
%      does_not_exist -> {reply, {error, error, "TODO"},St};
%      _ ->
%         % channel exist, then find nick
%         sendmessages(L, {Channel, Nick, Msg, GuiName}),
%         {reply, ok, St}
%     end.
    

handle(St,{msg_from_GUI,Ch,Nick,Msg,Pid}) ->
  ?LOG({"msg_from_GUI", St}),
  case maps:is_key(Ch,St#server_st.channels) of
    false ->
      {reply, {error,error}, St};
    true -> 
      List = maps:get(Ch,St#server_st.channels),
      ?LOG({"serverMG_ISKEY", List}),
      case lists:member({Pid,Nick},List) of
           false -> 
              io:format("NOT IN CHANNEL~n"),
              {reply, {error,error}, St};
           true ->
              io:format("IN CHANNEL SEND"),
              Res = genserver:request(Pid,{incoming_msg, Ch, Nick, Msg}),
              io:format("GOT REPLY ~p ~n",[Res]),
              {reply,ok,St}
      end
  end.


% send([{R_Pid,R_Nick}|Rest], {Ch, S_Nick, S_Pid, Msg,St}) ->
%     ?LOG({"sendServer",R_Pid,R_Nick, Rest}),
%     case R_Pid =:= S_Pid of
%           false ->
%             M = {incoming_msg, Ch, S_Nick, Msg},
%             io:format("SEND TO ~p DATA ~p ~n", [R_Pid,M]),
%             genserver:request(R_Pid, {incoming_msg}),
%             % send(Rest,{Ch, S_Nick, S_Pid,Msg}),
%             {reply, ok, St};
%           true ->
%             io:format("DONT SEND TO YOURSELF ~n"),
%             send(Rest, {Ch, S_Nick, S_Pid, Msg})
%       end;

send([], _) ->
  ok.
