-module(server).
-export([handle/2, initial_state/1,requestDontWaitForAnswer/2]).
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

handle(St, {connect, Pid, Nick}) ->
  ?LOG({"serverConnect", Pid, Nick}),
  case lists:member(Nick,St#server_st.conn) of
    false -> 
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
	       

handle(St,{join_channel,Ch,Pid,Nick}) ->
  ?LOG({"serverJoinChannel", Ch}),
  case lists:member(Ch,St#server_st.channels) of
    false -> 
      channel:start(Ch, channel:initial_state(Ch), fun channel:handle/2),
      requestDontWaitForAnswer(Ch, {join, {Pid,Nick}}),
      % genserver:request(Ch, {join, {Pid,Nick}}),
      NewSt = St#server_st{channels = [Ch|St#server_st.channels]},
      {reply,joined_channel,NewSt};
    true ->
      requestDontWaitForAnswer(Ch, {join, {Pid,Nick}}),
      % genserver:request(Ch, {join, {Pid,Nick}}),
      {reply,joined_channel,St}
  end;

handle(St,{exit_channel,Ch,Pid,Nick}) ->
  ?LOG({"serverExitChannel", St}),
  case lists:member(Ch,St#server_st.channels) of
    false ->
      {reply, failed_exit_channel, St};
    true -> 
     requestDontWaitForAnswer(Ch, {leave, {Pid,Nick}}),
     % genserver:request(Ch, {leave, {Pid,Nick}})
     {reply,success_exit_channel,St} 
  end;

handle(St,{msg_from_GUI,Ch,Nick,Msg,Pid}) ->
  ?LOG({"msg_from_GUI", St}),
  case lists:member(Ch,St#server_st.channels) of
    false ->
      {reply, failed_exit_channel, St};
    true -> 
     requestDontWaitForAnswer(Ch, {send_msg, {Pid,Nick,Msg}}),
     % genserver:request(Ch, {send_msg, {Pid,Nick,Msg}}),
     {reply,ok,St} 
  end.


requestDontWaitForAnswer(Pid, Msg) ->
  Pid ! {request, self(),make_ref(),Msg}.
