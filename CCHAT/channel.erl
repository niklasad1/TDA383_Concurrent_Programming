-module(channel).
-include_lib("./defs.hrl").
-export([start/3, initial_state/0]).
-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.

initial_state() ->
    #ch_st{channel = []}.

start(Atom, State, F) ->
  Pid = spawn(fun() -> loop(State,F) end),
  catch(unregister(Atom)),
  register(Atom,Pid).  

loop(State, F) ->
  receive
    {request, From, Ref, Data} ->
      {reply, R, NewState} = F(State, Data),
      From!{result, Ref, R},
      loop(NewState, F)
  end.

handle(St, {join, {Pid,Nick}}) ->
  ?LOG({"channelJoin", St}),
  {reply,ok,St};

handle(St, {leave, {Pid,Nick}}) ->
  ?LOG({"channelLeave", St}),
  {reply,ok,St};

handle(St, {send_msg, {Pid,Nick,Msg}}) ->
  ?LOG({"channelSendMsg", St}),
  {reply,ok,St}.
