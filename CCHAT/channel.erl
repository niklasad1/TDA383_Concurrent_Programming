-module(channel).
-include_lib("./defs.hrl").
-export([start/3, initial_state/1, handle/2]).
-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.

% list with Pid and Nick
% name of the channel
initial_state(Atom) ->
    #ch_st{name = Atom, channel = []}.

start(Atom, State, F) ->
  ?LOG({"startChannel", State, F}),
  Pid = spawn(fun() -> loop(State,F) end),
  catch(unregister(Atom)),
  register(Atom,Pid).  

loop(State, F) ->
  ?LOG({"channelLoop",State,F}),
  receive
    {request, From, Ref, Data} ->
      {reply, R, NewState} = F(State,Data),
      From!{result, Ref, R},
      loop(NewState, F)
  end.

handle(St, {join, {Pid,Nick}}) ->
  ?LOG({"channelJoin", St}),
  % assumes clients in already in channel can come here
  % un-necessary check
  case lists:member({Pid,Nick}, St#ch_st.channel) of
    true ->
      {reply, error, St};
    false ->  
      NewSt = St#ch_st{channel = [{Pid,Nick}|St#ch_st.channel]},
      {reply,ok,NewSt}
  end;

handle(St, {leave, {Pid,Nick}}) ->
  ?LOG({"channelLeave", St}),
  % assumes clients not in the channel can come here
  % un-necessary check 
  case lists:member({Pid,Nick}, St#ch_st.channel) of
    true ->
      NewSt = St#ch_st{channel = lists:delete({Pid,Nick},St#ch_st.channel)},
      {reply,ok,NewSt};
    false ->  
      {reply,error,St}
  end;

% spawn send messages in separate processes and reply back to the sender without
% waiting
handle(St, {send_msg, {Pid,Nick,Msg}}) ->
  ?LOG({"channelSendMsg", St}),
  spawn(fun() -> send(St#ch_st.channel, {St#ch_st.name, Pid,Nick,Msg,St}) end),
  {reply,ok,St}.

send([{R_Pid,_}|Rest], {Ch, Pid, Nick, Msg, St}) ->
    ?LOG({"sendTOCLIENT"}),
    case R_Pid =:= Pid of
          false ->
            spawn(fun() -> 
                      genserver:request(R_Pid,{incoming_msg, Ch, Nick, Msg})
                  end),
            send(Rest,{Ch, Pid, Nick, Msg, St});
          true ->
            send(Rest, {Ch, Pid, Nick, Msg, St})
      end;

send([], _) ->
  ok.

requestDontWaitForAnswer(Pid, Msg) ->
  Pid ! {request, self(),make_ref(),Msg}.

