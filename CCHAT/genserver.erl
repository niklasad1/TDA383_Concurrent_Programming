-module(genserver).
-export([start/3, request/2, request/3, update/2]).
-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.

%% Spawn a process and register it with a given atom
%% Function F should have arity 1
start(Atom, State, F) ->
  % ?LOG({"spawn process", Atom}),
  Pid = spawn(fun() -> loop(State, F) end),
  catch(unregister(Atom)),
  register(Atom, Pid),
  Pid.

loop(State, F) ->
  ?LOG({"genserverLoop", State, F}),
  receive
    {request, From, Ref, Data} ->
      ?LOG({"request", Ref, Data}),
      maybeWait(From),
      case catch(F(State, Data)) of
        {'EXIT', Reason} ->
          From!{exit, Ref, Reason},
          loop(State, F);
        {reply, R, NewState} ->
          ?LOG({"reply", R, NewState}),
          From!{result, Ref, R},
          loop(NewState, F)
        end;
    {update, From, Ref, NewF} ->
      From ! {ok, Ref},
      loop(State, NewF);
    stop ->
      true
  end.

%% Send a request to a Pid and wait for a response
request(Pid, Data) ->
  ?LOG({"genserverRequest", Pid, Data}),
  request(Pid, Data, 3000).

%% Send a request to a Pid and wait for a response
%% With a specified timeout
request(Pid, Data, Timeout) ->
  ?LOG({"genserverRequestTimeout", Pid, Data}),
  Ref = make_ref(),
  Pid!{request, self(), Ref, Data},
  receive
    {result, Ref, Result} ->
      Result;
    {exit, Ref, Reason} ->
      exit(Reason)
  after Timeout ->
    exit("Timeout")
  end.

%% Update loop function
update(Pid, Fun) ->
  ?LOG({"genserverUpdate", Pid, Fun}),
  Ref = make_ref(),
  Pid!{update, self(), Ref, Fun},
  receive
    {ok, Ref} ->
      ok
  end.

%% If process sleepy exists, ask her if we should sleep
maybeWait(FromPid) ->
  ?LOG({"genserverMaybeWait", FromPid}),
  case whereis(sleepy) of
    undefined -> ok ;
    Pid ->
      Pid ! {hi, self(), FromPid},
      receive
        {wait, N} -> timer:sleep(N) ;
        {go} -> ok
      after 100 ->
        ok
      end
  end.
