-module(our_test).
-export([test/0]).
-import(ts, [in/2,out/2,new/0]).


%% Module for performing a quick test on lab3 module.
%% To test your lab, compile it:
%% c(ts).
%% then compile the test:
%% c(our_test).
%% then run:
%% test:test().

%% Here is a run of the test on a correct tuplespace:

%% (node@chalmers)1> c(our_test).
%% {ok,test}
%% (node@chalmers)2> our_test:test().

test() ->
  Delay = 500,
  process_flag(trap_exit, true),
  TS = new(),
  link(TS),
  io:format("TEST: new tuplespace TS created~n", []),

  % test 1, add 10 values and remove them check so it's empty
  io:format("test 1: ~w ~n", [test_one()]),
  % test 2, invalid argument types
  io:format("test 2: ~w ~n", [test_two()]),
  % test 3: in on empty ts
  % test_three(),
  io:format("test 4; ~w ~n", [test_four()]),
  "TEST FINISHED".



%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sleep(T) ->
  receive
  after
    T -> true
  end.

test_one() ->
  Pid1 = new(),
  out(Pid1, {1,2}),
  Ret = in(Pid1, {1,2}),
  io:format("~p ~n", [Ret]), 
  
  case Ret of
    {1,2} -> pass;
    _ -> fail
  end.


test_two() ->
  Pid1 = new(),
  Ret1 = out(Pid1, [1,2,3]),
  Ret2 = out(hej, {1,2}), 
  Ret3 = in("str", {1,2}),
  Ret4 = in(Pid1, atom),

  case {Ret1,Ret2,Ret3,Ret4} of
    {incorrect_args,incorrect_args,incorrect_args,incorrect_args} -> pass;
    _ -> fail
  end.

% NOT FINISHED FIX CONCURRENCY
test_three() ->
  Pid = new(),
  % Ret1 = in(Pid, {1,2}),
  % Ret2 = in(Pid, {3,4}),
  Ret1 = spawn_link(ts, in, [Pid, {1,2}]),
  Ret2 = spawn_link(ts, in, [Pid, {1,2}]),
  sleep(500),
  out(Pid, {1,2}),
  io:format("Ret1 ~p ~n", [Ret1]),
  io:format("END").

% any etc
test_four() -> 
  Pid1 = new(),
  out(Pid1, {1,2}),
  out(Pid1, {2,99}),
  out(Pid1, {3,4}), 
  out(Pid1, {3,6}), 

  % (3,6)
  Ret1 = in(Pid1, {any,any}),
  % (3,4)
  Ret2 = in(Pid1, {any, 99}),

  Ret3 = in(Pid1, {3,any}),
  % 
  Ret4 = in(Pid1, {any, any}),

  io:format("Ret1 ~p Ret2 ~p Ret3 ~p Ret4 ~p ~n", [Ret1, Ret2, Ret3, Ret4]),


  case {Ret1,Ret2,Ret3,Ret4} of
    {{3,6},{2,99},{3,4},{1,2}} -> pass;
    _ -> fail
  end.



