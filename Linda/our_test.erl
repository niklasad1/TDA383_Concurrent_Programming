-module(our_test).
-export([test/0]).
-import(ts, [in/2,out/2,new/0]).


%% Module for performing a quick test on lab3 module.
%% To test your lab, compile it:
%% c(ts).
%% then compile the test:
%% c(our_test).
%% our_test.erl:147: Warning: function slave/2 is unused
%% {ok,our_test}
%% then run:
%% our_test:test().

%% Here is a run of the test on a correct tuplespace:

%% (node@chalmers)1> c(our_test).
%% our_test.erl:147: Warning: function slave/2 is unused
%% {ok,test}
%% (node@chalmers)2> our_test:test().
%% test1: pass
%% test2: pass
%% test3: pass
%% test4: pass
%% ok

test() ->

  % test 1, add 10 values and remove them check so it's empty
  io:format("test 1: ~w ~n", [test_one()]),
  % test 2, invalid argument types
  io:format("test 2: ~w ~n", [test_two()]),
  % test 3: in on empty ts
  io:format("test 3: ~w ~n", [test_three()]),
  io:format("test 4: ~w ~n", [test_four()]),
  end_of_tests.

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

% Test of Concurrency
test_three() ->
  Delay = 500,
  process_flag(trap_exit, true),
  TS = new(),
  link(TS),
  io:format("TEST: new tuplespace TS created~n", []),

  % runs in separate process
  Pid1 = spawn_in_test(TS, {fish,any}),
  sleep(Delay),

  % runs in a separate process
  Pid2 = spawn_in_test(TS, {fish,any}),
  sleep(Delay),

  out_test(TS, {fish,salmon}),
  sleep(Delay),

  out_test(TS, {fish,tuna}),
  sleep(Delay),

  % wait for answer
  Ret1 = replytest(Pid1, {fish,any}, {fish,salmon}),
  sleep(Delay),
  Ret2 =replytest(Pid2, {fish,any}, {fish,tuna}),
  sleep(Delay),
  receive
    {_,_} ->
      io:format("Should not happend~n")
  after
    1000 ->
      io:format("Correct. Tuplespace appears to be empty.~n"),
      exit(TS, this_is_it),
      collect_exits([Pid1, Pid2, TS]),
      finished
  end,
  case {Ret1,Ret2} of
    {pass, pass} -> pass;
    _ -> fail
  end.

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



out_test(Tuplespace, Tup) ->
  io:format("TEST: out(TS, ~w)~n", [Tup]),
  out(Tuplespace, Tup).

% spawns a slave task to perform an in test. This function
% returns the slave's Pid. The slave will forward the result of the
% in operation to the caller.

spawn_in_test(Tuplespace, Pat) ->
  S = spawn_link(test, slave, [Tuplespace, {self(), Pat}]),
  io:format("TEST: in(TS, ~w) by process ~w~n", [Pat, S]),
  S.

%% Get a tuple matching Item from Tuplespace T and send it to Pid
slave(T, {Pid,Item}) ->
  case in(T, Item) of
    R -> Pid!{self(), R}
  end.

%% Tests whether the reply from a Slave task matches the expected Tuple
replytest(Slave, Pat, Tup) ->
  io:format("Process ~w~n", [Slave]),
  receive
    {Slave,Tup} ->
      io:format("     Correct. in operation: ~w returned tuple: ~w~n", [Pat, Tup]),
      pass;
    {Slave,Bad} ->
      io:format("     Error. in with pattern: ~w returned tuple: ~w~n", [Pat,Bad]),
      fail
  after
    5000 ->
      io:format("     Error. No response for in operation with pattern: ~w~n", [Pat])
  end.

collect_exits([]) ->
  done;
collect_exits([Pid | Pids]) ->
  receive
    {'EXIT', Pid, _} ->
      collect_exits(Pids)
  end.
