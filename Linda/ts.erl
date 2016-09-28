-module(ts).
-export([new/0, in/2, out/2]).

% -------------API/External Functions -----------------------------------------------

% returns the PID of a new (empty) tuplespace.
new() -> 
  Pid = spawn_link(fun() -> loop() end),
  % may be unnecessary if we return the PID
  catch(unregister(tsserver)),
  register(tsserver, Pid),
  Pid.

% returns a tuple matching Pattern from tuplespace TS. Note that this operation
% will block if there is no such tuple.
in(TS, Pattern) ->
  Ref = make_ref(),
  TS ! {self(), Ref, Pattern},
  receive
    {From, Ref, "ACK"} -> io:fwrite("ACK From ~p Ref ~p~n", [From, Ref]);
    _ -> io:fwrite("UN-EXPECTED RESPONSE~n")
  end.

% puts Tuple into the tuplespace TS.
out(TS, Tuple) ->
  Ref = make_ref(),
  todo.
% --------------END------------------------------------------------------------------


% -------------Internal Functions -----------------------------------------------

match(any,_) -> true;

match(P,Q) when is_tuple(P), is_tuple(Q)
                -> match(tuple_to_list(P),tuple_to_list(Q));


match([P|PS],[L|LS]) -> case match(P,L) of
                              true -> match(PS,LS);
                              false -> false
                         end;

match(P,P) -> true;


match(_,_) -> false.

% --------------END------------------------------------------------------------------
 
% --------------OWN FUNCTIONS--------------------------------------------------------
loop() ->
  io:fwrite("loop() ~n"),
  receive
    {From, Ref, Data} -> 
      io:fwrite("recv: ~p ~p ~p ~n", [From, Ref, Data]),
      From ! {self(), Ref, "ACK"},
      loop()
  end.


