-module(ts).
-export([new/0, in/2, out/2]).

% -------------API/External Functions -----------------------------------------------

% returns the PID of a new (empty) tuplespace.
new() -> 
  todo.

% returns a tuple matching Pattern from tuplespace TS. Note that this operation
% will block if there is no such tuple.
in(TS, Pattern) ->
  todo.

% puts Tuple into the tuplespace TS.
out(TS, Tuple) ->
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
