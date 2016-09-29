-module(ts).
-export([new/0, in/2, out/2]).

% -------------API/External Functions -----------------------------------------------

% returns the PID of a new (empty) tuplespace.
new() -> 
  Pid = spawn_link(fun() -> loop([], []) end),
  % may be unnecessary if we return the PID
  catch(unregister(tsserver)),
  register(tsserver, Pid),
  Pid.

% returns a tuple matching Pattern from tuplespace TS. Note that this operation
% will block if there is no such tuple.
in(TS, Tuple) ->
  Ref = make_ref(),
  TS ! {take,self(), Ref, Tuple},
  receive
     {From, Take} -> io:fwrite("ACK From ~p Ref ~n", [From]);
      _ -> io:fwrite("UN-EXPECTED RESPONSE~n")
  end.

% puts Tuple into the tuplespace TS.
out(TS, Tuple) ->
  Ref = make_ref(),
  TS ! {put, self(), Ref, Tuple},
  receive
    {From, Put} -> io:fwrite("REPLY ~p~n", [From]);
    _ -> io:fwrite("UN-EXPECTED RESPONSE~n")
  end.
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
loop(List, Queue) ->
  io:fwrite("List ~p ~n", [List]),
  receive
    {put, From, Ref, Data} ->
           NewList=tryPutData(List,Data,From,Ref),
           reply(From, input);
    {take, From, Ref, Data} ->
           % if in list then remove and reply
           % else wait and add to waiting queue   
           NewList=tryTakeData(List,Data,From,Ref),
           reply(From, take);
    true -> io:format("WRONG FORMAT ~n"),
            NewList=List
  end,
   loop(NewList, Queue).

tryPutData(ServerData, Data,Pid, Ref) ->
                       case find(ServerData,Data) of
                                      not_found ->
                                      io:format("~w added ~w to Server~n",[Pid,Data]),
                            [{Data,Pid,Ref}|ServerData];
                            found ->
                                   io:format("~w failed to add ~w to Server~n",[Pid, Data]),
                            ServerData
                        end.
 tryTakeData(ServerData, Data, Pid, Ref) ->
			case find(ServerData,Data) of
			found ->
			io:format("~w extracted ~w from Server~n", [Pid, Data]),
			delete(ServerData,Data);
			not_found ->
			io:format("~w failed to extract ~w from Server",[Pid,Data]),
			ServerData
		        end.
delete([First|Rest],A) ->
                       if
                                First==A ->
                                      Rest;
                                true ->
                                     [First|delete(Rest,A)]
                       end;

delete([],A) ->
             [].

find([First|Rest], A) ->
                   if
                        First == A ->
                              found;
                        true ->
                             find(Rest,A)
                        end;
find([],A) ->
not_found.

reply(Pid, Msg) ->
         Pid ! {self(), Msg}.