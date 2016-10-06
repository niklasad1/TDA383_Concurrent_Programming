-module(ts).
-export([new/0,in/2,out/2]).

% The Tuplespace.
% CurrentList contains appended data from clients.
% QueueList contains sought data from blocked clients. It also carry the reference number of the call from client
% and the PID from the client.
% tuplespace reacts on message passing from clients.
% Given a client's request to take out a data, it will search through the
% server's list of appended data in CurrentList and pass it to the client if possible,
% otherwise the request will be stored in the QueueList - including the PID of which
% it was requested from and also the corresponding reference number.
% Given a client's request to put in a data, the server will search through the
% QueueList if there is any candidates to pass it to, otherwise it will be
% appended in CurrentList for later usage.

tuplespace(CurrentList,QueueList) ->
     receive
          {From, Ref, Pattern, takeout} ->
               case find(CurrentList, Pattern) of
	            {found,Matched} ->
		         NewList= delete(CurrentList,Matched),
		         NewQueueList=QueueList,
		         From ! {Matched, Ref};
		    {not_found,_} ->
		         NewQueueList= QueueList ++[{Pattern, From, Ref}],
		         NewList=CurrentList;
                    _ ->
		         NewList=CurrentList,
		         NewQueueList=QueueList,
			 io:format("Something wrong happened",[])
	       end;
           {From, Ref, Pattern, putin} ->
		case find(QueueList, Pattern) of
		     {found, {Matched,StoredFrom, StoredRef}} ->  
			  StoredFrom ! {Pattern, StoredRef},
			   NewList=CurrentList,
			   NewQueueList=delete(QueueList,Matched);
		     {not_found, _} ->
			   NewList = [Pattern|CurrentList],
			   NewQueueList = QueueList;
		     _ ->
		         NewList=CurrentList,
			 NewQueueList = QueueList,
			 io:format("Something wrong happened",[])
                end,
		From! {Pattern,Ref};
	   _ ->
	        NewList = CurrentList,
		NewQueueList = QueueList,
		io:format("Something wrong happened",[])
       end,
tuplespace(NewList,NewQueueList).


% delete
% When passed CurrentList, it will try to remove tuple A from it, otherwise it will return the original list.
% When passed QueueList, it will try to remove the tuple with its first element corresponding
% to the tuple A passed. If not, it will return the original list.
% It works with wildcard any passed.

delete([First={Data,_,_}|Rest],A) ->
       case match(A,Data) of
               true -> Rest;
		false ->[First]++delete(Rest,A)
       end;
delete([First|Rest],A) ->
         case match(A,First) of
                true -> Rest;
		false -> [First]++delete(Rest,A)
		end;
delete([],_) -> [].

% find
% When passed CurrentList, it will try find A in CurrentList. If it does, it returns {found, A}.
% otherwise it will return {not_found, A}.
% When passed QueueList, it will try find a tuple in the list with first element corresponding to A,
% otherwise it will return {not_found,A}.
% It works with wildcard any passed.

find([First={Data,_,_}|Rest], A) ->
                   case match(Data,A) of
                          true ->
                              {found, First};
                          false ->
                             find(Rest,A)
                        end;
find([First|Rest],A) ->
	           case match(A, First) of
		       true ->
		           {found, First};
		       false ->
		            find(Rest,A)
			    end;
find([],A) ->
{not_found,A}.


% new
% Returns the PID of a new tuplespace (the server)

new() ->
%spawn_link(ts,tuplespace,[[],[]])
%io:format("~w tuplespace created~n",[L])

% spawn_link(ts2,tuplespace,[[],[]])
  Pid = spawn_link(fun() -> tuplespace([], []) end),
  % may be unnecessary if we return the PID
  catch(unregister(ts)),
  register(ts, Pid),
  Pid.

% in
% Wants to take out pattern from TS. Should block if the element Pattern
% is not already in TS. It will be blocked until provided with the
% desired data. It can pass the tuple with an element given
% by any. Any basically means, doesn't matter what element
% is in that position.
% If a tuple is not passed in Pattern it will just return incorrect_args.
% A correct, existing tuplespace PID must be passed in TS, otherwise
% it will just return incorrect_args.

in(TS,Pattern) when is_pid(TS), is_tuple(Pattern)->
Ref = make_ref(),
TS ! {self(),Ref, Pattern, takeout},
receive
	{Matched, MsgRef} ->
	          io:format("~w received ~w with reference ~w from ~w~n",[self(),Matched, MsgRef,TS]),
		  Matched;
         _ ->
	      io:format("Error in Pattern or reference received~n",[])
	     end;

in(_,_) ->
  incorrect_args.
  
% out
% Wants to put in pattern in TS. If a tuple is not passed
% it will just return incorrect_args.
% A correct, existing tuplespace PID must be passed in TS,
% otherwise it will just return incorrect_args.

out(TS, Pattern) when is_pid(TS), is_tuple(Pattern) ->
Ref = make_ref(),
TS ! {self(), Ref, Pattern, putin},
receive
       {Pattern, Ref} ->
               io:format("~w successfully gave ~w to ~w with reference:~w~n ",[self(), Pattern, TS, Ref]);
       true ->
       io:format("Some wrong happened :(", [])
end;

out(_,_)  ->
  incorrect_args.

% match
% match function supplied (by lab PM) for wildcard any checks.

match(any,_) -> true;

match(P,Q) when is_tuple(P), is_tuple(Q)
                -> match(tuple_to_list(P),tuple_to_list(Q));


match([P|PS],[L|LS]) -> case match(P,L) of
                              true -> match(PS,LS);
                              false -> false
                         end;


match(P,P) -> true;


match(_,_) -> false.
