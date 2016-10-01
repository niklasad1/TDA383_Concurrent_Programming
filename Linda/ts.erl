-module(ts).
-export([new/0,in/2,out/2,tuplespace/2, match/2, delete/2, find/2]).

% The tuplespace
% CurrentList contains appended data from clients.
% QueueList contains sought data from blocked clients. It also carry the reference number of the call from client
% and the PID from the client.
%
% tuplespace reacts on message passing from clients. Upon request, it either supply the client
% with the needed data or takes the data and stores it in CurrentList.

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
% When passed CurrentList, it will try to remove tuple A from it.
% When passed QueueList, it will try to remove the tuple with first element corresponding
% to the tuple A passed.
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
% When passed QueueList, it will try find a tuple in the list with first element corresponding to A.
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
% Wants to take out pattern into TS. Should block if the element Pattern
% is not already in TS.
in(TS,Pattern) ->
Ref = make_ref(),
TS ! {self(),Ref, Pattern, takeout},
receive
	{Matched, MsgRef} ->
	          io:format("~w received ~w with reference ~w from ~w~n",[self(),Matched, MsgRef,TS]),
		  Matched;
         _ ->
	      io:format("Error in Pattern or reference received~n",[])
	     end
.

%out
% Wants to put in pattern from TS.

out(TS, Pattern) ->
Ref = make_ref(),
TS ! {self(), Ref, Pattern, putin},
receive
       {Pattern, Ref} ->
               io:format("~w successfully gave ~w to ~w with reference:~w~n ",[self(), Pattern, TS, Ref]);
       true ->
       io:format("Some wrong happened :(", [])
       end.

% match
% match function supplied for wildcard any checks.

match(any,_) -> true;

match(P,Q) when is_tuple(P), is_tuple(Q)
                -> match(tuple_to_list(P),tuple_to_list(Q));


match([P|PS],[L|LS]) -> case match(P,L) of
                              true -> match(PS,LS);
                              false -> false
                         end;


match(P,P) -> true;


match(_,_) -> false.