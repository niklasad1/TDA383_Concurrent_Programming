-module(ts2).
-export([new/0,in/2,out/2,tuplespace/2, match/2, delete/2, find/2]).

%Tuplespace
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
			  StoredFrom ! {Matched, StoredRef},
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


%Returns the PID of a new tuplespace (the server)
new() ->
spawn_link(ts2,tuplespace,[[],[]])
%io:format("~w tuplespace created~n",[L])


.
% Wants to take out pattern into TS. Should block if the element Pattern
% is not already in TS.
in(TS,Pattern) ->
Ref = make_ref(),
TS ! {self(),Ref, Pattern, takeout},
receive
	{Matched, MsgRef} ->
	          io:format("~w received ~w with reference ~w from ~w~n",[self(),Matched, MsgRef,TS]);
         _ ->
	      io:format("Error in Pattern or reference received~n",[])
	     end
.

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


match(any,_) -> true;

match(P,Q) when is_tuple(P), is_tuple(Q)
                -> match(tuple_to_list(P),tuple_to_list(Q));


match([P|PS],[L|LS]) -> case match(P,L) of
                              true -> match(PS,LS);
                              false -> false
                         end;


match(P,P) -> true;


match(_,_) -> false.