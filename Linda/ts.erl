-module(ts).
-export([new/0,in/2,out/2,tuplespace/2,find/2, delete/2, match/2]).

%Tuplespace
tuplespace(CurrentList,QueueList) ->
  receive
    {From, Ref, Pattern, takeout} ->

      case find(CurrentList, Pattern) of
        {found,Matched} ->
          NewList= element(2,delete(CurrentList,Matched)),
          NewQueueList=QueueList,
          From ! {Matched, Ref};
        {not_found,Matched} ->
          NewQueueList= QueueList ++[{Pattern, From, Ref}],
          NewList=CurrentList;
        true ->
          NewList=CurrentList,
          NewQueueList=QueueList
      end;
    {From, Ref, Pattern, putin} ->
      L=lists:keytake(Pattern, 1, QueueList),
      case L of
        {value, ExtractedElement, NewQueueList} ->
          element(2,ExtractedElement) ! {element(1,ExtractedElement),element(3,ExtractedElement)},
          NewList=CurrentList;
        false ->
          NewList = [Pattern|CurrentList],
          NewQueueList = QueueList
      end,
      From! {Pattern,Ref};
    true ->
      NewList = CurrentList,
      NewQueueList = QueueList
  end,
  tuplespace(NewList,NewQueueList).

delete([First|Rest],A) ->
  case match(A,First) of
    true -> {value, Rest, First};
    false -> [First|element(2,delete(Rest,A))]
  end;
delete([],A) ->
  [].
find([First|Rest], A) ->
  case match(A,First) of
    true ->
      {found, First};
    false ->
      find(Rest,A)
  end;
find([],A) ->
  {not_found,A}.


%Returns the PID of a new tuplespace (the server)
new() ->
  % spawn_link(ts2,tuplespace,[[],[]])
  Pid = spawn_link(fun() -> tuplespace([], []) end),
  % may be unnecessary if we return the PID
  catch(unregister(ts)),
  register(ts, Pid),
  Pid.

% Wants to take out pattern into TS. Should block if the element Pattern
% is not already in TS.
in(TS,Pattern) ->
  Ref = make_ref(),
  TS ! {self(),Ref, Pattern, takeout},
  receive
    {Matched, Ref} ->
      io:format("~w received ~w with reference ~w from ~w~n",[self(),Matched, Ref,TS]),
      Pattern;
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
