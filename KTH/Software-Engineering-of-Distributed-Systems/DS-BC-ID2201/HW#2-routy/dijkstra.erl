% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW2: Routy - link state OSPF protocol
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

% Section 2 -> Dijkstra
% Dijkstra module will compute a routing table. The table is 
% represented by a list with one entry per node where the entry
% describes which gateway, city, should be used to reach the node.
% The input to the algorithm is:
% • a map
% • a list of gateways to which we have direct access.

% An example of a routing table is:
% [{berlin,madrid},{rome,paris},{madrid,madrid},{paris,paris}]

% What does a sorted list looks like -> [{Node,lengthToReach,Gateway},{...}].
% This list is sorted on the basis of length

-module(dijkstra).
-export([entry/2,replace/4,update/4,iterate/3,table/2, route/2]).

% --------------------------------------------------------
% 2.1 -> A Sorted List
% --------------------------------------------------------
% entry/2 method -> returns the length of the shortest path to 
% the node or 0 if the node is not found.
entry(Node,Sorted) ->
  case lists:keyfind(Node,1,Sorted) of
    % structure will be like this -> [{Node,lengthToReach,Gateway},{...}].
    {_Node,LengthToReach,_Gateway} ->
      LengthToReach;
    false ->
      0
  end.

% --------------------------------------------------------
% replace/4 -> replaces the entry for Node in Sorted with a new 
% entry having a new length N and Gateway. The resulting list 
% should of course be sorted.
% Remember to replace only if entry for node exists and the 
% resultant should be sorted
replace(Node,N,Gateway,Sorted) ->
  case entry(Node,Sorted) of
    0 ->
      Sorted;
    _LengthToReach ->
      Replace = {Node,N,Gateway},
      UnSortedList = lists:keyreplace(Node,1,Sorted,Replace),
      % we get unsorted list, we have to sort it now with respect to length
      % keysort(N, TupleList1) -> TupleList2 -> Returns a list containing the 
      % sorted elements of list TupleList1. Sorting is performed on the Nth 
      % element of the tuples. The sort is stable.
      %UnSortedList
      lists:keysort(2,UnSortedList)
  end.
      
% --------------------------------------------------------
% update/4 -> update the list Sorted given the information 
% that Node can be reached in N hops using Gateway. If no 
% entry is found then no new entry is added. Only if we 
% have a better (shorter) path should we replace the existing 
% entry.
update(Node,N,Gateway,Sorted) ->
  case N < entry(Node,Sorted) of
    true ->
      replace(Node,N,Gateway,Sorted);
    false ->
      Sorted
  end.
% --------------------------------------------------------
% 2.1 ended
% --------------------------------------------------------

% --------------------------------------------------------
% 2.2 -> The Iteration (The heart of algorithm)
% --------------------------------------------------------

% --------------------------------------------------------
% iterate/3
% iterate(Sorted,Map,Table) -> iterate([{paris, 0, paris}, {berlin, inf, unknown}],[{paris, [berlin]}], []).
% To implement this we will follow cases given in document
% Case No 1 -> If there are no more entries in the sorted list 
% then we are done and the given routing table is complete,
% so we will just return constructed table in this case.
% Case No 2 -> If the first entry is a dummy entry with an 
% infinite path to a city we know that the rest of the sorted 
% list is also of infinite length and the given routing table 
% is complete so we will just return constructed table in this
% case also.
% Case No 3 -> Otherwise, take the first entry in the sorted 
% list, find the nodes in the map reachable from this entry 
% and for each of these nodes update the Sorted list. The entry 
% that you took from the sorted list is added to the routing table.
iterate(Sorted,Map,Table) ->
  %(handling first case)
  case Sorted of
    [] ->
      Table;
  _Entries ->
    [{Node,LengthToReach,Gateway} | RestOfEntries] = Sorted,
    case LengthToReach == inf of
      true ->
        Table;
      false ->
        ReachableList = map:reachable(Node,Map), % => this will give us directly connected cities for the node like [{berlin,[london,paris]}] will return [london,paris]
        %io:format("List : ~p~n", [ReachableList]),
        %io:format("Length : ~p~n", [LengthToReach]),
        NewSortedList = lists:foldl(
          fun(ReachableListCurrentNode,ResOfEntriesAccumulator) ->
            update(ReachableListCurrentNode,LengthToReach+1,Gateway,ResOfEntriesAccumulator)
          end,
          RestOfEntries,
          ReachableList
        ),
        %io:format("Old Sorted List : ~p~n", [Sorted]),
        %io:format("New Sorted List : ~p~n", [NewSortedList]),
        %io:format("New Table : ~p~n", [[{Node,Gateway}|Table]]),
        iterate(NewSortedList,Map,[{Node,Gateway}|Table])
    end
  end.
% --------------------------------------------------------

% --------------------------------------------------------
% table/2 -> construct a routing table given the gateways 
% and a map.
% steps to follow
% 1) List the nodes of the map and construct a initial sorted list. 
% 2) This list should have dummy entries for all nodes with the length 
% set to infinity, inf, and the gateway to unknown. 
% 3) The entries of the gateways should have length zero and gateway 
% set to itself. Note that inf is greater than any integer (try). 
% 4) When you have constructed this list you can call iterate with 
% an empty table
table(Gateways,Map) ->
  ListOfAllNodes = map:all_nodes(Map), % step 1
  Union = lists:usort(ListOfAllNodes ++ Gateways),
  %io:format("List of All Nodes : ~p~n", [ListOfAllNodes]),
  %io:format("Union : ~p~n", [Union]),
  DummyList = lists:map(   % step 2
    fun(Node) ->
      {Node,inf,unknown}
      % this gives sorted list = [{berlin, inf, unknown},{paris, inf, unknown},{london, inf, unknown}, ...]
    end,
    Union
  ),
  %io:format("DummyList : ~p~n", [DummyList]),
  AllNodesList = lists:foldl(  % step 3
    fun(CurrentGatewayNode,AccumulatorNode) ->
      update(CurrentGatewayNode,0,CurrentGatewayNode,AccumulatorNode)
      % this will append gateways as [{paris,0,paris},{...}] to sorted list
    end,
    DummyList,
    Gateways
  ),
  %io:format("All Nodes List : ~p~n", [AllNodesList]),
  iterate(AllNodesList,Map,[]). % step 4
% --------------------------------------------------------

% --------------------------------------------------------
% route/2 -> search the routing table and return the gateway 
% suitable to route messages to a node. If a gateway is found 
% we should return {ok, Gateway} otherwise we return notfound.
route(Node,Table) ->
  case lists:keyfind(Node,1,Table) of
    {_Node,Gateway} ->
      {ok,Gateway};
    false ->
      notfound
  end.
% --------------------------------------------------------