% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW2: Routy - link state OSPF protocol
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

% Section 1 -> The Map
% map module represent a list of entries where each entry consist
% of a city with a list of directly connected cities
% [{berlin,[london,paris]}, {...}]

-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

% --------------------------------------------------------
% new/0 method -> returns an empty map
new() ->
  [].

% --------------------------------------------------------
% update/3 method -> updates the Map to reflect that Node has
% directional links to all nodes in the list Links. The old
% entry is removed
update(Node,Links,Map) ->
  % keyfind -> Searches the list of tuples TupleList for a tuple
  % whose Nth element compares equal to Key. Returns Tuple if 
  % such a tuple is found, otherwise false.
  NewLink = {Node,Links},
  case lists:keyfind(Node,1,Map) of  % Key, N, TupleList
    {Node,_} ->
      % keyreplace -> Returns a copy of TupleList1 where the first 
      % occurrence of a T tuple whose Nth element compares equal to
      % Key is replaced with NewTuple, if there is such a tuple T.
      lists:keyreplace(Node,1,Map,NewLink);
    false ->
      % just append it to Map
      [NewLink|Map]
  end.

% --------------------------------------------------------
% reachable/2 method -> returns the list of nodes directly
% reachable from node.
reachable(Node,Map) ->
  case lists:keyfind(Node,1,Map) of
    {_,Links} ->
      Links;
    false ->
      []
  end.

% --------------------------------------------------------
% all_nodes method -> returns a list of all nodes in the map, also 
% the ones without outgoing links. So if berlin is linked 
% to london but london does not have any outgoing links 
% (and thus no entry in the list), london should still be 
% in the returned list.
all_nodes(Map) ->
  List = lists:foldl(
    fun(CurrentMapEntry,Accumulator) ->
      {Node,Links} = CurrentMapEntry, % separating nodes and links from the X
      [Node|Links] ++ Accumulator % appending the accumulator where accumulator is the first node and then moving towards links
    end, 
    [],
    Map
  ),
  % usort(list) -> list2 -> Returns a list2 containing the sorted elements of 
  % list where all except the first element of the elements 
  % comparing equal have been deleted.
  lists:usort(List).
% --------------------------------------------------------