% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW3: Chordy - a distributed hash table
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

-module(storage).
-export([create/0,add/3,lookup/2,split/3,splitReplica/2,merge/2]).

create() -> 
  [].

add(Key, Value, Store) ->
  lists:keystore(Key, 1, Store, {Key, Value}).
  

lookup(Key, Store) ->
  lists:keyfind(Key, 1, Store).

% From = old, To = new pred, Store is the store
split(From, To, Store) ->
  % Partitions List into two lists, where the first list contains all elements 
  % for which Pred(Elem) returns true, and the second list contains all elements 
  % for which Pred(Elem) returns false.
  lists:partition(fun({Key, _Value}) -> key:between(Key,From,To) end, Store).

splitReplica(Key, Store) ->
    Sorted = lists:keysort(1, Store),
    lists:splitwith(fun(E) -> E =< Key end, Sorted).
  
merge(Entries, Store) ->
  Store ++ Entries.

% [{3,x},{4,x},{5,x}]