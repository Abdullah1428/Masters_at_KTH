% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW3: Chordy - a distributed hash table
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

-module(key).
-export([generate/0,between/3,between2/3]).


generate() ->
  rand:uniform(30000).

between(Key, From, To) ->
  if 
    (From < To) and (Key > From) and (Key =< To) ->
      true;
    (From > To) and ((Key > From) or (Key =< To)) -> 
      true;
    From == To ->
      true;
    true ->
      false
  end.

% [1 ,5 ,8 ,10]
% From = 10, To = 1, Key = 12 so 10 > 1 and (12 > 1) or (12 =< 1)
% [1, 5, 8, 10, 12]

between2(Key, From, To) ->
  case (From < To) and (Key > From) and (Key =< To) of
    true ->
      true;
    false ->
      case (From > To) and ((Key > From) or (Key =< To)) of
        true -> 
          true;
        false ->
          case (From == To) of
            true ->
              true;
            false ->
              false
          end
      end
  end.

