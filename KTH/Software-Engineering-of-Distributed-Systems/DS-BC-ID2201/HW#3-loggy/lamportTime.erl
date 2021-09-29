% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW3: Loggy - a logical time logger
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

% Section 4 -> Lamport Time (the assignment)

-module(lamportTime).
-export([zero/0,inc/2,merge/2,leq/2,clock/1,update/3,safe/2,newQueue/0,updateHoldBackQueue/4,logSafeHoldBackQueue/2]).

% zero/0
% return an initial Lamport value (could it be 0)
zero() ->
  0.

% inc/2
% return the time T incremented by one (you will probably ignore the Name but we will use it later)
inc(_Name,T) ->
  T + 1.

% merge/2
% merge the two Lamport time stamps (i.e. take the maximum value)
merge(Ti,Tj) ->
  erlang:max(Ti,Tj).

% leq/2
% true if Ti is less than or equal to Tj
leq(Ti,Tj) ->
  case Ti =< Tj of
    true ->
      true;
    false ->
      false
  end.

% ------------------------------------------------------
% extending time module for the tricky part

% clock/1
% return a clock that can keep track of the nodes
clock(Nodes) ->
  lists:map(fun(Node) -> {Node, zero()} end,Nodes).

% update/3
% return a clock that has been updated given that we have 
% received a log message from a node at a given time
update(Node,Time,Clock) ->
  lists:keyreplace(Node,1,Clock,{Node,Time}).

% safe/2
% is it safe to log an event that happened at a given time, true or false
% safe (Time,Clock)
% we have been given Time and the Clock. To implement safe we can get the
% least value stored in clock and compare it with Time and if its less it
% means that it is safe to log the event.
safe(Time,Clock) ->
   UpdatedClock = lists:keysort(2,Clock),

   [{_Node,OldTime}|_Rest] = UpdatedClock,

   leq(Time,OldTime). 


%---------------------------------------------------------------------------
% The Tricky Part of Assignment

%---------------------------------------------------------------------------
newQueue() ->
  [].

% updateHoldBackQueue
% here we will add new messages received to the hold back queue
% first step to add {From,Time,Msg} to the queue which is a list
% so we can just append it.
% second step is to sort the updated queue so the older messages
% are at the front
updateHoldBackQueue(From,Time,Msg,HoldBackQueue) ->
  NewEntry = {From,Time,Msg},
  UpdatedQueue = [NewEntry | HoldBackQueue],
  lists:keysort(2,UpdatedQueue). 
%---------------------------------------------------------------------------  

%---------------------------------------------------------------------------
% logSafeHoldBackQueue
% here we will take messages from queue and check queue message time value 
% with clock time value and if it queue time is less we can log that message
% as it is safe else we return the queue back
% Steps
% first step to get the first message in queue
% second step we call safe method from time module and
% pass Time from the queue msg and the Clock. 
% To recall what safe does at time module -> it takes 
% least value in Clock and compares it with Time from 
% queue and if Time is least then it returns true else false
logSafeHoldBackQueue(_,[]) ->
 [];
logSafeHoldBackQueue(Clock,HoldBackQueue) -> 
  %io:format("Hold Back Queue First :: ~w~n",[HoldBackQueue]),
 [{From,Time,Msg} | RestQueue] = HoldBackQueue,

 case safe(Time,Clock) of
   true -> 
     lamportLogger:log(From,Time,Msg),
     logSafeHoldBackQueue(Clock,RestQueue);
   false ->
     %io:format("Hold Back Queue False :: ~w~n",[HoldBackQueue]),
     HoldBackQueue
 end.

% logSafeHoldBackQueue(Clock,HoldBackQueue) ->
%   lists:dropwhile(
%     fun(QueueElement) ->
%       {From,Time,Msg} = QueueElement,
%       %io:format("----~w~w~w~n",[From,Time,Msg]),
%       case safe(Time,Clock) of
%         true ->
%           logicallogger:log(From,Time,Msg),
%           true;
%         false ->
%           false
%       end
%     end,
%     HoldBackQueue
%   ).
%---------------------------------------------------------------------------
