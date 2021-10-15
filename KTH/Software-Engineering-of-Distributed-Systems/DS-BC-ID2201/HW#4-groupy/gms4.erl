% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW4: Groupy - a group membership service
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

% gms3_lostmessages -> extra bonus point task
% handling msgs lost

-module(gms4).
-export([start/1,start/2]).
% macro for arghh
-define(arghh, 100).
-define(timeout, 100).
-define(loss, 20).

%-------------------------------------------------------------------------------------------
% init for Leader
%-------------------------------------------------------------------------------------------
% Initializing a process that is the first node in a group is simple. The only thing 
% we need to do is to give it an empty list of peers and let it know that its master 
% is the only node in the group. Since it is the only node in the group it will of 
% course be the leader of the group.
start(Id) ->
  Rnd = rand:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
  rand:seed(exsss, {Rnd, Rnd, Rnd}),
  MessagesQueue = [],
  leader(Id, Master, 0, [], [Master], MessagesQueue). % 0 for initial sequence number N, the last argument is for initial queue
%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
% init for slave
%-------------------------------------------------------------------------------------------
% Starting a node that should join an existing group is only slightly more problematic. 
% We need to send a {join, Master, self()} message to a node in the group and wait for 
% an invitation. The invitation is delivered as a view message containing everything we 
% need to know. The initial state is of course as a slave.
start(Id, Grp) ->
  Rnd = rand:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
  rand:seed(exsss, {Rnd, Rnd, Rnd}),
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, N, [Leader|Slaves], Group, MessagesQueue} ->
      % failure detectors
      erlang:monitor(process, Leader),
      Master ! {view, Group},
      slave(Id, Master, Leader, N, {view, N, [Leader|Slaves], Group, MessagesQueue}, Slaves, Group)
    after ?timeout ->
      Master ! {error, "no reply from leader"}
  end.
%-------------------------------------------------------------------------------------------


%-------------------------------------------------------------------------------------------
% The leader process keeps following state -> 
% ID (for debugging), 
% Master (pid of application layer), 
% Slaves (an ordered list of pids of all slaves in group), 
% Group (list of all application layer processes in group)  

% gms3 leader -> leader(Id, Master, N, Slaves): the leader procedure is extended with the the 
% argument N, the sequence number of the next message (regular message or view) to be sent.

leader(Id, Master, N, Slaves, Group, MessagesQueue) ->
  receive
    % {mcast, Msg}: a message either from its own master or from a peer node. 
    % A message {msg, Msg} is multicasted to all peers and a 
    % message Msg is sent to the application layer.
    {mcast, Msg} ->
      
      %io:format("leader ~w: received {mcast, ~w} with seq num ~w~n", [Id, Msg, N]),

      %io:format("msg ---- ~w~n",[erlang:append_element({msg, N + 1, Msg}, MessagesQueue)]),

      bcast(Id, erlang:append_element({msg, N + 1, Msg}, MessagesQueue), Slaves),
      % add msg = [Msg] to queue
      NewMessagesQueue = lists:append(MessagesQueue, [Msg]),
      Master ! Msg,
      leader(Id, Master, N + 1, Slaves, Group, NewMessagesQueue);
    % {join, Wrk, Peer}: a message, from a peer or the master, that is a request from 
    % a node to join the group. The message contains both the process 
    % identifier of the application layer, Wrk, and the process 
    % identifier of its group process.
    {join, Wrk, Peer} ->
      %io:format("leader ~w: join from ~w forward to master~n", [Id, Peer]),
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),

      %io:format("view ---- ~w~n",[erlang:append_element({view, N + 1, [self()|Slaves2], Group2}, MessagesQueue)]),

      bcast(Id, erlang:append_element({view, N + 1, [self()|Slaves2], Group2}, MessagesQueue), Slaves2),
      % adding view = [{view, Group2}] to the queue
      NewMessagesQueue = lists:append(MessagesQueue, [{view, Group2}]),
      Master ! {view, Group2},
      leader(Id, Master, N + 1, Slaves2, Group2, NewMessagesQueue);
    stop -> 
      ok
  end.
%-------------------------------------------------------------------------------------------


%-------------------------------------------------------------------------------------------
% The slave process -> A slave has an even simpler job, it will not make any 
% complicated decisions. It is simply forwarding messages from its master to 
% the leader and vice verse. The state of a slave is exactly the same as for 
% the leader with the only exception that the slaves keep explicit track of 
% the leader.

% gms3 slave -> slave(Id, Master, Leader, N, Last, Slaves, Group): the slave 
% procedure is extended with two arguments: N and Last. N is the expected sequence 
% number of the next message and Last is a copy of the last message 
% (a regular message or a view) received from the leader.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
  receive
    % {mcast, Msg}: a request from its master to multicast a message, 
    % the message is forwarded to the leader.
    {mcast, Msg} ->
      %io:format("slave ~w: received {mcast, ~w} with seq num ~w~n", [Id, Msg, N]),
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    % {join, Wrk, Peer}: a request from the master to allow a new node 
    % to join the group, the message is forwarded to the leader.
    {join, Wrk, Peer} ->
      %io:format("slave ~w: join from ~w forward to leader~n", [Id, Peer]),
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    % old message = Num < N discard it
    {msg, Num, _, _} when Num < (N + 1) -> 
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    % {msg, Num, Msg, Queue}: a multicasted message from the leader. A message Msg 
    % is sent to the master.
    % Num == N so we can send the msg
    {msg, Num, Msg, Queue} when Num == (N + 1) ->
      %io:format("slave ~w: msg ~w with seq num ~w~n", [Id, Msg, N]),
      Master ! Msg,
      slave(Id, Master, Leader, Num, {msg, Num, Msg, Queue}, Slaves, Group);
    % Num > N = print the queue first and then send message
    {msg, Num, Msg, Queue} when Num > (N + 1) ->
      %io:format("missed ones --- ~w: msg ~w with seq num ~w~n", [Id, Msg, N]),
      callQueue(Master, Msg, Queue, N + 1, Num - (N + 1)),
      slave(Id, Master, Leader, N, {msg, Num, Msg, []}, Slaves, Group);
    % {view, Peers, Group}: a multicasted view from the leader. A view 
    % is delivered to the master process.
    {view, Num, _, _} when Num < (N + 1) ->
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {view, Num, [Leader|Slaves2], Group2, Queue} when Num == (N + 1) ->
      Master ! {view, Group2},
      slave(Id, Master, Leader, Num, {view, Num, [Leader|Slaves2], Group2, Queue}, Slaves2, Group2);
    {view, Num, [Leader|Slaves2], Group2, Queue} when Num > (N + 1) ->
      %io:format("missed ones --- ~w: view ~w ~w~n", [Id, N, [Leader|Slaves2]]),
      callQueue(Master, {view, Group2}, Queue, N + 1, Num - (N + 1)),
      slave(Id, Master, Leader, N, {view, Num, [Leader|Slaves2], Group2, []}, Slaves, Group);
    % A slave that detects that a leader has died will move to an election state.
    {'DOWN', _Ref, process, Leader, _Reason} ->
      %io:format("slave ~w: leader, went down ~w~n", [Id, Leader]),
      election(Id, Master, N, Last, Slaves, Group);
    stop ->
      ok 
  end.
%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
% We use a function bcast/3 that will send a message to each of the processes in a list.
bcast(ID,Msg,Nodes) ->
  lists:foreach(fun(Node) -> Node ! Msg, crash(ID) end, Nodes).
%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
% The election state

% gm3 election -> election(Id, Master, N, Last, Slaves, Group): the election procedure is extended 
% with the same two arguments.
%-------------------------------------------------------------------------------------------
election(Id, Master, N, Last, Slaves, [_|Group]) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      io:format("Leader : ~w~n",[Id]),
      % The crucial part is then in the election procedure where the elected leader will forward 
      % the last received message to all peers in the group. Hopefully this will be enough to keep 
      % slaves synchronized.
      bcast(Id, Last, Rest),
      bcast(Id, {view, N, Slaves, Group, erlang:element(size(Last), Last)}, Rest),
      Master ! {view, Group},
      leader(Id, Master, N, Rest, Group, erlang:element(size(Last), Last));
    [Leader|Rest] ->
      io:format("Slave : ~w~n",[Id]),
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N, Last, Rest, Group)
  end.
%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
% Missing messages -> introduce a random crash
%-------------------------------------------------------------------------------------------
crash(Id) ->
  case rand:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
    _ -> 
      ok
  end.
%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
% Bonus Point Part
% Message Loss Method -> a random loss method
% one in 20 messages will be lost
%-------------------------------------------------------------------------------------------
% lossMessage() ->
%   case rand:uniform(?loss) of
%     ?loss ->
%       io:format("--message loss--~n"),
%       true;
%     _ -> 
%       false
%   end.
%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
% callQueue
%-------------------------------------------------------------------------------------------
callQueue(Node,Message,List,Start,Length) ->
  Msgs = lists:sublist(List,Start,Length),
  lists:foreach(fun(Msg) -> Node ! Msg end, Msgs),
  Node ! Message.

%-------------------------------------------------------------------------------------------


%   How to do extra part
%
%   Num = Current Number received, N is the max value at the moment.
%   If Num < N  -> discard the message 
%   If Num == N  -> print the message as its received
%   If Num > N -> save the message to queue as its missed. = simulatedn loss message (leader increments)
%   (Num = 25, N = 20) => need to separate array from 25 to 20 to get the messages value 
%
%   we will use sublist => lists:sublist(List1,start,len) -> we have to provide List, the starting position, and len.
%   lists:sublist([1,2,3,4,5], 5, 2). -> output -> [5]
%
%   MessagesNotPrinted = lists:sublist(Queue,N,Num-N).
%   print this messages
%
%   https://stackoverflow.com/questions/12534898/splitting-a-list-in-equal-sized-chunks-in-erlang
% 