% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW4: Groupy - a group membership service
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

% gms3 -> Reliable multicast = replace the basic multicaster 
% with a reliable multicaster. A process that would forward all 
% messages before delivering them to the higher layer

-module(gms3).
-export([start/1,start/2]).
% macro for arghh
-define(arghh, 100).
-define(timeout, 100).

% Each Node has a Master as application process

%-------------------------------------------------------------------------------------------
% init for Leader
%-------------------------------------------------------------------------------------------
% Initializing a process that is the first node in a group is simple. The only thing 
% we need to do is to give it an empty list of peers and let it know that its master 
% is the only node in the group. Since it is the only node in the group it will of 
% course be the leader of the group.
start(Id) ->
  Rnd = rand:uniform(1000),
  Self = self(), % the master is created here which is also a leader as first node
  {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
  rand:seed(exsss, {Rnd, Rnd, Rnd}),
  leader(Id, Master, 0, [], [Master]). % 0 for initial sequence number N
  % [] = empty list of peers, [Master] = the only group at the start, as there is only one Node so master is leader for now.
%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
% init for slave
%-------------------------------------------------------------------------------------------
% Starting a node that should join an existing group is only slightly more problematic. 
% We need to send a {join, Master, self()} message to a node in the group and wait for 
% an invitation. The invitation is delivered as a view message containing everything we 
% need to know. The initial state is of course as a slave.
%-------------------------------------------------------------------------------------------
start(Id, Grp) -> % [Master] Grp is exisiting group made from first node.
  Rnd = rand:uniform(1000),
  Self = self(), % master for slave nodes % Master Pid for Slave
  {ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
  rand:seed(exsss, {Rnd, Rnd, Rnd}),
  Self = self(), % slave pid
  Grp ! {join, Master, Self}, % calls the worker function call = Someone wants to join our group
  receive
    {view, N, [Leader|Slaves], Group} -> % this will be received from line 95 call
      % failure detectors
      erlang:monitor(process, Leader),
      Master ! {view, Group}, % update new view
      slave(Id, Master, Leader, N, {view, N, [Leader|Slaves], Group}, Slaves, Group)
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

leader(Id, Master, N, Slaves, Group) ->
  receive
    % {mcast, Msg}: a message either from its own master or from a peer node. 
    % A message {msg, Msg} is multicasted to all peers and a 
    % message Msg is sent to the application layer.
    {mcast, Msg} ->
      %io:format("leader ~w: received {mcast, ~w} with seq num ~w~n", [Id, Msg, N]),
      bcast(Id, {msg, N + 1, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, N + 1, Slaves, Group);
    % {join, Wrk, Peer}: a message, from a peer or the master, that is a request from 
    % a node to join the group. The message contains both the process 
    % identifier of the application layer, Wrk, and the process 
    % identifier of its group process.
    {join, Wrk, Peer} ->
      %io:format("leader ~w: join from ~w forward to master~n", [Id, Peer]),
      Slaves2 = lists:append(Slaves, [Peer]), % Peer is slave who wants to join the slaves list
      Group2 = lists:append(Group, [Wrk]), % Wrk is the application layer group process who wants to join the group [MastrePid,  SlaveMasterPid]
      % io:format("new group :: ~w~n",[{view, N, [self()|Slaves2], Group2}]),
      bcast(Id, {view, N + 1, [self()|Slaves2], Group2}, Slaves2), % broadcast the new view = here self is the leader pid
      Master ! {view, Group2}, % Master displays the updated view 
      leader(Id, Master, N+1, Slaves2, Group2); 
    stop ->
      io:format("Leader: ~w (~w) stops~n",[Id,self()]),
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
    % {msg, Msg}: a multicasted message from the leader. A message Msg 
    % is sent to the master.
    {msg, Num, _} when Num < N + 1 -> % discard it
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, Num, Msg} when Num == N + 1 ->
      %io:format("slave ~w: msg ~w with seq num ~w~n", [Id, Msg, N]),
      Master ! Msg, % Master for the slave
      slave(Id, Master, Leader, Num, {msg, Num, Msg}, Slaves, Group);
    % {view, Peers, Group}: a multicasted view from the leader. A view 
    % is delivered to the master process.
    {view, Num, _, _} when Num < N + 1 -> % also discard the view if we have I < N
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {view, Num, [Leader|Slaves2], Group2} when Num == N + 1 ->
      %io:format("slave ~w: view ~w ~w~n", [Id, N, [Leader|Slaves2]]),
      Master ! {view, Group2},
      slave(Id, Master, Leader, Num, {view, Num, [Leader|Slaves2],Group2}, Slaves2, Group2);
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
      %------------------------------------------------------------------------------------------
      % The crucial part is then in the election procedure where the elected leader will forward 
      % the last received message to all peers in the group. Hopefully this will be enough to keep 
      % slaves synchronized.
      %------------------------------------------------------------------------------------------
      bcast(Id, Last, Rest), % broadcast the last message to Rest which is the rest of the slaves
      bcast(Id, {view, N, Slaves, Group}, Rest), % broadcast the updated view to all other slaves
      Master ! {view, Group}, % Master sends the view msg to get updated view of new group as first group is removed. [_|Group]
      leader(Id, Master, N, Rest, Group); % leader function
    [Leader|Rest] ->
      io:format("Slave : ~w~n",[Id]),
      erlang:monitor(process, Leader), % monitor the new leader 
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

%Sure! So far I understand each node consists of a application process using worker and 
%the group process using gms module. The first node to start a group is the leader while 
%joining nodes are slaves