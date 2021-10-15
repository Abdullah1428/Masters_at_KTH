% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW4: Groupy - a group membership service
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

% first version -> gms1 = will only handle starting of a single node 
% and the adding of more nodes. Failures will not be handled so some 
% of the states that we need to keep track of is not described. We 
% will then extend this implementation to handle failures.The group 
% process will when started be a slave but might in the future become 
% a leader. The first process that is started will however become a 
% leader directly.

-module(gms1).
-export([start/1,start/2]).

%-------------------------------------------------------------------------------------------
% init for Leader
%-------------------------------------------------------------------------------------------
% Initializing a process that is the first node in a group is simple. The only thing 
% we need to do is to give it an empty list of peers and let it know that its master 
% is the only node in the group. Since it is the only node in the group it will of 
% course be the leader of the group.
start(Id) ->
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Self) end)}.

init(Id, Master) ->
  leader(Id, Master, [], [Master]).
%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
% init for slave
%-------------------------------------------------------------------------------------------
% Starting a node that should join an existing group is only slightly more problematic. 
% We need to send a {join, Master, self()} message to a node in the group and wait for 
% an invitation. The invitation is delivered as a view message containing everything we 
% need to know. The initial state is of course as a slave.
start(Id, Grp) ->
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, [Leader|Slaves], Group} ->
      Master ! {view, Group},
      slave(Id, Master, Leader, Slaves, Group)
  end.
%-------------------------------------------------------------------------------------------


%-------------------------------------------------------------------------------------------
% The leader process keeps following state -> 
% ID (for debugging), 
% Master (pid of application layer), [{master [pid,pidofNode]},{[]}]
% Slaves (an ordered list of pids of all slaves in group), 
% Group (list of all application layer processes in group)  
leader(Id, Master, Slaves, Group) -> 
  receive
    % {mcast, Msg}: a message either from its own master or from a peer node. 
    % A message {msg, Msg} is multicasted to all peers and a 
    % message Msg is sent to the application layer.
    {mcast, Msg} ->
      bcast(Id, {msg, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, Slaves, Group);
    % {join, Wrk, Peer}: a message, from a peer or the master, that is a request from 
    % a node to join the group. The message contains both the process 
    % identifier of the application layer, Wrk, and the process 
    % identifier of its group process.
    {join, Wrk, Peer} -> % this never happens between a leader and slave, its happening only for master and leader
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2), % first time , second one  (self() => process id of group ) [pid->Node,pid]
      Master ! {view, Group2},
      leader(Id, Master, Slaves2, Group2);
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
slave(Id, Master, Leader, Slaves, Group) ->
  receive
    % {mcast, Msg}: a request from its master to multicast a message, 
    % the message is forwarded to the leader.
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, Slaves, Group);
    % {join, Wrk, Peer}: a request from the master to allow a new node 
    % to join the group, the message is forwarded to the leader.
    {join, Wrk, Peer} ->
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, Slaves, Group);
    % {msg, Msg}: a multicasted message from the leader. A message Msg 
    % is sent to the master.
    {msg, Msg} ->
      Master ! Msg,
      slave(Id, Master, Leader, Slaves, Group);
    % {view, Peers, Group}: a multicasted view from the leader. A view 
    % is delivered to the master process.
    {view, [Leader|Slaves2], Group2} ->
      Master ! {view, Group2},
      slave(Id, Master, Leader, Slaves2, Group2);
    stop ->
      ok 
  end.
%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
% We use a function bcast/3 that will send a message to each of the processes in a list.
bcast(_Id,Msg,Nodes) ->
  lists:foreach(fun(Node) -> Node ! Msg end, Nodes).
%-------------------------------------------------------------------------------------------