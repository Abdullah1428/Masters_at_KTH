% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW4: Groupy - a group membership service
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

% gms2 -> Handling failure = We will build up our fault tolerance gradually. 
% First we will make sure that we detect crashes, then to 
% make sure that a new leader is elected an then make sure 
% that the layer preserves the properties of the atomic 
% multicast. Keep gms1 as a reference and call the adapted 
% module gms2.

-module(gms2).
-export([start/1,start/2]).
% macro for arghh
-define(arghh, 200). % once in 100 request
-define(timeout, 300).
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
  Rnd = rand:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
  rand:seed(exsss, {Rnd, Rnd, Rnd}),
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, [Leader|Slaves], Group} ->
      % failure detectors
      erlang:monitor(process, Leader),
      Master ! {view, Group},
      slave(Id, Master, Leader, Slaves, Group)
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
leader(Id, Master, Slaves, Group) ->
  %io:format("Id: ~w Master: ~w Slaves: ~w Group: ~w self(): ~w~n", [Id, Master, Slaves, Group, self()]),
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
    {join, Wrk, Peer} ->
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
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
    % A slave that detects that a leader has died will move to an election state.
    {'DOWN', _Ref, process, Leader, _Reason} ->
      election(Id, Master, Slaves, Group);
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
%-------------------------------------------------------------------------------------------
election(Id, Master, Slaves, [_|Group]) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      %io:format("Leader : ~w~n",[Id]),
      bcast(Id, {view, Slaves, Group}, Rest),
      Master ! {view, Group},
      leader(Id, Master, Rest, Group);
    [Leader|Rest] ->
      %io:format("Slave : ~w~n",[Id]),
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, Rest, Group)
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