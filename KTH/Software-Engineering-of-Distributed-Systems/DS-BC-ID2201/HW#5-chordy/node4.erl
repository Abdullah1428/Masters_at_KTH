% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW3: Chordy - a distributed hash table
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

% Replication

-module(node4).

-export([start/1,start/2]).
-define(Stabilize, 100).
-define(Timeout, 1000).

start(Id) ->
  start(Id, nil).

start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  Next = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  Store = storage:create(),
  Replica = storage:create(),
  node(Id, Predecessor, Successor, Store, Next, Replica).

connect(Id, nil) ->
  {ok, {Id, nil, self()}};
connect(_Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} -> % we will get the response as our successor key
      {ok, {Skey, monitor(Peer), Peer}}
  after ?Timeout ->
    io:format("Time out: no response~n",[])
end.

node(Id, Predecessor, Successor, Store, Next, Replica) ->
  % io:format("Id : ~w , Predecessor : ~w , Successor : ~w ~n",[Id,Predecessor,Successor]),
  receive
    % a peer needs to know our key
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store, Next, Replica);
    % a new node informs us of its existence
    {notify, New} ->
      {Pred, NewStore} = notify(New, Id, Predecessor, Store, Replica),
      node(Id, Pred, Successor, NewStore, Next, Replica);
    % a predecessor needs to know our predecessor
    {request, Peer} ->
      request(Peer, Predecessor, Successor),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    % our successor informs us about its predecessor
    {status, Pred, Nx} ->
      {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
      node(Id, Predecessor, Succ, Store, Nxt, Replica);
    stabilize ->
	    stabilize(Successor),
	    node(Id, Predecessor, Successor, Store, Next, Replica);
    % adding probe
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    {probe, Id, Nodes, T} -> % Id, [Id]
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added, Next, Replica);
    % The Qref parameters will be used to tag the return message to the Client. 
    % This allows the client to identify the reply message and makes it easier 
    % to implement the client.
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    {handover, Elements, RElements} ->
      Merged = storage:merge(Store, Elements),
      MergedReplica = storage:merge(Replica, RElements),
      Successor ! {replica, Merged},
      node(Id, Predecessor, Successor, Merged, Next, MergedReplica);
    state ->
      io:format("Id: ~w, Predecessor: ~w, Successor: ~w, NextSuccessor: ~w,  Store: ~w, ReplicaStore: ~w~n", [Id, Predecessor, Successor, Store, Next, Replica]),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    {'DOWN', Ref, process, _, _} ->
      {Pred, Succ, Nxt, NewStore}  = down(Ref, Predecessor, Successor, Store, Next, Replica),
      node(Id, Pred, Succ, NewStore, Nxt, storage:create());
    {replicate, Client, Qref, Key, Value} ->
	    NewReplica = storage:add(Key, Value, Replica),
	    Client ! {Qref, ok},
	    node(Id, Predecessor, Successor, Store, Next, NewReplica);
	  {replica, NewReplica} ->
	    node(Id, Predecessor, Successor, Store, Next, NewReplica);
    stop ->
      stop;
    WeirdMessage ->
      io:format("Weird message: ~w~n", [WeirdMessage]),
      node(Id, Predecessor, Successor, Store, Next, Replica)
end.

stabilize(Pred, Nx, Id, Successor) ->
  {Skey, Sref, Spid} = Successor,
  case Pred of
    nil -> % it is nil
      Spid ! {notify, {Id, self()}}, % because the structure is {Key, Pid}
	    {Successor, Nx};
    {Id, _, _} -> % pointing back to us).)
      {Successor, Nx};
    {Skey, _, _} -> %  pointing to itself
      Spid ! {notify, {Id, self()}}, % because the structure is {Key, Pid}
	    {Successor, Nx};
    {Xkey, _, Xpid} -> % pointing to another node we need to be careful => Xpid is the new node in between
      case key:between(Xkey, Id, Skey) of % If the key of the predecessor of our successor (Xkey) is between us and our successor
        true -> % Xkey is our new successor
          Xpid ! {notify, {Id, self()}},
          drop(Sref),
          {{Xkey,  monitor(Xpid), Xpid}, nil}; % successor will be next
        false -> 
          Spid ! {notify, {Id, self()}},
		      {Successor, Nx}
      end 
  end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

% we are asking our successor if we are still his predecessor
stabilize({_, _, Spid}) ->
    Spid ! {request, self()}.


request(Peer, Predecessor, Successor) ->
  Peer ! {status, Predecessor, Successor}.


% notify({Nkey, Npid}, Id, Predecessor) ->
%   case Predecessor of
%     nil -> % set as new predecessor because we dont have one yet
%       {Nkey, Npid};
%     {Pkey,  _} ->
%       case key:between(Nkey, Pkey, Id) of 
%         true ->
%           % notify someone => no because he has already checked
%           {Nkey, Npid};
%         false -> 
%           % he thinks he is but isn't
%           Predecessor % How will it know if we have discarded its friendly proposal? -> on next stablization we will inform him about his new successor when he asks
%       end 
%   end.

notify({Nkey, Npid}, Id, Predecessor, Store, Replica) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Replica, Nkey, Npid),
      {{Nkey, monitor(Npid), Npid}, Keep};
    {Pkey, Pref, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Replica, Nkey, Npid),
          drop(Pref),
          {{Nkey, monitor(Npid), Npid}, Keep};
        false ->
          {Predecessor, Store}
      end
  end.


create_probe(Id, Successor) ->
  {_Key, _Ref, Spid} = Successor,
  io:format("ID : ~w~n",[Id]),
  Spid ! {probe, Id, [Id], erlang:monotonic_time()}.

forward_probe(Ref, T, Nodes, Id, Successor) ->
  {_Key, _Ref, Spid} = Successor,
  io:format("ID : ~w~n",[Id]),
  Spid ! {probe, Ref, lists:append(Nodes, [Id]), T}.

remove_probe(Time, Nodes) ->
  T = erlang:monotonic_time(),
  T2 = T - Time,
  io:format("Elapsed Time : ~w , Nodes : ~w~n",[T2,Nodes]).

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key, Pkey, Id)  of
  true ->
    Spid ! {replicate, Client, Qref, Key, Value},
    storage:add(Key, Value, Store); 
  false ->
    Spid ! {add, Key, Value, Qref, Client},
    Store
  end.

lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      {_, _, Spid} = Successor,
      Spid ! {lookup, Key, Qref, Client}
  end.

handover(Id, Store, Replica, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    {KeepReplica, RestReplicaa} = storage:splitReplica(Nkey, Replica),
    Npid ! {handover, Rest, RestReplicaa},
    {Keep, KeepReplica}.

monitor(Pid) ->
  erlang:monitor(process, Pid).

drop(nil) ->
  ok;

drop(Pid) ->
  erlang:demonitor(Pid, [flush]).

down(Ref, {_, Ref, _}, Successor, Store, Next, Replica) ->
  NewStore = storage:merge(Store, Replica),
  {nil, Successor, Next, NewStore};

down(Ref, Predecessor, {_, Ref, _}, {Nkey, _, Npid}, Store, _Replica) ->
  self() ! stabilize,
  Npid ! {replica, Store},
  {Predecessor, {Nkey, monitor(Npid), Npid}, nil, Store}.