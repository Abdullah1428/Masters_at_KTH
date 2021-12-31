% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW3: Chordy - a distributed hash table
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

-module(node3).

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
  node(Id, Predecessor, Successor, Store, Next).

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

node(Id, Predecessor, Successor, Store, Next) ->
  % io:format("Id : ~w , Predecessor : ~w , Successor : ~w ~n",[Id,Predecessor,Successor]),
  receive
    % a peer needs to know our key
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store, Next);
    % a new node informs us of its existence
    {notify, New} ->
      {Pred, NewStore} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, NewStore, Next);
    % a predecessor needs to know our predecessor
    {request, Peer} ->
      request(Peer, Predecessor, Successor),
      node(Id, Predecessor, Successor, Store, Next);
    % our successor informs us about its predecessor
    {status, Pred, Nx} ->
      {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
      node(Id, Predecessor, Succ, Store, Nxt);
    stabilize ->
	    stabilize(Successor),
	    node(Id, Predecessor, Successor, Store, Next);
    % adding probe
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store, Next);
    {probe, Id, Nodes, T} -> % Id, [Id]
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store, Next);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store, Next);
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added, Next);
    % The Qref parameters will be used to tag the return message to the Client. 
    % This allows the client to identify the reply message and makes it easier 
    % to implement the client.
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store, Next);
    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged, Next);
    state ->
      io:format("Id: ~w, Predecessor: ~w, Successor: ~w, Store: ~w~n", [Id, Predecessor, Successor, Store]),
      node(Id, Predecessor, Successor, Store, Next);
    {'DOWN', Ref, process, _, _} ->
      {Pred, Succ, Nxt}  = down(Ref, Predecessor, Successor, Next),
      node(Id, Pred, Succ, Store, Nxt);
    stop ->
      stop;
    WeirdMessage ->
      io:format("Weird message: ~w~n", [WeirdMessage]),
      node(Id, Predecessor, Successor, Store, Next)
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

notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      {{Nkey, monitor(Npid), Npid}, Keep};
    {Pkey, Pref, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
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
    Client ! {Qref, ok},
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

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.

monitor(Pid) ->
  erlang:monitor(process, Pid).

drop(nil) ->
  ok;

drop(Pid) ->
  erlang:demonitor(Pid, [flush]).

down(Ref, {_, Ref, _}, Successor, Next) ->
  {nil, Successor, Next};

down(Ref, Predecessor, {_, Ref, _}, {Nkey, _, Npid}) ->
  self() ! stabilize,
  {Predecessor, {Nkey, monitor(Npid), Npid}, nil}.