% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW3: Chordy - a distributed hash table
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

-module(node2).

-export([start/1,start/2]).
-define(Stabilize, 1000).
-define(Timeout, 10000).

start(Id) ->
  start(Id, nil).

start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  Store = storage:create(),
  node(Id, Predecessor, Successor, Store).

connect(Id, nil) ->
  {ok, {Id, self()}};
connect(_Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} -> % we will get the response as our successor key
      {ok, {Skey, Peer}}
  after ?Timeout ->
    io:format("Time out: no response~n",[])
end.

node(Id, Predecessor, Successor, Store) ->
  % io:format("Id : ~w , Predecessor : ~w , Successor : ~w ~n",[Id,Predecessor,Successor]),
  receive
    % a peer needs to know our key
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store);
    % a new node informs us of its existence
    {notify, New} ->
      {Pred, NewStore} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, NewStore);
    % a predecessor needs to know our predecessor
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor, Store);
    % our successor informs us about its predecessor
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ, Store);
    stabilize ->
	    stabilize(Successor),
	    node(Id, Predecessor, Successor, Store);
    % adding probe
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store);
    {probe, Id, Nodes, T} -> % Id, [Id]
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store);
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added);
    % The Qref parameters will be used to tag the return message to the Client. 
    % This allows the client to identify the reply message and makes it easier 
    % to implement the client.
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store);
    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged);
    state ->
      io:format("Id: ~w, Predecessor: ~w, Successor: ~w, Store: ~w~n", [Id, Predecessor, Successor, Store]),
      node(Id, Predecessor, Successor, Store);
    stop ->
      stop;
    WeirdMessage ->
      io:format("Weird message: ~w~n", [WeirdMessage]),
      node(Id, Predecessor, Successor, Store)
end.

stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    nil -> % it is nil
      Spid ! {notify, {Id, self()}}, % because the structure is {Key, Pid}
	    Successor;
    {Id, _} -> % pointing back to us).)
      Successor;
    {Skey, _} -> %  pointing to itself
      Spid ! {notify, {Id, self()}}, % because the structure is {Key, Pid}
	    Successor;
    {Xkey, Xpid} -> % pointing to another node we need to be careful => Xpid is the new node in between
      case key:between(Xkey, Id, Skey) of % If the key of the predecessor of our successor (Xkey) is between us and our successor
        true -> 
          Xpid ! {notify, {Id, self()}},
          {Xkey, Xpid};
        false -> 
          Spid ! {notify, {Id, self()}},
		      Successor
      end 
  end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

% we are asking our successor if we are still his predecessor
stabilize({_, Spid}) ->
    Spid ! {request, self()}.


request(Peer, Predecessor) ->
  case Predecessor of
    nil -> % I dont have a predecessor
      Peer ! {status, nil};
    {Pkey, Ppid} -> % this my predecessor
      Peer ! {status, {Pkey, Ppid}}
end.


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
      {{Nkey, Npid}, Keep};
    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
          {{Nkey, Npid}, Keep};
        false ->
          {Predecessor, Store}
      end
  end.


create_probe(Id, Successor) ->
  {_Key, Pid} = Successor,
  io:format("ID : ~w~n",[Id]),
  Pid ! {probe, Id, [Id], erlang:monotonic_time()}.

forward_probe(Ref, T, Nodes, Id, Successor) ->
  {_Key, Pid} = Successor,
  io:format("ID : ~w~n",[Id]),
  Pid ! {probe, Ref, Nodes ++ [Id], T}.

remove_probe(Time, Nodes) ->
  T = erlang:monotonic_time(),
  T2 = T - Time,
  io:format("Elapsed Time : ~w , Nodes : ~w~n",[T2,Nodes]).

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  io:format("Id : ~w~n",[Id]),
  case key:between(Key, Pkey, Id)  of
  true ->
    Client ! {Qref, ok},
    storage:add(Key, Value, Store); 
  false ->
    Spid ! {add, Key, Value, Qref, Client},
    Store
  end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      {_, Spid} = Successor,
      Spid ! {lookup, Key, Qref, Client}
  end.

handover(Id, Store, Nkey, Npid) ->
    {Rest, Keep} = storage:split(Id, Nkey, Store),
    io:format("Rest : ~w, Keep : ~w~n",[Rest,Keep]),
    Npid ! {handover, Rest},
    Keep.