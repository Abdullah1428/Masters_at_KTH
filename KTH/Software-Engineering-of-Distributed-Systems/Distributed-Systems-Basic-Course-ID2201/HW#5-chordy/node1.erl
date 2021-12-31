% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW3: Chordy - a distributed hash table
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

-module(node1).
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
  node(Id, Predecessor, Successor).

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

node(Id, Predecessor, Successor) ->
  % io:format("Id : ~w , Predecessor : ~w , Successor : ~w ~n",[Id,Predecessor,Successor]),
  receive
    % a peer needs to know our key
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor);
    % a new node informs us of its existence
    {notify, New} ->
      Pred = notify(New, Id, Predecessor),
      node(Id, Pred, Successor);
    % a predecessor needs to know our predecessor
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor);
    % our successor informs us about its predecessor
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ);
    stabilize ->
	    stabilize(Successor),
	    node(Id, Predecessor, Successor);
    % adding probe
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor);
    {probe, Id, Nodes, T} -> % Id, [Id]
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor)

    
end.

stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    nil -> % it is nil suggest self as predecessor
      Spid ! {notify, {Id, self()}}, % because the structure is {Key, Pid}
	    Successor;
    {Id, _} -> % pointing back to us so just return successor
      Successor;
    {Skey, _} -> %  pointing to itself so just self as predecessor
      Spid ! {notify, {Id, self()}}, % because the structure is {Key, Pid}
	    Successor;
    {Xkey, Xpid} -> % pointing to another node we need to be careful => Xpid is the new node in between
      case key:between(Xkey, Id, Skey) of % If the key of the predecessor of our successor (Xkey) is between us and our successor
        true -> % adopt it as our successor
          stabilize(Pred,Id,{Xkey, Xpid}); 
        false -> 
          Spid ! {notify, {Id, self()}}, % suggest self to be predecessor of successor (conversation from lecture).
		      Successor
      end 
  end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

% we are asking our successor if we are still his predecessor
stabilize({_, Spid}) ->
    Spid ! {request, self()}.

% check about predecessor
request(Peer, Predecessor) ->
  case Predecessor of
    nil -> % I dont have a predecessor
      Peer ! {status, nil};
    {Pkey, Ppid} -> % this my predecessor
      Peer ! {status, {Pkey, Ppid}}
end.


% we have to check that the Nkey and Npid is our proper predecessor or not
notify({Nkey, Npid}, Id, Predecessor) ->
  case Predecessor of
    nil -> % set as new predecessor because we dont have one yet
      {Nkey, Npid};
    {Pkey,  _} ->
      case key:between(Nkey, Pkey, Id) of 
        true ->
          % notify someone => no because he has already checked
          {Nkey, Npid};
        false -> 
          % he thinks he is but isn't
          Predecessor % How will it know if we have discarded its friendly proposal? -> on next stablization we will inform him about his new successor when he asks
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