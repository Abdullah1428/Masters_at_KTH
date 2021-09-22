% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW2: Routy - link state OSPF protocol
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

% Section 5 -> The Router
% The router should be able to, not only route messages 
% through a network of connected nodes but also, maintain 
% view of the network and construct optimal routing tables. 
% Each routing process will have a state:
% • a symbolic name such as london
% • a counter
% • a history of received messages 
% • a set of interfaces
% • a routing table
% • a map of the network

% When a new router process is created it will sett all its 
% parameters to initial empty values. We will also register 
% the router process under a uniqe name (unique for the 
% erlang machine it is running on, for example r1, r2, etc).

-module(routy).
-export([start/2,stop/1]).

% --------------------------------------------------------
% start/2
start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).
% stop/2
stop(Node) ->
    Node ! stop,
    unregister(Node).
% --------------------------------------------------------

% --------------------------------------------------------
% init/1
init(Name) ->
  Interface = interface:new(),
  Map = map:new(),
  Table = dijkstra:table(Interface, Map),
  History = history:new(Name),
  router(Name, 0, History, Interface, Table, Map).

% --------------------------------------------------------
% router/6
% 5.1 adding interfaces and use of erlang moniter
router(Name, N, History, Interface, Table, Map) ->
  receive
		{add, Node, Pid} ->
	    Ref = erlang:monitor(process,Pid),
	    Interface1 = interface:add(Node, Ref, Pid, Interface),
	    router(Name, N, History, Interface1, Table, Map);
		{remove, Node} ->
	    {ok, Ref} = interface:ref(Node, Interface),
	    erlang:demonitor(Ref),
	    Interface1 = interface:remove(Node, Interface),
	    router(Name, N, History, Interface1, Table, Map);
		{'DOWN', Ref, process, _, _} ->
	    {ok, Down} = interface:name(Ref, Interface),
	    io:format("~w: exit recived from ~w~n", [Name, Down]),
	    Interface1 = interface:remove(Down, Interface),
	    router(Name, N, History, Interface1, Table, Map);
		% 5.2 link-state messages
		% Next we need to implement the link-state message. 
		% When this is sent it is tagged with the counter 
		% value. The counter is then updates so subsequent 
		% messages will have a higher value. When receiving 
		% a links-state message a router must check if this 
		% is an old or new message.
		{links, Node, R, Links} ->
	    case history:update(Node, R, History) of
		    {new, History1} ->
		      interface:broadcast({links, Node, R, Links}, Interface),
		      Map1 = map:update(Node, Links, Map),
		      router(Name, N, History1, Interface, Table, Map1);
		    old ->
					%io:format("~w: this is old mesage from ~w ~n", [Name, Node]),
		      router(Name, N, History, Interface, Table, Map)
	    end;
		% 5.4 Routing a message
		% message has actually arrived to the final destination.
		{route, Name, From, Message} ->
      io:format("(~w) node: received message: (~w) from (~w)~n", [Name, Message,From]),
      router(Name, N, History, Interface, Table, Map);
		% If the message is not ours we should forward it. 
		% If we find a suitable gateway in the routing table 
		% we simply forward the message to the gateway. If 
		% we do not find a routing entry or do not find a 
		% interface of a gateway we have a problem, simply 
		% drop the packet and keep smiling.
		{route, To, From, Message} ->
	    io:format("(~w) node: routing message: (~w) from: (~w)~n", [Name, Message,From]),
	    case dijkstra:route(To, Table) of
		    {ok, Gateway} ->
		      case interface:lookup(Gateway, Interface) of
			      {ok, Pid} ->
			        Pid ! {route, To, From, Message};
			      notfound ->
			        ok
		      end;
		    notfound ->
		      ok
	    end,
	    router(Name, N, History, Interface, Table, Map);
		% We also add a message so that a local user can 
		% initiate the routing of a message without knowing 
		% the name of the local router.
		{send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, History, Interface, Table, Map);
		% updating routing table when map is updated
		% this should be done periodically maybe every 
		% time we receive a link-state message or better 
		% every time the map changes but for now manually
		update ->
	    Table1 = dijkstra:table(interface:list(Interface), Map),
	    router(Name, N, History, Interface, Table1, Map);
		% boradcast a link-state message by router. This 
		% should of course be done periodically or every 
		% time a link is added but we want to experiment 
		% with inconsistent maps so we keep this as a 
		% manual procedure.
	  broadcast ->
	    Message = {links, Name, N, interface:list(Interface)},
	    interface:broadcast(Message, Interface),
	    router(Name, N+1, History, Interface, Table, Map);
		status ->
			io:format("Name: ~w~nN: ~w~nHistory: ~w~nInterfaces: ~w~nTable: ~w~nMap: ~w~n", [Name, N, History, Interface, Table, Map]),
			router(Name, N, History, Interface, Table, Map);
		{status, From} ->
      From ! {status, {Name, N, History, Interface, Table, Map}},
      router(Name, N, History, Interface, Table, Map);
		stop ->
			ok
	end.
  