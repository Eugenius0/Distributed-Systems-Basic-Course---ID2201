-module(node4).
-export([start/1, start/2]).

-define(STABILIZE, 100).
-define(TIMEOUT, 1000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    Replica = storage:create(),
    node(Id, Predecessor, Successor, storage:create(), Replica, nil).

connect(Id, nil) ->
    {ok, {Id, nil, self()}};
connect(_Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            Sref = monitor(Peer),
            {ok, {Skey, Sref, Peer}}
    after ?TIMEOUT ->
        io:format("Timeout: no response!~n")
    end.

schedule_stabilize() ->
    timer:send_interval(?STABILIZE, self(), stabilize).

node(Id, Predecessor, Successor, Store, Replica, Next) ->
    receive
        %% A peer needs to know our key
	{key, Qref, Peer} ->
	    Peer ! {Qref, Id},
	    node(Id, Predecessor, Successor, Store, Replica, Next);
	%% A new node informs us of its existence
	{notify, New} ->
	    {Pred, NewStore} = notify(New, Id, Predecessor, Store),
	    node(Id, Pred, Successor, NewStore, Replica, Next);
	%% A predecessor needs to know our predecessor
	{request, Peer} ->
	    request(Peer, Predecessor, Successor),
	    node(Id, Predecessor, Successor, Store, Replica, Next);
	%% Our successor informs us about its predecessor
	{status, Pred, Nx} ->
	    {Succ, Nxt} = stabilize(Pred, Id, Successor, Nx),
	    node(Id, Predecessor, Succ, Store, Replica, Nxt);
	stabilize ->
	    stabilize(Successor),
	    node(Id, Predecessor, Successor, Store, Replica, Next);
	{add, Key, Value, Qref, Client} ->
	    Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
	    node(Id, Predecessor, Successor, Added, Replica, Next);
	{lookup, Key, Qref, Client} ->
	    lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
	    node(Id, Predecessor, Successor, Store, Replica, Next);
	{handover, Elements} ->
	    Merged = storage:merge(Elements, Store),
	    node(Id, Predecessor, Successor, Merged, Replica, Next);
	probe ->
	    create_probe(Id, Successor),
	    node(Id, Predecessor, Successor, Store, Replica, Next);
	{probe, Id, Nodes, T} ->
	    remove_probe(T, Nodes),
	    node(Id, Predecessor, Successor, Store, Replica, Next);
	{probe, Ref, Nodes, T} ->
	    forward_probe(Ref, T, Nodes, Id, Successor),
	    node(Id, Predecessor, Successor, Store, Replica, Next);
	state ->
	    io:format("ID: ~w~n", [Id]),
	    io:format("Predecessor: ~p, Successor: ~p~n", [Predecessor, Successor]),
	    io:format("Store: ~p~n", [Store]),
	    node(Id, Predecessor, Successor, Store, Replica, Next);
    {'DOWN', Ref, process, _, _} ->
        {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
        node(Id, Pred, Succ, Store, Replica, Nxt);
    {add, Key, Value, Qref, Client} ->
        Replicated = replicate(Key, Value, Qref, Client, Successor),
        Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
        MergedReplica = storage:merge_replica(Replicated, Replica),
        MergedReplicaStore = storage:merge_replica(MergedReplica, Store),
        node(Id, Predecessor, Successor, MergedReplicaStore, MergedReplica, Next)
    end.

replicate(Key, Value, Qref, Client, {Skey, Sref, Spid}) ->
    Sref ! {replicate, Key, Value, Qref, Client},
    ok.

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
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
        {_,_, Spid} = Successor,
	    Spid ! {lookup, Key, Qref, Client}
    end.

create_probe(Id, {_, _, Spid}) ->
    Spid ! {probe, Id, [Id], erlang:now()}.

forward_probe(Ref, T, Nodes, Id, {_, _, Spid}) ->
    Spid ! {probe, Ref, [Id | Nodes], T}.

remove_probe(T, Nodes) ->
    Diff = timer:now_diff(erlang:now(), T),
    io:format("Route: ~p~n", [lists:reverse(Nodes)]),
    io:format("Trip time: ~w micro~n", [Diff]).

request(Peer, Predecessor, {Skey,Sref,Spid}) ->
    case Predecessor of
	nil ->
	    Peer ! {status, nil, {Skey, Spid}};
    {Pkey, _, Ppid} ->
      Peer ! {status, {Pkey, Ppid}, {Skey,Spid}}
    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
	nil ->
	    Keep = handover(Id, Store, Nkey, Npid),
        Nref = monitor(Npid),
	    {{Nkey, Nref, Npid}, Keep};
	{Pkey, Pref, _} ->
	    case key:between(Nkey, Pkey, Id) of
		true ->
		    Keep = handover(Id, Store, Nkey, Npid),
		    Nref = monitor(Npid),
            drop(Pref),
            {{Nkey, Nref, Npid}, Keep}; 
		false ->
		    {Predecessor, Store}
	    end
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.

stabilize({_, _, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Id, Successor, Next) ->
    {Skey, Sref, Spid} = Successor,
    case Pred of
	nil ->
	    %% Inform the node of our existence
	    Spid ! {notify, {Id, self()}},
	    {Successor, Next};
	{Id, _} ->
	    %% It is pointing back to us, do nothing
	    {Successor, Next};
	{Skey, Spid} ->
	    %% It is pointing to itself, notify the node of our existence
	    Spid ! {notify, {Id, self()}},
	    {Successor, Next};
	{Xkey, Xpid} ->
	    case key:between(Xkey, Id, Skey) of
		true ->
		    % If a node's predecessor is between the node and the successor then the Pred is the new Successor of our node
		    Xpid ! {request, self()},
            Xref = monitor(Xpid),
      		    drop(Sref),
      		    {{Xkey, Xref, Xpid}, {Skey, Spid}};
		false ->
		    Spid ! {notify, {Id, self()}},
		    {Successor, Next}
	    end
    end.

% monitor a node
monitor(Pid) ->
  erlang:monitor(process, Pid).

% demonitor a node
drop(nil) ->
  ok;
drop(Ref) ->
  erlang:demonitor(Ref, [flush]).

% predecessor has died
down(Ref, {_, Ref, _}, Successor, Next) ->
  {nil, Successor, Next};

% successor has died
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
  % monitor the new successor
io:format("Successor of ~w died~n", [Ref]),
  Nref = monitor(Npid),
  % to repair the ring
  Npid ! stabilize,
  {Predecessor, {Nkey, Nref, Npid}, nil}. % use Next as new successor


