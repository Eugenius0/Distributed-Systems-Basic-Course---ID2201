-module(time).
-export([zero/0, inc/1, merge/2, leq/2, clock/1, update/3, safe/2]).

% return the initial Lamport timestamp (0).
zero() -> 0.

% increment a Lamport timestamp by one.
inc(T) -> T + 1.

% merge two Lamport timestamps by taking the maximum value.
merge(Ti, Tj) ->
    if Ti >= Tj -> Ti; true -> Tj end.

% check if one Lamport timestamp is less than or equal to another.
leq(Ti, Tj) when Ti =< Tj -> true;
leq(_, _) -> false.

% function to create a clock to keep track of nodes.
clock(Nodes) ->
    lists:foldl(fun(Node, Acc) -> [{Node, zero()} | Acc] end, [], Nodes).

% function to update the clock given a log message from a node at a given time.
update(Node, Time, Clock) ->
    NewClock = update_clock(Node, Time, Clock),
    NewClock.

% Helper function to update the clock for a specific node.
update_clock(Node, Time, Clock) ->
    case lists:keysearch(Node, 1, Clock) of
        {value, {Node, OldTime}} ->
            NewTime = merge(OldTime, Time),
            NewClock = lists:keyreplace(Node, 1, Clock, {Node, NewTime}),
            NewClock;
        false ->
            Clock  % Node not found in clock, return as is
    end.

% function to check if it's safe to log an event at a given time.
safe(Time, Clock) ->
    lists:all(fun({_, NodeTime}) -> leq(NodeTime, Time) end, Clock).
