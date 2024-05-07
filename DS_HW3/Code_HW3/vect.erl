-module(vect).
-export([zero/0, inc/2, merge/2, leq/2, update/3, safe/2, clock/1, start/5, stop/1, peers/2, worker/6, send_message/2, receive_message/2, log_message/5]).

% Initialize a vector clock as an empty list.
zero() -> [].

% Increment the timestamp for a specific process in the vector clock.
inc(Name, Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Value} ->
            NewTime = lists:keyreplace(Name, 1, Time, {Name, Value + 1}),
            NewTime;
        false ->
            [{Name, 1} | Time]
    end.

% Merge two vector clocks.
merge([], Time) -> Time;
merge([{Name, Ti} | Rest], Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} ->
            NewTime = lists:keyreplace(Name, 1, Time, {Name, max(Ti, Tj)}),
            merge(Rest, NewTime);
        false ->
            NewTime = [{Name, Ti} | Time],
            merge(Rest, NewTime)
    end.

% Check if one vector clock is less than or equal to another.
leq([], _) -> true;
leq([{Name, Ti} | Rest], Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} when Ti =< Tj ->
            leq(Rest, Time);
        _ ->
            false
    end.

% Function to create an initial vector clock with zero values for all processes.
clock(_) ->
    zero().

% Function to update the vector clock given a message from a process at a given time.
update(From, Time, Clock) ->
    NewClock = merge(Clock, inc(From, Time)),
    NewClock.

% Check if it's safe to log a message based on vector clocks.
safe(Time, Clock) ->
    SafeEntries = lists:all(fun({Name, Ti}) -> 
        case lists:keyfind(Name, 1, Clock) of
            {Name, Tj} when Ti =< Tj -> true;
            _ -> false
        end
    end, Time),
    SafeEntries.

% Function to start a process with a vector clock.
start(Name, Log, Id, Sleep, Jitter) ->
    Pid = spawn(fun() -> worker(Name, Log, Id, Sleep, Jitter, clock([])) end),
    {Pid, clock([])}.

% Function to stop a process.
stop({Pid, Clock}) ->
    exit(Pid, kill),
    ok.

% Function to set peers for a process.
peers({Pid, Clock}, Peers) ->
    {Pid, Clock, Peers}.

% Worker function (assuming it's a long-running process).
worker(Name, Log, Id, Sleep, Jitter, Clock) ->
    % Your worker implementation here.
    timer:sleep(Sleep + random:uniform(Jitter + 1)),
    {NewClock, Msg} = send_message(Name, Clock),
    log_message(Name, Id, NewClock, sending, Msg),
    timer:sleep(Sleep + random:uniform(Jitter + 1)),
    {NewClock2, ReceivedMsg} = receive_message(Name, NewClock),
    log_message(Name, Id, NewClock2, received, ReceivedMsg),
    worker(Name, Log, Id, Sleep, Jitter, NewClock2).

log_message(Name, Id, Clock, Type, Msg) ->
    case Type of
        sending ->
            loger:log(Name, Clock, {sending, {Id, Msg}});
        received ->
            loger:log(Name, Clock, {received, {Id, Msg}})
    end.

send_message(Name, Clock) ->
    NewClock = inc(Name, Clock),
    Msg = {hello, random:uniform(100)},
    {NewClock, Msg}.

receive_message(Name, Clock) ->
    NewClock = inc(Name, Clock),
    Msg = {hello, random:uniform(100)},
    {NewClock, Msg}.
