-module(worker).
-export([start/5, stop/1, peers/2, init/6, loop/6, select/1, jitter/1]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter, time:zero()) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter, LamportTime) ->
    random:seed(Seed, Seed, Seed),
    receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, LamportTime);
        stop ->
            ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, LamportTime) ->
    Wait = random:uniform(Sleep),
    receive
        {msg, ReceivedTime, Msg} ->
            % update Lamport time to the greater of current time and received time
            UpdatedTime = time:merge(LamportTime, ReceivedTime),
            % increment Lamport time
            NewLamportTime = time:inc(UpdatedTime),
            Log ! {log, Name, NewLamportTime, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, NewLamportTime);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, LamportTime, {error, Error}}
    after Wait ->
        Selected = select(Peers),
        % increment Lamport time before sending
        NewLamportTime = time:inc(LamportTime),
        Time = NewLamportTime,
        Message = {hello, random:uniform(100)},
        Selected ! {msg, Time, Message},
        jitter(Jitter),
        Log ! {log, Name, Time, {sending, Message}},
        loop(Name, Log, Peers, Sleep, Jitter, NewLamportTime)
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
