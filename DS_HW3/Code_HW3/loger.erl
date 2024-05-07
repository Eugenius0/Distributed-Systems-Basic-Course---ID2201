-module(loger).
-export([start/1, stop/1, log/3, loop/2, add_message_to_queue/4, check_safe_messages/2, print_messages/2]).

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

init(Nodes) ->
    Clock = time:clock(Nodes),  % Initialize the clock for tracking timestamps.
    HoldbackQueue = [],        % Initialize an empty holdback queue.
    loop(Clock, HoldbackQueue).

loop(Clock, HoldbackQueue) ->
    receive
        {log, From, Time, Msg} ->
            % Update clock, add message to the holdback queue, and process.
            NewClock = time:update(From, Time, Clock),
            NewQueue = add_message_to_queue(Msg, From, Time, HoldbackQueue),
            io:format("Queue Size: ~w~n", [length(NewQueue)]),
            loop(NewClock, NewQueue);
        stop ->
            print_messages(Clock, HoldbackQueue),  % Print any remaining messages on stop.
            ok
    end.

add_message_to_queue(Msg, From, Time, HoldbackQueue) ->
    % Add the new message to the holdback queue.
    [{Msg, From, Time} | HoldbackQueue].

check_safe_messages(_Clock, []) ->
    {[], []};  % If the queue is empty, nothing to process.
check_safe_messages(Clock, Messages) ->
    {Ready, UpdatedQueue} = check_safe_messages(Clock, Messages, [], []),
    {Ready, UpdatedQueue}.

check_safe_messages(_Clock, [], ReadyAcc, UpdatedQueueAcc) ->
    {lists:reverse(ReadyAcc), lists:reverse(UpdatedQueueAcc)};  % Reverse lists for correct order.
check_safe_messages(Clock, [{Msg, From, Time} | Rest], ReadyAcc, UpdatedQueueAcc) ->
    case time:safe(Time, Clock) of
        true ->
            % Message is safe to print, add it to the ready list.
            check_safe_messages(Clock, Rest, [{Msg, From, Time} | ReadyAcc], UpdatedQueueAcc);
        false ->
            % Message is not yet safe to print, keep it in the queue.
            check_safe_messages(Clock, Rest, ReadyAcc, [{Msg, From, Time} | UpdatedQueueAcc])
    end.

print_messages(_Clock, []) ->
    ok;
print_messages(Clock, [{Msg, From, Time} | Rest]) ->
    % Print the message and recursively process the rest of the ready messages.
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]),
    print_messages(Clock, Rest).
