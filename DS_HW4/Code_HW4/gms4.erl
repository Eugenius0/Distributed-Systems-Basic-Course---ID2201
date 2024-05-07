-module(gms4).
-export([start/1, start/2, init/2, init/3, wait_for_view/2, slave/8, election/7, leader/5, bcast/3, wait_for_ack/2, crash/1]).
-define(timeout, 1000).
-define(arghh, 100).

start(Id) ->
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Self) end)}.

start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Grp, Self) end)}.

init(Id, Master) ->
    {A0, A1, A2} = now(),
    random:seed(A0, A1, A2),
    leader(Id, Master, 1, [], [Master]).

init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    case wait_for_ack(Id, Self) of
        {ok, AckRef} ->
            case wait_for_view(Id, Master) of
                {ok, {view, N, [Leader | Slaves], Group}} ->
                    io:format("gms4: Received view~n"),
                    erlang:monitor(process, Leader),
                    Master ! {view, Group},
                    {ok, slave(Id, Master, Leader, 1, {view, N, [Leader | Slaves], Group}, Slaves, Group, AckRef)};
                {error, Reason} ->
                    io:format("gms4: View error: ~w~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("gms4: Acknowledgment error: ~w~n", [Reason]),
            {error, Reason}
    end.

wait_for_view(Id, Master) ->
    receive
        {view, N, [Leader | Slaves], Group} ->
            io:format("gms4: Received view~n"),
            erlang:monitor(process, Leader),
            Master ! {view, Group},
            {ok, {view, N, [Leader | Slaves], Group}}  % Return a tuple indicating success
        after ?timeout ->
            Master ! {error, "NO reply from the Leader"},
            {error, "NO reply from the Leader"}
    end.

slave(Id, Master, Leader, N, Last, Slaves, Group, AckSender) ->
    io:format("Last: ~w~n", [Last]),
    io:format("N: ~w~n", [N]),
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            wait_for_ack(Id, Leader),
            slave(Id, Master, Leader, N, Last, Slaves, Group, AckSender);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            wait_for_ack(Id, Leader),
            slave(Id, Master, Leader, N, Last, Slaves, Group, AckSender);
        {msg, N2, _} when N2 < N ->
            io:format("N2: ~w~n", [N2]),
            slave(Id, Master, Leader, N, Last, Slaves, Group, AckSender);
        {msg, N, Msg} ->
            Master ! Msg,
            wait_for_ack(Id, AckSender),
            slave(Id, Master, Leader, N + 1, {msg, N, Msg}, Slaves, Group, AckSender);
        {view, N2, [Leader | Slaves2], Group2}  ->
            Master ! {view, Group2},
            wait_for_ack(Id, Master),
            slave(Id, Master, Leader, N2 + 1, {view, N2, [Leader | Slaves2], Group2},
                Slaves2, Group2, AckSender);
        {ack, _Ref} ->
            io:format("gms ~w: Acknowledgment received~n", [Id]),
            slave(Id, Master, Leader, N, Last, Slaves, Group, AckSender);
        {'DOWN', _Ref, process, Leader, Reason} ->
            election(Id, Master, N, Last, Slaves, Group, AckSender),
            io:format("Leader is DEAD with ~w~n", [Reason]);
        stop ->
            ok;
        Error ->
            io:format("gms ~w: slave, Error message ~w~n", [Id, Error])
    end.

election(Id, Master, N, Last, Slaves, Group, AckSender) ->
    Self = self(),
    io:format("Last message: ~w~n", [Last]),
    case Slaves of
        [Self | Rest] ->
            bcast(Id, Last, Rest),
            bcast(Id, {view, N, Slaves, Group}, Rest),
            Master ! {view, Group},
            io:format("The new leader is: ~w~n", [Self]),
            leader(Id, Master, N + 1, Rest, Group);
        [Leader | Rest] ->
            erlang:monitor(process, Leader),
            io:format("The new leader is ~w~n", [Leader]),
            slave(Id, Master, Leader, N, Last, Rest, Group, AckSender)
    end.

leader(Id, Master, N, Slaves, Group) ->
    receive
        {ack, Sender} ->
            Master ! {ack, Sender},
            leader(Id, Master, N, Slaves, Group);
        {mcast, Msg} ->
            bcast(Id, {msg, N, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, N + 1, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N, [self() | Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N + 1, Slaves2, Group2);
        stop ->
            ok;
        Error ->
            io:format("gms ~w: leader: error message: ~w~n", [Id, Error])
    end.

bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, wait_for_ack(Id, Node) end, Nodes).

wait_for_ack(Id, Node) ->
    Ref = make_ref(), % create a unique reference
    Node ! {ack, Ref}, % send a message to the specified node asking for acknowledgment.
    receive
        {ack, Ref} -> {ok, Ref} % if acknowledgment received and the reference matches return {ok, Ref}
    after ?timeout ->
        io:format("~w: No acknowledgment received from ~w~n", [Id, Node]),
        {error, "No acknowledgment received"} % if no acknowledgment received within timeout return error message
    end.

crash(Id) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
        _ ->
            ok
    end.
