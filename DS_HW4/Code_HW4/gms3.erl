-module(gms3).
-export([start/1, start/2]).
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
    receive
        {view, N, [Leader | Slaves], Group} ->
            io:format("gms3: Received view~n"),
            Master ! {view, Group},
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N + 1, {view, N, [Leader | Slaves], Group}, Slaves, Group)
    after ?timeout ->
        Master ! {error, "NO reply from the Leader"}
    end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
    io:format("Last: ~w~n", [Last]),
    io:format("N: ~w~n", [N]),
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, N2, _} when N2 < N ->
            io:format("N2: ~w~n", [N2]),
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, N, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, N + 1, {msg, N, Msg}, Slaves, Group);
        {view, N, [Leader | Slaves2], Group2}  ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, N + 1, {view, N, [Leader | Slaves2], Group2},
                Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, Reason} ->
            election(Id, Master, N, Last, Slaves, Group),
            io:format("Leader is DEAD with ~w~n", [Reason]);
        stop ->
            ok;
        Error ->
            io:format("gms ~w: slave, Error message ~w~n", [Id, Error])
    end.

election(Id, Master, N, Last, Slaves, [_ | Group]) ->
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
            slave(Id, Master, Leader, N, Last, Rest, Group)
    end.

leader(Id, Master, N, Slaves, Group) ->
    receive
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
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
        _ ->
            ok
    end.
