-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

% new empty set of interfaces.
new() -> [].

% add a new interface entry to the set.
add(Name, Ref, Pid, Intf) ->
    [{Name, Ref, Pid}] ++ Intf.

% remove an interface entry by name.
remove(Name, Intf) ->
    lists:keydelete({Name}, 1, Intf).

% lookup an interface by name and return the process identifier.
lookup(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        false -> notfound;
        {_, _, Pid} -> {ok, Pid}
    end.

% find the reference by name.
ref(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        false -> notfound;
        {_, Ref, _} -> {ok, Ref}
    end.

% find the name by reference.
name(Ref, Intf) ->
    case lists:keyfind(Ref, 2, Intf) of
        false -> notfound;
        {Name, _, _} -> {ok, Name}
    end.

list(Intf) ->
    lists:foldl(fun({Name, _Ref, _Pid}, Acc) -> [Name | Acc] end, [], Intf).

broadcast(Message, Intf) ->
    Names = list(Intf),
    lists:map(fun(X) -> send(Message, lookup(X, Intf)) end, Names),
    ok.

send(Msg, {ok, Destination}) ->
    Destination ! Msg.
