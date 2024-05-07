-module(dijkstra).
-export([table/2, route/2, update/4, iterate/3]).

% looks for Node and returns length of the shortest path
entry(Node, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        {Node, Length, _Gateway} -> Length;
        false -> 0 % represents an unreachable node.
    end.

replace(Node, N, Gateway, Sorted) ->
    NewEntry = {Node, N, Gateway},
    NewSorted = update_entry(Node, N, Gateway, Sorted),
    lists:keyreplace(Node, 1, NewSorted, NewEntry),
    sort_list(NewSorted).

update(Node, N, Gateway, Sorted) ->
    case entry(Node, Sorted) of
        0 -> Sorted; % no entry found, do nothing.
        OldLength when N >= OldLength -> Sorted; % existing path is shorter, so do nothing.
        _ -> sort_list(replace(Node, N, Gateway, Sorted)) % there's a shorter path, so replace the entry.
    end.

% helper function to update the entry for Node or insert a new entry
% while maintaining the sorted order.
update_entry(_Node, _N, _Gateway, []) ->
    []; % Base case: Empty list, return an empty list.

update_entry(Node, N, Gateway, [{Node, Length, _} | Rest]) when N < Length ->
    % replace the existing entry with a shorter path.
    [{Node, N, Gateway} | Rest];

update_entry(Node, N, Gateway, [{Node, OldLength, Gateway} | Rest]) when N == OldLength ->
    % keep the existing entry (shortest path).
    [{Node, OldLength, Gateway} | Rest];

update_entry(Node, N, Gateway, [Entry | Rest]) ->
    % entry does not match, continue searching.
    [Entry | update_entry(Node, N, Gateway, Rest)].

% sorts the list based on the length of the path in ascending order.
sort_list(List) ->
    lists:sort(fun({_, Length1, _}, {_, Length2, _}) -> Length1 < Length2 end, List).

% helper function to construct a routing table
iterate([], Map, Table) -> Table;
iterate([{_, inf, _} | _], _Map, Table) -> Table;
iterate([{Node, Length, Gateway} | Rest], Map, Table) ->
    ReachableNodes = proplists:get_value(Node, Map, []),
    UpdatedSorted = lists:foldl(
        fun(ReachableNode, AccSorted) ->
            LengthToReachable = Length + 1, % assuming unit distance for simplicity
            update(ReachableNode, LengthToReachable, Gateway, AccSorted)
        end,
        Rest,
        ReachableNodes
    ),
    iterate(UpdatedSorted, Map, [{Node, Gateway} | Table]).


table(Gateways, Map) ->
    AllNodes = map:all_nodes(Map),
    InitialList = lists:map(
        fun(Node) ->
            case lists:member(Node, Gateways) of
                true ->
                    {Node, 0, Node};
                false ->
                    {Node, inf, unknown}
            end
        end,
        AllNodes
    ),
    SortedList = lists:keysort(2, InitialList),
    iterate(SortedList, Map, []).



route(Node, Table) ->
    case lists:keyfind(Node, 1, Table) of
        false -> notfound;
    {_, Gateway} -> {ok, Gateway}
    end. 

