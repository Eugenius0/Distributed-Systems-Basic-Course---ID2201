-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

% initializes an empty map (empty list)
new() ->
    [].

% used when map is initially empty
update(Node, Links, []) ->
    [{Node, Links}];

update(Node, Links, Map) ->
    % remove any old entry for the Node to ensure there are no duplicates
    NewMap = lists:keydelete(Node, 1, Map),
    
    % add the new directional links
    UpdatedMap = [{Node, Links} | NewMap],
    
    UpdatedMap.


 % returns the list of nodes directly reachable from a given Node in the map
reachable(Node, Map) ->
    % search for node in the Map
    case lists:keyfind(Node, 1, Map) of 
        {Node, ReachableNodes} -> ReachableNodes;
        false -> []
    end.

% returns all nodes in the Map, also nodes without outgoing links
all_nodes(Map) ->
    AllNodes = lists:flatten([[From | To] || {From, To} <- Map]),
    lists:usort(AllNodes). % removes duplicates and sorts the result



