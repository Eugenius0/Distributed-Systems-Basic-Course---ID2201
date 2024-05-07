-module(storage).
-export([create/0, add/3, lookup/2, split/3, merge/2, merge_replica/2]).

% Function to create a new store
create() ->
    [].

% Function to add a key-value pair to the store
add(Key, Value, Store) ->
    [{Key, Value} | Store].

% Function to lookup a key in the store
lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

% Function to split the store based on key range
split(From, To, Store) ->
  lists:partition(fun({Key,Value})-> key:between(Key, From, To) end, Store).

% Function to merge a list of key-value pairs into the store
merge(Entries, Store) ->
    lists:merge(Entries, Store).

% Function to merge the Replica store into the main store
merge_replica(Replica, Store) ->
    lists:merge(Replica, Store).
