-module(key).
-export([generate/0, between/3]).

-define(Max, 1000000000).

% Function to generate a random key within the specified range
generate() ->
    random:uniform(?Max).

between(Key, From, To) when From < To ->
    From < Key andalso Key =< To;
between(Key, From, To) when From > To ->
    Key =< To orelse From < Key;
between(_Key, Id, Id) ->
    true.
