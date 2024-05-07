-module(rudy).
-export([init/1, start/1, stop/0]).
-import(http, [parse_request/1, ok/1]).

start(Port) ->
    register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
    exit(whereis(rudy), "time to die").

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen),
            gen_tcp:close(Listen),
            ok;
        {error, Error} ->
            io:format("rudy: init error: ~w~n", [Error])
        end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            spawn(fun() -> request(Client) end),
            handler(Listen),
            ok;
        {error, Error} ->
            io:format("rudy: handler error: ~w~n", [Error])
        end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            {Request, Headers, Body} = parse_request(Str),
            Response = reply({Request, Headers, Body}),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: request error: ~w~n", [Error])
        end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    ok("Hello World!").