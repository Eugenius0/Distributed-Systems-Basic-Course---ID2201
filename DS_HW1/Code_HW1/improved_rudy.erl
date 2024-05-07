-module(improved_rudy).
-export([init/1, start/1, stop/0]).
-import(http, [parse_request/1, ok/1]).

-define(IDLE, idle).

-define(TIMEOUT, 5000).

start(Port) ->
    register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
    exit(whereis(rudy), "time to die").

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler_pool(Listen, 5, []),
            gen_tcp:close(Listen),
            ok;
        {error, Error} ->
            io:format("rudy: init error: ~w~n", [Error])
    end.

handler_pool(_, 0, Handlers) ->
    {ok, Handlers};
handler_pool(Listen, N, Handlers) ->
    case gen_tcp:accept(Listen, ?TIMEOUT) of
        {ok, Client} ->
            NewHandlers = spawn_handler(Client, Handlers),
            handler_pool(Listen, N - 1, NewHandlers);
        {error, _} ->
            handler_pool(Listen, N, Handlers)
    end.

spawn_handler(Client, Handlers) ->
    case find_idle_handler(Handlers) of
        {ok, HandlerPid} ->
            HandlerPid ! {self(), Client},
            Handlers;
        {error, _} ->
            {ok, spawn(fun() -> handler_loop() end, [Client | Handlers])}
    end.

find_idle_handler([]) ->
    {error, ?IDLE};
find_idle_handler([HandlerPid | Handlers]) ->
    case erlang:process_info(HandlerPid, status) of
        {status, running} ->
            find_idle_handler(Handlers);
        _ ->
            {ok, HandlerPid}
    end.

handler_loop() ->
    receive
        {Client, {ok, Client}} ->
            Recv = gen_tcp:recv(Client, 0),
            case Recv of
                {ok, Str} ->
                    {Request, Headers, Body} = parse_request(Str),
                    Response = reply({Request, Headers, Body}),
                    gen_tcp:send(Client, Response);
                {error, Error} ->
                    io:format("rudy: request error: ~w~n", [Error])
            end,
            gen_tcp:close(Client),
            handler_loop();
        {_, _} ->
            handler_loop()
    after ?TIMEOUT ->
            erlang:exit(normal)
    end.

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    ok("Hello World!").
