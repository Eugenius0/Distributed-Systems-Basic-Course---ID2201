-module(parallel_test).
-export([bench/4]).
-export([request/2]).

bench(Host, Port, C, N) ->
  Start = erlang:system_time(micro_seconds),
  parallel(C, Host, Port, N, self()),
  collect(C),
  Finish = erlang:system_time(micro_seconds),
  Finish - Start.

parallel(0, _, _, _, _) ->
  ok;
parallel(C, Host, Port, N, Ctrl) ->
  spawn(fun() -> report(N, Host, Port, Ctrl) end),
  parallel(C-1, Host, Port, N, Ctrl).

report(N, Host, Port, Ctrl) ->
  run(N, Host, Port),
  Ctrl ! ok.

collect(0) ->
  ok;
collect(N) ->
  receive
    ok ->
      collect(N-1)
  end.

run(0, _Host, _Port) ->
  ok;
run(N,Host, Port) ->
  request(Host, Port),
  run(N-1, Host, Port).

request(Host, Port) ->
  io:format("requesting ~s:~w~n", [Host, Port]),
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:connect(Host, Port, Opt) of
    {ok, Server} ->
      gen_tcp:send(Server, http:get("foo")),
      Recv = gen_tcp:recv(Server, 0),
      case Recv of
        {ok, _} ->
          ok;
        {error, Error} ->
          io:format("request: error: ~w~n", [Error])
      end,
      io:format("request completed~n"),
      gen_tcp:close(Server);
    {error, Error} ->
      io:format("connect: error: ~w~n", [Error])
  end.