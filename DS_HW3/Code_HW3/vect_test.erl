-module(vect_test).
-export([run/2]).

run(Sleep, Jitter) ->
    Log = loger:start([john, paul, ringo, george]),
    A = vect:start(john, Log, 13, Sleep, Jitter),
    B = vect:start(paul, Log, 23, Sleep, Jitter),
    C = vect:start(ringo, Log, 36, Sleep, Jitter),
    D = vect:start(george, Log, 49, Sleep, Jitter),
    vect:peers(A, [B, C, D]),
    vect:peers(B, [A, C, D]),
    vect:peers(C, [A, B, D]),
    vect:peers(D, [A, B, C]),
    timer:sleep(5000),
    loger:stop(Log),
    vect:stop(A),
    vect:stop(B),
    vect:stop(C),
    vect:stop(D).
