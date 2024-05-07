-module(routy_test).
-export([start/0, statelink/0, status/0, message/4, stop/0, update/0, send_example_message/0]).

% Step 1. start a country shell
% erl -name sweden@130.123.112.23 -setcookie routy -connect_all false


% Step 2. add routers to that country
start() ->
    % initializing nodes
    routy:start(stockholm, stockholm),
    routy:start(lund, lund),
    routy:start(uppsala, uppsala),
    routy:start(helsingborg, helsingborg),
    routy:start(kiruna, kiruna),
    % adding links
    stockholm ! {add, lund, {lund, 'sweden@130.123.112.23'}},
    stockholm ! {add, uppsala, {uppsala, 'sweden@130.123.112.23'}},
    stockholm ! {add, helsingborg, {helsingborg, 'sweden@130.123.112.23'}},
    stockholm ! {add, kiruna, {kiruna, 'sweden@130.123.112.23'}},
    lund ! {add, stockholm, {stockholm, 'sweden@130.123.112.23'}},
    uppsala ! {add, helsingborg, {helsingborg, 'sweden@130.123.112.23'}},
    helsingborg ! {add, stockholm, {stockholm, 'sweden@130.123.112.23'}},
    kiruna ! {add, helsingborg, {helsingborg, 'sweden@130.123.112.23'}}.   

% Step 3. sending state-link messages and update the respective routing tables 
statelink() ->
    % stockholm broadcasts to its neighbors
    stockholm ! broadcast,
    timer:sleep(1000), % let all the nodes update their maps 
    % make the nodes update their tables 
    update(),
    timer:sleep(2000), % let all the nodes update their tables
    % lund broadcasts to its neighbors 
    lund ! broadcast,
    timer:sleep(1000), % let all the nodes update their maps 
    % make the nodes update their tables 
    update(),
    timer:sleep(2000), % let all the nodes update their tables
    % uppsala broadcasts to its neighbors 
    uppsala ! broadcast,
    timer:sleep(1000), % let all the nodes update their maps 
    % make the nodes update their tables 
    update(),
    timer:sleep(2000), % let all the nodes update their tables
    % helsingborg broadcasts to its neighbors 
    helsingborg ! broadcast,
    timer:sleep(1000), % let all the nodes update their maps  
    % make the nodes update their tables 
    update(),
    timer:sleep(2000),
    % kiruna broadcasts to its neighbors
    kiruna ! broadcast,
    timer:sleep(1000), % let all the nodes update their maps 
    % make the nodes update their tables 
    update(),
    timer:sleep(2000). % let all the nodes update their tables

update() ->
    % this function make the nodes update their respective routing tables 
    stockholm ! update,
    lund ! update,
    uppsala ! update,
    helsingborg ! update,
    kiruna ! update.

status() ->
    % check the status of each node
    stockholm ! {status, self()},
    lund ! {status, self()},
    uppsala ! {status, self()},
    helsingborg ! {status, self()},
    kiruna ! {status, self()}.

    % this function aims to test the messaging functionality 
message(LocalRouterRef, To, From, Message) ->
    % - LocalRouterRef: The PID (Process Identifier) of the local router sending the message.
    LocalRouterRef ! {route, To, From, Message}.

send_example_message() ->
    % all routers must been started

    % get the reference to the sender router (e.g., Stockholm).
    Sender = self(),

    % get the reference to the destination router (e.g., Lund).
    Dest = lund,

    % define the message you want to send as a tuple.
    Message = {greeting, "Hello, Lund!"},

    % use the message/4 function to send the message.
    message(Sender, Dest, Sender, Message).

stop() ->
    % stop all the routers 
    stockholm ! stop,
    lund ! stop,
    uppsala ! stop,
    helsingborg ! stop,
    kiruna ! stop.