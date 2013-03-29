-module(chat).

-export([start/0]).

start() ->
    IndexPid = spawn(fun () -> index([]) end),
    {ok, LSock} = gen_tcp:listen(5999, [{active, true}, {packet, line}, {reuseaddr, true}]),
    accept_loop(LSock, IndexPid).

accept_loop(LSock, IndexPid) ->
    case gen_tcp:accept(LSock) of
        {ok, Sock} ->
            gen_tcp:controlling_process(Sock, spawn(fun () -> connection(Sock, IndexPid) end)),
            accept_loop(LSock, IndexPid)
    end.

index(Connected) ->
    receive
        {arrive, Pid} ->
            [begin
                 P ! {utterance, {arrive, Pid}},
                 Pid ! {utterance, {arrive, P}}
             end || P <- Connected],
            monitor(process, Pid),
            index([Pid | Connected]);
        {'DOWN', _, process, Pid, _} ->
            NewConnected = Connected -- [Pid],
            [P ! {utterance, {depart, Pid}} || P <- NewConnected],
            index(NewConnected);
        M = {_Pid, says, _Thing} ->
            [P ! {utterance, M} || P <- Connected],
            index(Connected);
        Other ->
            error_logger:error_report({index, unhandled, Other})
    end.

say(Sock, V) ->
    gen_tcp:send(Sock, io_lib:format("~p~n", [V])).

connection(Sock, IndexPid) ->
    IndexPid ! {arrive, self()},
    say(Sock, {you_are, self()}),
    connection_mainloop(Sock, IndexPid).

connection_mainloop(Sock, IndexPid) ->
    receive
        {utterance, V} ->
            say(Sock, V),
            connection_mainloop(Sock, IndexPid);
        {tcp, _, Line} ->
            IndexPid ! {self(), says, Line},
            connection_mainloop(Sock, IndexPid);
        {tcp_closed, _} ->
            ok;
        Other ->
            error_logger:error_report({connection, unhandled, Other})
    end.
