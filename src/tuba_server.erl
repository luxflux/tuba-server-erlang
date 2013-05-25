-module(tuba_server).

-export([start_link/0]).

start_link() ->
    spawn(fun() -> server(4000) end).

server(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary, {active, false}]),
    io:format("server opened socket:~p~n",[Socket]),
    loop(Socket).

loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {udp, Socket, Host, Port, Bin} ->
            io:format("server received:~p~n",[Bin]),
            gen_udp:send(Socket, Host, Port, Bin),
            loop(Socket)
    end.
