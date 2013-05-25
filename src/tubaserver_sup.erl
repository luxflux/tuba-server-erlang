
-module(tubaserver_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    spawn(fun() -> server(4000) end),
    {ok, { {one_for_one, 5, 10}, []} }.

