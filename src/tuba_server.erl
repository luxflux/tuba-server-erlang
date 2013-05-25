-module(tuba_server).

-behaviour(gen_server).

-export([start_link/0, say_hello/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

say_hello() ->
    gen_server:call(?MODULE, hello).


%% callbacks
handle_call(hello, _From, State) ->
    io:format("Hello from server!~n", []),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%start() ->
%    spawn(fun() -> server(4000) end).
%
%server(Port) ->
%    {ok, Socket} = gen_udp:open(Port, [binary, {active, false}]),
%    io:format("server opened socket:~p~n",[Socket]),
%    loop(Socket).
%
%loop(Socket) ->
%    inet:setopts(Socket, [{active, once}]),
%    receive
%        {udp, Socket, Host, Port, Bin} ->
%            io:format("server received:~p~n",[Bin]),
%            gen_udp:send(Socket, Host, Port, Bin),
%            loop(Socket)
%    end.

