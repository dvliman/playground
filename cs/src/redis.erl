-module(redis).
-behavior(gen_server).

-export([q/1, qp/1]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% a wrapper on eredis so we always query against
% a single connection. if later this becomes a bottleneck
% we can set up poolboy, or round robin multiple connections
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

q(Command) ->
    gen_server:call(?MODULE, {q, Command}).

qp(Pipeline) ->
    gen_server:call(?MODULE, {qp, Pipeline}).

% callbacks
init([]) ->
    {ok, Client} = eredis:start_link(),
    {ok, Client}.

handle_call({q, Command}, _From, State) ->
    Reply = eredis:q(State, Command),
    {reply, Reply, State};

handle_call({qp, Command}, _From, State) ->
    Reply = eredis:qp(State, Command),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.
