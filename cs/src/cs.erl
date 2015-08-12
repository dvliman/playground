-module(cs).
-behavior(application).

-export([start/0, start/2]).
-export([stop/1]).

start() ->
    load_beams(),
    lager:start(),
    application:start(lager),
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(ranch),
    application:start(cowlib),
    application:start(cowboy),
    application:start(ibrowse),
    application:start(cs).

start(_Type, _Args) ->
    Port = utils:get_config(port),

    Dispatch = cowboy_router:compile([
        {'_', [{"/api/v1/links/[:link_id]",         rest, [link_handler]},
               {"/api/v1/merchants/[:merchant_id]", rest, [merchant_handler]},
               {"/api/v1/categories",               rest, [category_handler]}]}]),

    cowboy:start_http(http, 100,
        [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]),

    sup:start_link().

stop(_State) ->
    application:stop(cs).

load_beams() ->
    [code:load_file(list_to_atom(re:replace(filename:basename( F ), "[.]beam$", "", [{return, list}])))
        || P <- code:get_path(), string:str(P, code:lib_dir()) == 0, F <- filelib:wildcard(filename:join(P, "*.beam"))],
    ok.