-module(rest).
-include("cs.hrl").

-export([init/3,
         rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         resource_exists/2,
         service_available/2,
         content_types_accepted/2,
         content_types_provided/2,
         is_authorized/2,
         forbidden/2,
         delete_resource/2,
         process_form/2,
         process_json/2,
         process_any/2,
         process_get/2]).

-export([write_response/3]).

init(_Transport, _Req, _) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, [Module]) ->
    {Method, Req0} = cowboy_req:method(Req),
    {Path, Req1} = cowboy_req:path_info(Req0),

    {ok, Req1, #state{mod = Module,
                      method = Method,
                      path = Path}}.

rest_terminate(_Req, _State) ->
    ok.

allowed_methods       (Req, State) -> hook(allowed_methods, Req, State).
resource_exists       (Req, State) -> {true, Req, State}.
service_available     (Req, State) -> {true, Req, State}.
content_types_accepted(Req, State) -> hook(content_types_accepted, Req, State).
content_types_provided(Req, State) -> hook(content_types_provided, Req, State).
is_authorized         (Req, State) -> {true, Req, State}.
forbidden             (Req, State) -> {false, Req, State}.
delete_resource       (Req, State) -> hook(delete_resource, Req, State).
process_form          (Req, State) -> hook(process_form,    Req, State).
process_json          (Req, State) -> hook(process_json,    Req, State).
process_any           (Req, State) -> hook(process_any,     Req, State).
process_get           (Req, State) -> hook(process_get,     Req, State).

hook(Fun, Req, #state{mod = Mod} = State) ->
    case erlang:function_exported(Mod, Fun, 2) of
        true  -> Mod:Fun(Req, State);
        false ->
            try default(Fun, Req, State)
            catch
                _:function_clause ->
                    lager:error("need implementation ~p:~p/2", [Mod, Fun]),
                    write_response({error, need_implementation}, Req, State)
            end
    end.

hook(Fun, Param, Req, #state{mod = Mod} = State) ->
    case erlang:function_exported(Mod, Fun, 3) of
        true  -> Mod:Fun(Param, Req, State);
        false -> write_response({error, need_implementation}, Req, State)
    end.

default(content_types_accepted, Req, State) ->
    {[{{<<"application">>, <<"json">>,                  '*'}, process_json},
      {{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, process_form},
      {{<<"application">>, <<"x-www-form-data">>,       '*'}, process_form},
      {'*', process_any}], Req, State};

default(content_types_provided, Req, State) ->
    {[{<<"application/json">>, process_get}], Req, State};

default(process_form, Req, State) ->
    case cowboy_req:body_qs(Req) of
        {ok, Props, Req1} -> hook(process_post, Props, Req1, State);
        {error, _Reason} -> write_response({error, parse_form}, Req, State)
    end;

default(process_json, Req, State) ->
    try
        {ok, Body, Req1} = cowboy_req:body(Req),
        {Value} = jiffy:decode(Body),
        hook(process_post, Value, Req1, State)
    catch
        _:_ ->
            write_response({error, parse_json}, Req, State)
    end.

write_response(ok, _, _) -> ok;

write_response({ok, Props}, Req, State) ->
    Body = jiffy:encode({[{data, {Props}}]}),
    Req1 = cowboy_req:set_resp_body(Body, Req),
    {true, Req1, State};

write_response({error, Reason}, Req, State) ->
    {StatusCode, Message} = error_to_statuscode(Reason),
    Body = jiffy:encode({[{error, {[{code, StatusCode},
                                    {message, Message},
                                    {reason, Reason}]}}]}),

    Req1 = cowboy_req:set_resp_body(Body, Req),
    {ok, Req2} = cowboy_req:reply(StatusCode, Req1),
    {halt, Req2, State}.

error_to_statuscode(parse_json)                                  -> {400, bad_request};
error_to_statuscode([{[{key, _}, {value, _}, {reason, _}]} | _]) -> {400, bad_request};

error_to_statuscode(need_implementation) -> {500, internal_server_error};
error_to_statuscode(_)                   -> {500, unknown}.


