-module(v).

-export([validate/2]).
-export([required/1, required/3]).
-export([optional/1, optional/3]).

% fold all errors into either
%   ok | {error, [Json]}
%   Json = {"key": Key, "value": Value, "reason": Reason}
validate(I, S) ->
    R1 = validate_required(I, S),
    R2 = validate_params(I, S),

    Errors = lists:foldl(fun(Elem, Acc) ->
                            case Elem of
                                {ok, {_, _}} -> Acc;
                                {error, Reason, {Key, Value}} ->
                                    Error = {[{key, Key},
                                              {value, Value},
                                              {reason, Reason}]},
                                    [Error | Acc]
                            end
                         end, [], R1 ++ R2),

    case length(Errors) > 0 of
        false -> ok;
        true -> {error, Errors}
    end.

% {error, missing, {Key, undefined}} |
validate_required(I, S) ->
    InputParams = [Name || {Name, _Value} <- I],
    R = [case Bool of
              true ->
                  ParamName;
              false ->
                  false
          end || {{_, ParamName}, {required, Bool}, {_, _}, {_, _}} <- S],
    ReqParams = lists:filter(fun(A) -> A =/= false end, R),
    V = [lists:member(N, InputParams) || N <- ReqParams],
    [{error, missing, {Name, undefined}} || {Name, false} <- lists:zip(ReqParams, V)].

% {ok, {Key, Value}} | {error, Reason, {Key, Value}}
validate_params(Input, Spec) ->
    [case lists:filter(fun(A) -> A =/= skip end, [find_spec(Name, S) || S <- Spec]) of
          [H|_T] ->
              {{name, Name}, {required, _Bool}, {Type, F}, {error, Reason}} = H,
              try
                  {ok, Coerce} = coerce_type(Type, Value),
                  case F(Coerce) of
                      true ->
                          {ok, {Name, Coerce}};
                      false ->
                          {error, Reason, {Name, Value}}
                  end
              catch
                  _:_ ->
                      {error, bad, {Name, Value}}
              end;
          [] ->
              {error, unknown_extra, {Name, Value}}
      end || {Name, Value} <- Input].

required(Key) ->
    {{name, Key}, {required, true},
     {binary, fun(_) -> true end}, {error, no_error}}.

required(Key, Fun, ErrorMessage) ->
    {{name, Key}, {required, true},
     {binary, Fun}, {error, ErrorMessage}}.

optional(Key) ->
    {{name, Key}, {required, false},
     {binary, fun(_) -> true end}, {error, no_error}}.

optional(Key, Fun, ErrorMessage) ->
    {{name, Key}, {required, false},
     {binary, Fun}, {error, ErrorMessage}}.

find_spec(P, S) ->
    case S of
        {{name, P}, R, V, E} ->
            {{name, P}, R, V, E};
        _ ->
            skip
    end.

coerce_type(Type, Value) ->
    case Type of
        int when is_list(Value) ->
            try
                V = list_to_integer(Value),
                {ok, V}
            catch
                _:_ ->
                    {bad, Value}
            end;
        int when is_integer(Value) ->
            {ok, Value};
        string when is_list(Value) ->
            {ok, Value};
        binary when is_list(Value) ->
            try
                V = list_to_binary(Value),
                {ok, V}
            catch
                _:_ ->
                    {bad, Value}
            end;
        binary when is_binary(Value) ->
            {ok, Value};
        tuple when is_list(Value) ->
            try
                V = list_to_tuple(Value),
                {ok, V}
            catch
                _:_ ->
                    {bad, Value}
            end;
        tuple when is_tuple(Value) ->
            {ok, Value};
        list when is_list(Value) ->
            {ok, Value}
    end.

