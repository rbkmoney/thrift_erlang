-module(ast_thrift_gen).

-include("gen-erlang/ast_thrift.hrl").
-include_lib("proper/include/proper.hrl").

-export([gen/1]).

-spec gen(_Complexity) ->
    {ok, #'Module'{}}.

%% Generates an instance of AST.Module thrift model with given complexity.
%% Useful for benchmarking purposes.
gen(Complexity) ->
    proper_gen:pick(t_module(), Complexity).

%%

-type typ() :: proper_types:raw_type().

-spec t_module() -> typ().
t_module() ->
    #'Module'{
        name = t_atomstr(),
        forms = t_optional(list(t_form()))
    }.

-spec t_optional(typ()) -> typ().
t_optional(Type) ->
    frequency([
        {2, Type},
        {1, undefined}
    ]).

-spec t_form() -> typ().
t_form() ->
    ?LAZY(#'Form'{
        type = frequency([
            {1, {attribute, t_attribute()}},
            {1, {docstring, t_docstring()}},
            {8, {fn, t_function()}}
        ]),
        annos = map(t_string(), t_anno()),
        coord = t_coord(),
        hash = t_i64(),
        aliases = t_optional(list(t_string())),
        backrefs = t_optional(list(t_coord())),
        context = t_optional(binary()),
        valid = boolean(),
        insrumented = t_optional(boolean())
    }).

-spec t_attribute() -> typ().
t_attribute() ->
    #'Attribute'{
        name = t_atomstr(),
        val = t_term()
    }.

-spec t_docstring() -> typ().
t_docstring() ->
    #'Docstring'{
        content = binary()
    }.

-spec t_anno() -> typ().
t_anno() ->
    #'Anno'{
        tag = t_i64(),
        val = t_string()
    }.

-spec t_function() -> typ().
t_function() ->
    ?LAZY(#'Function'{
        name = t_atomstr(),
        arity = t_i8(),
        priv = t_optional(boolean()),
        clauses = t_optional(list(t_clause()))
    }).

-spec t_clause() -> typ().
t_clause() ->
    ?LAZY(#'Clause'{
        head = list(t_binding()),
        body = list(?SIZED(S, t_expression(S))),
        coord = t_coord()
    }).

-spec t_binding() -> typ().
t_binding() ->
    oneof([
        {variable, t_atomstr()},
        {pat, t_term()},
        {hole, #'Hole'{}}
    ]).

-spec t_expression(_Size) -> typ().
t_expression(S) ->
    ?LAZY(#'Expression'{
        type = oneof([
            {match, t_match(S)},
            {call, t_call(S)}
        ]),
        annos = t_optional(map(t_string(), t_anno())),
        coord = t_coord()
    }).

-spec t_match(_Size) -> typ().
t_match(S) ->
    #'Match'{
        lhs = t_binding(),
        rhs = t_expression(half_size(S))
    }.

-spec t_call(_Size) -> typ().
t_call(S) ->
    #'Call'{
        mod = t_optional(t_atomstr()),
        name = t_atomstr(),
        arglist = resize(S, list(t_expression(half_size(S))))
    }.

-spec t_coord() -> typ().
t_coord() ->
    #'Coord'{
        mod = t_atomstr(),
        file = t_string(),
        line = t_i32(),
        column = t_i32()
    }.

-spec t_term() -> typ().
t_term() ->
    ?SIZED(S, t_term(S)).

-spec t_term(_Size :: non_neg_integer()) -> typ().
t_term(S) ->
    ?LAZY(
        oneof([
            {atom, t_atomstr()},
            {int, t_i64()},
            {int, integer()},
            {flt, float()},
            {bin, binary()},
            {lst, resize(S, list(t_term(half_size(S))))},
            {mp, t_map(S)}
        ])
    ).

-spec t_map(_Size :: non_neg_integer()) -> typ().
t_map(S) ->
    ?LET(
        L,
        resize(S, list({t_term(half_size(S)), t_term(half_size(S))})),
        maps:from_list(L)
    ).

-spec half_size(Size) -> Size when Size :: non_neg_integer().
half_size(Size) ->
    case Size div 2 of
        N when N > 0, N rem 2 == 0 ->
            N - 1;
        N ->
            N
    end.

-spec t_string() -> typ().
t_string() ->
    ?LET(S, string(), unicode:characters_to_binary(S)).

-spec t_i64() -> typ().
t_i64() ->
    integer(-(1 bsl 63), (1 bsl 63) - 1).

-spec t_i32() -> typ().
t_i32() ->
    integer(-(1 bsl 31), (1 bsl 31) - 1).

-spec t_i8() -> typ().
t_i8() ->
    integer(-(1 bsl 7), (1 bsl 7) - 1).

-spec t_atomstr() -> typ().
t_atomstr() ->
    ?LET(S, list(integer($a, $z)), unicode:characters_to_binary(S)).
