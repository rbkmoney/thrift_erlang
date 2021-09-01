-module(thrift_meta).

-export([encode_enum/3]).

-export([type_info/3]).

-type namespace() :: module().
-type type() :: atom().
-type type_ref() :: {namespace(), type()}.
-type type_info() :: type_ref() | struct_info().

-type struct_flavour() :: struct | exception | union.
-type field_num() :: pos_integer().
-type field_req() :: required | optional | undefined.
-type field_name() :: atom().
-type field_type() ::
    bool
    | byte
    | i16
    | i32
    | i64
    | string
    | double
    | {enum, type_ref()}
    | {struct, struct_flavour(), type_ref()}
    | {list, field_type()}
    | {set, field_type()}
    | {map, field_type(), field_type()}.

-type struct_field_info() ::
    {field_num(), field_req(), field_type(), field_name(), any()}.
-type struct_info() ::
    {struct, struct_flavour(), [struct_field_info()]}.

-type type_path() ::
    [{variant, atom()} | {field, atom()}].

-type enum_value() :: atom().
%% -type enum_info() :: {enum, [{enum_value(), integer()}]}.

%%====================================================================
%% API functions
%%====================================================================

-spec encode_enum(namespace(), type(), binary()) -> {ok, enum_value()} | {error, unknown_atom | unknown_variant}.
encode_enum(Module, Type, Binary) when is_binary(Binary) ->
    {enum, Variants} = Module:enum_info(Type),
    try erlang:binary_to_existing_atom(Binary, utf8) of
        Atom ->
            case lists:keyfind(Atom, 1, Variants) of
                false -> {error, unknown_variant};
                _ -> {ok, Atom}
            end
    catch
        error:badarg ->
            {error, unknown_atom}
    end.

type_info(Module, Type, Path) when is_list(Path) ->
    type_info({Module, Type}, Path).

-spec type_info(type_info(), type_path()) -> type_info().
type_info(TypeInfo, []) ->
    TypeInfo;
type_info(TypeInfo, [{variant, Variant} | Rest]) ->
    type_info(fetch_struct_field(TypeInfo, union, Variant), Rest);
type_info(TypeInfo, [{field, Field} | Rest]) ->
    type_info(fetch_struct_field(TypeInfo, struct, Field), Rest);
type_info(_, _) ->
    error(badarg).

%%====================================================================
%% Internal functions
%%====================================================================

fetch_struct_field(TypeInfo, Kind, Field) ->
    {Module, Type} = extract_type(TypeInfo),
    {struct, Kind, Fields} = Module:struct_info(Type),
    lists:keyfind(Field, 4, Fields).

-spec extract_type(type_info()) -> {module(), type()}.
extract_type(Type = {Module, Name}) when is_atom(Module), is_atom(Name) ->
    Type;
extract_type({struct, struct, Type = {Module, Name}}) when is_atom(Module), is_atom(Name) ->
    Type;
extract_type({enum, Type = {Module, Name}}) when is_atom(Module), is_atom(Name) ->
    Type;
extract_type({_ID, _Kind, TypeInfo, _Field, _Default}) ->
    extract_type(TypeInfo);
extract_type(_) ->
    error(badarg).
