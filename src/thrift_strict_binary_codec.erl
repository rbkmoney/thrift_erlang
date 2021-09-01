%%
%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements. See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership. The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License. You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

%% This module specifically designed for coding Thrift protocol
%% structures into and from a plain binary, following binary
%% protocol with strict read and write turned on.
%%
%% It's essentially a result of squashing `thrift_protocol`,
%% `thrift_binary_protocol` and `thrift_membuffer_transport`
%% together in a single module, with aggressive inlining and
%% shedding of unnecessary allocations. Validation still works
%% but it's now performed in the same single encoding pass.
%% All these measures give significant performance improvements
%% over generic construction, both in terms of reductions and
%% memory pressure. With a sample, quite complex (1Mb on the
%% wire) Thrift model we've seen 5x speedup in encoding and 4x
%% speedup in decoding.

-module(thrift_strict_binary_codec).

-compile(inline).

-export([
    new/0,
    new/1,
    write/3,
    write_message_begin/4,
    write_message_end/1,
    read/2,
    read_message_begin/1,
    read_message_end/1,
    skip/2,
    validate/1,
    close/1
]).

-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

-opaque buffer() :: binary().

-export_type([buffer/0]).

-spec new() -> buffer().
new() ->
    new(<<>>).

-spec new(iodata()) -> buffer().
new(Buf) ->
    impl_transport_new(Buf).

-spec close(buffer()) -> binary().
close(Proto) ->
    Proto.

% typeid_to_atom(?tType_STOP) -> field_stop;
typeid_to_atom(?tType_VOID) -> void;
typeid_to_atom(?tType_BOOL) -> bool;
typeid_to_atom(?tType_DOUBLE) -> double;
typeid_to_atom(?tType_I8) -> byte;
typeid_to_atom(?tType_I16) -> i16;
typeid_to_atom(?tType_I32) -> i32;
typeid_to_atom(?tType_I64) -> i64;
typeid_to_atom(?tType_STRING) -> string;
typeid_to_atom(?tType_STRUCT) -> struct;
typeid_to_atom(?tType_MAP) -> map;
typeid_to_atom(?tType_SET) -> set;
typeid_to_atom(?tType_LIST) -> list;
typeid_to_atom(TypeId) -> {unknown_type_id, TypeId}.

term_to_typeid(void) -> ?tType_VOID;
term_to_typeid(bool) -> ?tType_BOOL;
term_to_typeid(byte) -> ?tType_I8;
term_to_typeid(double) -> ?tType_DOUBLE;
term_to_typeid(i8) -> ?tType_I8;
term_to_typeid(i16) -> ?tType_I16;
term_to_typeid(i32) -> ?tType_I32;
term_to_typeid(i64) -> ?tType_I64;
term_to_typeid(string) -> ?tType_STRING;
term_to_typeid({struct, _, _}) -> ?tType_STRUCT;
term_to_typeid({enum, _}) -> ?tType_I32;
term_to_typeid({map, _, _}) -> ?tType_MAP;
term_to_typeid({set, _}) -> ?tType_SET;
term_to_typeid({list, _}) -> ?tType_LIST.

-spec read_message_begin(buffer()) ->
    {ok, #protocol_message_begin{}, buffer()} | {error, any()}.

read_message_begin(IProto) ->
    impl_read_message_begin(IProto).

-spec read_message_end(buffer()) ->
    {ok, ok, buffer()}.

read_message_end(IProto) ->
    {ok, ok, IProto}.

-spec read
    (buffer(), {struct, _Flavour, _Info}) ->
        {ok, tuple(), buffer()} | {error, _Reason};
    (buffer(), tprot_cont_tag()) ->
        {ok, any(), buffer()} | {error, _Reason};
    (buffer(), tprot_empty_tag()) ->
        {ok, ok, buffer()} | {error, _Reason};
    (buffer(), tprot_header_tag()) ->
        {ok, tprot_header_val(), buffer()} | {error, _Reason};
    (buffer(), tprot_data_tag()) ->
        {ok, any(), buffer()} | {error, _Reason}.

read(IProto, Type) ->
    try read_frag(IProto, Type, []) of
        {IProto2, Data} ->
            {ok, Data, IProto2}
    catch
        throw:Reason ->
            {error, Reason}
    end.

read_union(IProto0, StructDef, Path) ->
    % {IProto1, ok} = read_frag(IProto0, struct_begin),
    {IProto1, Result} = read_union_loop(IProto0, StructDef, undefined, Path),
    case Result of
        {_, _} ->
            {IProto1, Result};
        _ ->
            throw({invalid, Path, Result})
    end.

read_struct(IProto0, StructDef, undefined, Path) ->
    {IProto1, Acc} = read_struct_loop(IProto0, StructDef, 1, [], Path),
    Record = erlang:make_tuple(length(StructDef), undefined, Acc),
    {IProto1, validate_struct(StructDef, Record, 1, Path)};
read_struct(IProto0, StructDef, Tag, Path) ->
    % If we want a tagged tuple, we need to offset all the tuple indices
    % by 1 to avoid overwriting the tag.
    {IProto1, Acc} = read_struct_loop(IProto0, StructDef, 2, [{1, Tag}], Path),
    Record = erlang:make_tuple(length(StructDef) + 1, undefined, fill_default_struct(2, StructDef, Acc)),
    {IProto1, validate_struct(StructDef, Record, 2, Path)}.

fill_default_struct(_, [], Acc) ->
    Acc;
fill_default_struct(N, [FieldDef | Rest], Acc) when element(5, FieldDef) =:= undefined ->
    fill_default_struct(N + 1, Rest, Acc);
fill_default_struct(N, [FieldDef | Rest], Acc) ->
    fill_default_struct(N + 1, Rest, [{N, element(5, FieldDef)} | Acc]).

-define(read_byte(V), V:8 / integer - signed - big).
-define(read_i16(V), V:16 / integer - signed - big).
-define(read_i32(V), V:32 / integer - signed - big).
-define(read_i64(V), V:64 / integer - signed - big).
-define(read_double(V), V:64 / float - signed - big).

-define(read_list(Etype, Size), ?read_byte(Etype), ?read_i32(Size)).
-define(read_set(Etype, Size), ?read_byte(Etype), ?read_i32(Size)).
-define(read_map(Ktype, Vtype, Size), ?read_byte(Ktype), ?read_byte(Vtype), ?read_i32(Size)).

read_frag(IProto, {struct, union, {Module, StructName}}, Path) when
    is_atom(Module), is_atom(StructName)
->
    read_union(IProto, element(3, Module:struct_info(StructName)), Path);
read_frag(IProto, {struct, _, {Module, StructName}}, Path) when
    is_atom(Module), is_atom(StructName)
->
    read_struct(IProto, element(3, Module:struct_info(StructName)), Module:record_name(StructName), Path);
read_frag(IProto, {enum, {Module, EnumName}}, Path) when is_atom(Module) ->
    read_enum(IProto, element(2, Module:enum_info(EnumName)), Path);
read_frag(
    <<?read_list(EType, Size), IProto1/binary>> = IProto,
    {list, Type} = SType,
    Path
) ->
    case term_to_typeid(Type) of
        EType ->
            read_list_loop(IProto1, Type, Size, Path);
        % IProto3 = read_frag(IProto2, list_end),
        _ ->
            throw({unexpected, Path, SType, IProto})
    end;
read_frag(
    <<?read_map(KType, VType, Size), IProto1/binary>> = IProto,
    {map, KeyType, ValType} = SType,
    Path
) ->
    case KType =:= term_to_typeid(KeyType) andalso VType =:= term_to_typeid(ValType) of
        true ->
            read_map_loop(IProto1, KeyType, ValType, Size, Path);
        % IProto3 = read_frag(IProto2, map_end),
        _ ->
            throw({unexpected, Path, SType, IProto})
    end;
read_frag(
    <<?read_set(EType, Size), IProto1/binary>> = IProto,
    {set, Type} = SType,
    Path
) ->
    case term_to_typeid(Type) of
        EType ->
            read_set_loop(IProto1, Type, Size, Path);
        % IProto3 = read_frag(IProto2, set_end),
        _ ->
            throw({unexpected, Path, SType, IProto})
    end;
read_frag(<<?read_byte(Byte), Proto/binary>>, bool, _) ->
    {Proto, Byte /= 0};
read_frag(<<?read_byte(Val), Proto/binary>>, byte, _) ->
    {Proto, Val};
read_frag(<<?read_i16(Val), Proto/binary>>, i16, _) ->
    {Proto, Val};
read_frag(<<?read_i32(Val), Proto/binary>>, i32, _) ->
    {Proto, Val};
read_frag(<<?read_i64(Val), Proto/binary>>, i64, _) ->
    {Proto, Val};
read_frag(<<?read_double(Val), Proto/binary>>, double, _) ->
    {Proto, Val};
read_frag(<<?read_i32(Sz), Proto/binary>>, string, _) ->
    read_data(Proto, Sz);
read_frag(IProto, {struct, union, StructDef}, Path) when is_list(StructDef) ->
    % reading w/o validation here for easier handling of replies in the client codec
    read_union_loop(IProto, StructDef, undefined, Path);
read_frag(IProto, {struct, _, StructDef}, Path) when is_list(StructDef) ->
    read_struct(IProto, StructDef, undefined, Path);
read_frag(Proto, Type, Path) ->
    throw({unexpected, Path, Type, Proto}).

-spec read_list_loop(buffer(), any(), non_neg_integer(), [atom()]) -> {buffer(), [any()]}.
read_list_loop(Proto0, ValType, Size, Path) ->
    read_list_loop(Proto0, ValType, Size, [], Path).

read_list_loop(Proto0, _ValType, 0, List, _Path) ->
    {Proto0, lists:reverse(List)};
read_list_loop(Proto0, ValType, Left, List, Path) ->
    {Proto1, Val} = read_frag(Proto0, ValType, Path),
    read_list_loop(Proto1, ValType, Left - 1, [Val | List], Path).

-spec read_map_loop(buffer(), any(), any(), non_neg_integer(), [atom()]) -> {buffer(), map()}.
read_map_loop(Proto0, KeyType, ValType, Size, Path) ->
    read_map_loop(Proto0, KeyType, ValType, Size, #{}, Path).

read_map_loop(Proto0, _KeyType, _ValType, 0, Map, _Path) ->
    {Proto0, Map};
read_map_loop(Proto0, KeyType, ValType, Left, Map, Path) ->
    {Proto1, Key} = read_frag(Proto0, KeyType, Path),
    {Proto2, Val} = read_frag(Proto1, ValType, Path),
    read_map_loop(Proto2, KeyType, ValType, Left - 1, maps:put(Key, Val, Map), Path).

-spec read_set_loop(buffer(), any(), non_neg_integer(), [atom()]) -> {buffer(), ordsets:ordset(any())}.
read_set_loop(Proto0, ValType, Size, Path) ->
    read_set_loop(Proto0, ValType, Size, ordsets:new(), Path).

read_set_loop(Proto0, _ValType, 0, Set, _Path) ->
    {Proto0, Set};
read_set_loop(Proto0, ValType, Left, Set, Path) ->
    {Proto1, Val} = read_frag(Proto0, ValType, Path),
    read_set_loop(Proto1, ValType, Left - 1, ordsets:add_element(Val, Set), Path).

read_union_loop(<<?read_byte(?tType_STOP), IProto1/binary>>, _StructDef, Acc, _Path) ->
    % {IProto2, ok} = read_frag(IProto1, struct_end),
    {IProto1, Acc};
read_union_loop(<<?read_byte(FType), ?read_i16(Fid), IProto1/binary>>, StructDef, Acc, Path) ->
    case lists:keyfind(Fid, 1, StructDef) of
        {_, _Req, Type, Name, _Default} ->
            case term_to_typeid(Type) of
                FType ->
                    {IProto2, Val} = read_frag(IProto1, Type, [Name | Path]),
                    read_union_loop(IProto2, StructDef, set_union_val(Name, Val, Acc), Path);
                _Expected ->
                    IProto2 = skip_mistyped_field(IProto1, Name, FType),
                    read_union_loop(IProto2, StructDef, Acc, Path)
            end;
        _ ->
            IProto2 = skip_unknown_field(IProto1, Fid, FType),
            read_union_loop(IProto2, StructDef, Acc, Path)
    end;
read_union_loop(Proto, StructDef, _Acc, Path) ->
    throw({unexpected, Path, StructDef, Proto}).

set_union_val(Name, Val, undefined) ->
    {Name, Val};
set_union_val(Name, Val, Acc) ->
    [{Name, Val} | Acc].

read_struct_loop(<<?read_byte(?tType_STOP), IProto1/binary>>, _StructDef, _Offset, Acc, _Path) ->
    % {IProto2, ok} = read_frag(IProto1, struct_end),
    {IProto1, Acc};
read_struct_loop(<<?read_byte(FType), ?read_i16(Fid), IProto1/binary>>, StructDef, Offset, Acc, Path) ->
    case find_struct_field(Fid, StructDef, Offset) of
        {Idx, Name, Type} ->
            case term_to_typeid(Type) of
                FType ->
                    {IProto2, Val} = read_frag(IProto1, Type, [Name | Path]),
                    % {IProto3, ok} = read_frag(IProto2, field_end),
                    NewAcc = [{Idx, Val} | Acc],
                    read_struct_loop(IProto2, StructDef, Offset, NewAcc, Path);
                _Expected ->
                    IProto2 = skip_mistyped_field(IProto1, Name, FType),
                    read_struct_loop(IProto2, StructDef, Offset, Acc, Path)
            end;
        _ ->
            IProto2 = skip_unknown_field(IProto1, Fid, FType),
            read_struct_loop(IProto2, StructDef, Offset, Acc, Path)
    end;
read_struct_loop(Proto, StructDef, _Offset, _Acc, Path) ->
    throw({unexpected, Path, StructDef, Proto}).

find_struct_field(Fid, [{Fid, _, Type, Name, _} | _], Idx) ->
    {Idx, Name, Type};
find_struct_field(Fid, [_ | Rest], Idx) ->
    find_struct_field(Fid, Rest, Idx + 1);
find_struct_field(_, [], _) ->
    false.

skip_mistyped_field(IProto, Name, FType) ->
    FTypeAtom = typeid_to_atom(FType),
    error_logger:info_msg("Skipping field ~p with wrong type: ~p~n", [Name, FTypeAtom]),
    skip(IProto, FTypeAtom).

skip_unknown_field(IProto, Fid, FType) ->
    FTypeAtom = typeid_to_atom(FType),
    error_logger:info_msg("Skipping unknown field [~p] with type: ~p~n", [Fid, FTypeAtom]),
    skip(IProto, FTypeAtom).

validate_struct([], Record, _, _) ->
    Record;
validate_struct([{_Fid, required, _Type, Name, _} | Rest], Record, I, Path) ->
    case element(I, Record) of
        undefined ->
            throw({invalid, [Name | Path], undefined});
        _ ->
            validate_struct(Rest, Record, I + 1, Path)
    end;
validate_struct([_ | Rest], Record, I, Path) ->
    validate_struct(Rest, Record, I + 1, Path).

read_enum(<<?read_i32(IVal), IProto2/binary>>, Fields, Path) ->
    case lists:keyfind(IVal, 2, Fields) of
        {EnumVal, IVal} ->
            {IProto2, EnumVal};
        _ ->
            throw({invalid, Path, IVal})
    end.

-spec skip(buffer(), any()) -> buffer().

skip(Proto0, struct) ->
    % Proto1 = read_frag(Proto0, struct_begin),
    Proto1 = skip_struct_loop(Proto0),
    % Proto3 = read_frag(Proto2, struct_end),
    Proto1;
skip(<<?read_map(KType, VType, Size), Proto1/binary>>, map) ->
    Proto2 = skip_map_loop(Proto1, KType, VType, Size),
    % Proto3 = read_frag(Proto2, map_end),
    Proto2;
skip(<<?read_set(EType, Size), Proto1/binary>>, set) ->
    Proto2 = skip_list_loop(Proto1, EType, Size),
    % Proto3 = read_frag(Proto2, set_end),
    Proto2;
skip(<<?read_list(EType, Size), Proto1/binary>>, list) ->
    Proto2 = skip_list_loop(Proto1, EType, Size),
    % Proto3 = read_frag(Proto2, list_end),
    Proto2;
skip(Proto0, Type) when is_atom(Type) ->
    {Proto1, _Ignore} = read_frag(Proto0, Type, []),
    Proto1;
skip(Proto0, Type) when is_integer(Type) ->
    skip(Proto0, typeid_to_atom(Type));
skip(Proto, Type) ->
    throw({unexpected, Proto, Type, []}).

skip_struct_loop(<<?read_byte(?tType_STOP), Proto1/binary>>) ->
    % {IProto2, ok} = read_frag(Proto1, struct_end),
    Proto1;
skip_struct_loop(<<?read_byte(FType), ?read_i16(_), Proto1/binary>>) ->
    Proto2 = skip(Proto1, FType),
    % Proto3 = read(Proto2, field_end),
    skip_struct_loop(Proto2).

skip_map_loop(Proto0, _, _, 0) ->
    Proto0;
skip_map_loop(Proto0, Ktype, Vtype, Size) ->
    Proto1 = skip(Proto0, Ktype),
    Proto2 = skip(Proto1, Vtype),
    skip_map_loop(Proto2, Ktype, Vtype, Size - 1).

skip_list_loop(Proto0, _, 0) ->
    Proto0;
skip_list_loop(Proto0, Etype, Size) ->
    Proto1 = skip(Proto0, Etype),
    skip_list_loop(Proto1, Etype, Size - 1).

%%

-spec write_message_begin(
    buffer(),
    _Name :: binary(),
    _Type :: integer(),
    _SeqId :: integer()
) -> buffer().

write_message_begin(Proto, Name, Type, SeqId) ->
    impl_write_message_begin(Proto, Name, Type, SeqId).

-spec write_message_end(buffer()) -> buffer().

write_message_end(Proto) ->
    Proto.

-spec write(buffer(), any(), any()) -> {ok, buffer()} | {error, _Reason}.

write(Proto, Type, Data) ->
    try
        {ok, write_frag(Proto, Type, Data, [])}
    catch
        {invalid, Path, _Type, Value} ->
            {error, {invalid, lists:reverse(Path), Value}}
    end.

write_union(Proto0, StructDef, {Name, Value} = Data, Path) ->
    case lists:keyfind(Name, 4, StructDef) of
        {Fid, _, FType, _, _Default} ->
            % Proto1 = impl_write_struct_begin(Proto0, StructName),
            Proto1 = impl_write_field_begin(Proto0, Name, term_to_typeid(FType), Fid),
            Proto2 = write_frag(Proto1, FType, Value, [Name | Path]),
            % Proto4 = impl_write_field_end(Proto3),
            Proto3 = impl_write_field_stop(Proto2),
            % Proto6 = impl_write_struct_end(Proto5),
            Proto3;
        _ ->
            throw({invalid, Path, {struct, union, StructDef}, Data})
    end.

write_struct(Proto0, StructDef, Data, Path) ->
    % Proto1 = impl_write_struct_begin(Proto0, StructName),
    Proto1 = struct_write_loop(Proto0, StructDef, Data, 2, Path),
    % Proto3 = impl_write_struct_end(Proto2),
    Proto1.

%% thrift client specific stuff
write_frag(Proto, {struct, union, {Module, StructName}}, Data, Path) ->
    write_union(Proto, element(3, Module:struct_info(StructName)), Data, Path);
write_frag(Proto, {struct, _, {Module, StructName}} = Type, Data, Path) ->
    try Module:record_name(StructName) of
        RName when RName =:= element(1, Data) ->
            write_struct(Proto, element(3, Module:struct_info(StructName)), Data, Path);
        _ ->
            throw({invalid, Path, Type, Data})
    catch
        error:badarg ->
            throw({invalid, Path, Type, Data})
    end;
write_frag(Proto, {enum, {Module, EnumName}}, Data, Path) ->
    write_frag(Proto, Module:enum_info(EnumName), Data, Path);
write_frag(Proto, {enum, Fields} = Type, Data, Path) ->
    case lists:keyfind(Data, 1, Fields) of
        {Data, IVal} ->
            write_frag(Proto, i32, IVal, Path);
        _ ->
            throw({invalid, Path, Type, Data})
    end;
write_frag(Proto0, {list, Type}, Data, Path) when
    is_list(Data)
->
    Proto1 = impl_write_list_begin(Proto0, term_to_typeid(Type), length(Data)),
    Proto2 = lists:foldl(
        fun(Elem, ProtoIn) ->
            write_frag(ProtoIn, Type, Elem, Path)
        end,
        Proto1,
        Data
    ),
    % Proto3 = impl_write_list_end(Proto2),
    Proto2;
write_frag(Proto0, {map, KeyType, ValType}, Data, Path) when
    is_map(Data)
->
    Proto1 = impl_write_map_begin(Proto0, term_to_typeid(KeyType), term_to_typeid(ValType), map_size(Data)),
    Proto2 = maps:fold(
        fun(KeyData, ValData, ProtoS0) ->
            ProtoS1 = write_frag(ProtoS0, KeyType, KeyData, Path),
            ProtoS2 = write_frag(ProtoS1, ValType, ValData, Path),
            ProtoS2
        end,
        Proto1,
        Data
    ),
    % Proto3 = impl_write_map_end(Proto2),
    Proto2;
write_frag(Proto0, {set, Type}, Data, Path) when
    is_list(Data)
->
    case set_size(Data) of
        Size when is_integer(Size) ->
            Proto1 = impl_write_set_begin(Proto0, term_to_typeid(Type), Size),
            Proto2 = ordsets:fold(
                fun(Elem, ProtoIn) ->
                    write_frag(ProtoIn, Type, Elem, Path)
                end,
                Proto1,
                Data
            ),
            % Proto3 = impl_write_set_end(Proto2),
            Proto2;
        false ->
            throw({invalid, Path, Type, Data})
    end;
write_frag(Proto0, string, Data, _) when
    is_binary(Data)
->
    impl_write_string(Proto0, Data);
write_frag(Proto0, i64, Data, _) when
    is_integer(Data), Data >= -(1 bsl 63), Data < (1 bsl 63)
->
    impl_write_i64(Proto0, Data);
write_frag(Proto0, i32, Data, _) when
    is_integer(Data), Data >= -(1 bsl 31), Data < (1 bsl 31)
->
    impl_write_i32(Proto0, Data);
write_frag(Proto0, i16, Data, _) when
    is_integer(Data), Data >= -(1 bsl 15), Data < (1 bsl 15)
->
    impl_write_i16(Proto0, Data);
write_frag(Proto0, byte, Data, _) when
    is_integer(Data), Data >= -(1 bsl 7), Data < (1 bsl 7)
->
    impl_write_byte(Proto0, Data);
write_frag(Proto0, double, Data, _) when
    is_float(Data)
->
    impl_write_double(Proto0, Data);
write_frag(Proto0, bool, Data, _) when
    is_boolean(Data)
->
    impl_write_bool(Proto0, Data);
write_frag(Proto0, {struct, union, StructDef}, Data, Path) ->
    write_union(Proto0, StructDef, Data, Path);
write_frag(Proto0, {struct, _, StructDef}, Data, Path) ->
    % Proto1 = impl_write_struct_begin(Proto0, element(1, Data)),
    Offset = tuple_size(Data) - length(StructDef),
    Proto1 = struct_write_loop(Proto0, StructDef, Data, Offset + 1, Path),
    % Proto3 = impl_write_struct_end(Proto2),
    Proto1;
write_frag(_Proto, Type, Data, Path) ->
    throw({invalid, Path, Type, Data}).

struct_write_loop(Proto0, [{Fid, Req, Type, Name, _Default} | RestStructDef], Struct, Idx, Path) ->
    Data = element(Idx, Struct),
    NewProto =
        case Data of
            undefined when Req =:= required ->
                throw({invalid, [Name | Path], Type, Data});
            undefined ->
                % null fields are skipped in response
                Proto0;
            _ ->
                Proto1 = impl_write_field_begin(Proto0, Name, term_to_typeid(Type), Fid),
                Proto2 = write_frag(Proto1, Type, Data, [Name | Path]),
                % Proto3 = impl_write_field_end(Proto2),
                Proto2
        end,
    struct_write_loop(NewProto, RestStructDef, Struct, Idx + 1, Path);
struct_write_loop(Proto, [], _, _, _) ->
    impl_write_field_stop(Proto).

-spec set_size(ordsets:ordset(any())) ->
    non_neg_integer() | false.
%% Returns ordset cardinality unless ordset is malformed, otherwise returns `false`.
%% We knowingly abuse the inner structure of the `ordset()` type here.
set_size([E | Es]) -> set_size(Es, E, 1);
set_size([]) -> 0;
set_size(_) -> false.

set_size([E2 | Es], E1, S) when E1 < E2 -> set_size(Es, E2, S + 1);
set_size([_ | _], _, _) -> false;
set_size([], _, S) -> S.

-spec validate(tprot_header_val() | tprot_header_tag() | tprot_empty_tag() | field_stop | TypeData) ->
    ok | {error, {invalid, Location :: [atom()], Value :: term()}}
when
    TypeData :: {Type, Data},
    Type :: tprot_data_tag() | tprot_cont_tag() | {enum, _Def} | {struct, _Flavour, _Def},
    Data :: term().

validate(#protocol_message_begin{}) ->
    ok;
validate(#protocol_struct_begin{}) ->
    ok;
validate(#protocol_field_begin{}) ->
    ok;
validate(#protocol_map_begin{}) ->
    ok;
validate(#protocol_list_begin{}) ->
    ok;
validate(#protocol_set_begin{}) ->
    ok;
validate(message_end) ->
    ok;
validate(field_stop) ->
    ok;
validate(field_end) ->
    ok;
validate(struct_end) ->
    ok;
validate(list_end) ->
    ok;
validate(set_end) ->
    ok;
validate(map_end) ->
    ok;
validate({Type, Data}) ->
    try
        validate(required, Type, Data, [])
    catch
        throw:{invalid, Path, _Type, Value} ->
            {error, {invalid, lists:reverse(Path), Value}}
    end.

validate(Req, _Type, undefined, _Path) when
    Req =:= optional orelse Req =:= undefined
->
    ok;
validate(_Req, {list, Type}, Data, Path) when
    is_list(Data)
->
    lists:foreach(fun(E) -> validate(required, Type, E, Path) end, Data);
validate(_Req, {set, Type}, Data, Path) when
    is_list(Data)
->
    _ = ordsets:is_set(Data) orelse throw({invalid, Path, Type, Data}),
    lists:foreach(fun(E) -> validate(required, Type, E, Path) end, ordsets:to_list(Data));
validate(_Req, {map, KType, VType}, Data, Path) when
    is_map(Data)
->
    maps:fold(
        fun(K, V, _) ->
            validate(required, KType, K, Path),
            validate(required, VType, V, Path),
            ok
        end,
        ok,
        Data
    );
validate(Req, {struct, union, {Mod, Name}}, Data = {_, _}, Path) ->
    validate(Req, Mod:struct_info(Name), Data, Path);
validate(_Req, {struct, union, StructDef} = Type, Data = {Name, Value}, Path) when
    is_list(StructDef) andalso is_atom(Name)
->
    case lists:keyfind(Name, 4, StructDef) of
        {_, _, SubType, Name, _Default} ->
            validate(required, SubType, Value, [Name | Path]);
        false ->
            throw({invalid, Path, Type, Data})
    end;
validate(Req, {struct, _Flavour, {Mod, Name} = Type}, Data, Path) when
    is_tuple(Data)
->
    try Mod:record_name(Name) of
        RName when RName =:= element(1, Data) ->
            validate(Req, Mod:struct_info(Name), Data, Path);
        _ ->
            throw({invalid, Path, Type, Data})
    catch
        error:badarg ->
            throw({invalid, Path, Type, Data})
    end;
validate(_Req, {struct, _Flavour, StructDef}, Data, Path) when
    is_list(StructDef) andalso tuple_size(Data) =:= length(StructDef) + 1
->
    validate_struct_fields(StructDef, Data, 2, Path);
validate(_Req, {struct, _Flavour, StructDef}, Data, Path) when
    is_list(StructDef) andalso tuple_size(Data) =:= length(StructDef)
->
    validate_struct_fields(StructDef, Data, 1, Path);
validate(_Req, {enum, _Fields}, Value, _Path) when is_atom(Value), Value =/= undefined ->
    ok;
validate(_Req, string, Value, _Path) when is_binary(Value) ->
    ok;
validate(_Req, bool, Value, _Path) when is_boolean(Value) ->
    ok;
validate(_Req, byte, Value, _Path) when
    is_integer(Value), Value >= -(1 bsl 7), Value < (1 bsl 7)
->
    ok;
validate(_Req, i8, Value, _Path) when
    is_integer(Value), Value >= -(1 bsl 7), Value < (1 bsl 7)
->
    ok;
validate(_Req, i16, Value, _Path) when
    is_integer(Value), Value >= -(1 bsl 15), Value < (1 bsl 15)
->
    ok;
validate(_Req, i32, Value, _Path) when
    is_integer(Value), Value >= -(1 bsl 31), Value < (1 bsl 31)
->
    ok;
validate(_Req, i64, Value, _Path) when
    is_integer(Value), Value >= -(1 bsl 63), Value < (1 bsl 63)
->
    ok;
validate(_Req, double, Value, _Path) when is_float(Value) ->
    ok;
validate(_Req, Type, Value, Path) ->
    throw({invalid, Path, Type, Value}).

validate_struct_fields([{_, Req, Type, Name, _} | Types], Data, Idx, Path) ->
    _ = validate(Req, Type, element(Idx, Data), [Name | Path]),
    validate_struct_fields(Types, Data, Idx + 1, Path);
validate_struct_fields([], _Data, _Idx, _Path) ->
    ok.

%% Binary w/ strict read + write thrift protocol implementation.
%%
%% Inlined by hand for maximum efficiency, assuming the protocol is backed by
%% a membuffer transport. No-ops are commented out to help compiler optimize
%% more easily.

-define(VERSION_MASK, 16#FFFF0000).
-define(VERSION_1, 16#80010000).
-define(TYPE_MASK, 16#000000ff).

impl_write_message_begin(Trans0, Name, Type, Seqid) ->
    Trans1 = impl_write_i32(Trans0, ?VERSION_1 bor Type),
    Trans2 = impl_write_string(Trans1, Name),
    Trans3 = impl_write_i32(Trans2, Seqid),
    Trans3.

impl_write_field_begin(Trans0, _Name, Type, Id) ->
    Trans1 = impl_write_byte(Trans0, Type),
    Trans2 = impl_write_i16(Trans1, Id),
    Trans2.

impl_write_field_stop(Trans) ->
    impl_write_byte(Trans, ?tType_STOP).

% impl_write_field_end(Trans) -> Trans.

impl_write_map_begin(Trans0, Ktype, Vtype, Size) ->
    Trans1 = impl_write_byte(Trans0, Ktype),
    Trans2 = impl_write_byte(Trans1, Vtype),
    Trans3 = impl_write_i32(Trans2, Size),
    Trans3.

% impl_write_map_end(Trans) -> Trans.

impl_write_list_begin(Trans0, Etype, Size) ->
    Trans1 = impl_write_byte(Trans0, Etype),
    Trans2 = impl_write_i32(Trans1, Size),
    Trans2.

% impl_write_list_end(Trans) -> Trans.

impl_write_set_begin(Trans0, Etype, Size) ->
    Trans1 = impl_write_byte(Trans0, Etype),
    Trans2 = impl_write_i32(Trans1, Size),
    Trans2.

% impl_write_set_end(Trans) -> Trans.

% impl_write_struct_begin(Trans, _Name) -> Trans.
% impl_write_struct_end(Trans) -> Trans.

impl_write_bool(Trans, true) -> impl_write_byte(Trans, 1);
impl_write_bool(Trans, false) -> impl_write_byte(Trans, 0).

impl_write_byte(Trans, Byte) ->
    <<Trans/binary, Byte:8/big-signed>>.

impl_write_i16(Trans, I16) ->
    <<Trans/binary, I16:16/big-signed>>.

impl_write_i32(Trans, I32) ->
    <<Trans/binary, I32:32/big-signed>>.

impl_write_i64(Trans, I64) ->
    <<Trans/binary, I64:64/big-signed>>.

impl_write_double(Trans, Double) ->
    <<Trans/binary, Double:64/big-signed-float>>.

impl_write_string(Trans, Bin) ->
    <<Trans/binary, (byte_size(Bin)):32/big-signed, Bin/binary>>.

%%

impl_read_message_begin(<<?read_i32(Sz), This1/binary>>) ->
    impl_read_message_begin(This1, Sz).

impl_read_message_begin(This0, Sz) when Sz band ?VERSION_MASK =:= ?VERSION_1 ->
    %% we're at version 1
    {This1, Name} = impl_read_string(This0),
    <<?read_i32(SeqId), This2/binary>> = This1,
    {ok,
        #protocol_message_begin{
            name = Name,
            type = Sz band ?TYPE_MASK,
            seqid = SeqId
        },
        This2};
impl_read_message_begin(_This, Sz) when Sz < 0 ->
    %% there's a version number but it's unexpected
    {error, {bad_binary_protocol_version, Sz}};
impl_read_message_begin(_This, _) ->
    %% strict_read is true and there's no version header; that's an error
    {error, no_binary_protocol_version}.

% impl_read(This, message_end) -> {This, ok};

% impl_read(This, struct_begin) -> {This, ok};
% impl_read(This, struct_end) -> {This, ok};

% impl_read(This, field_end) -> {This, ok};

% impl_read(This, map_end) -> {This, ok};

% impl_read(This, list_end) -> {This, ok};

% impl_read(This, set_end) -> {This, ok};

impl_read_string(<<?read_i32(Sz), This/binary>>) ->
    read_data(This, Sz).

-spec read_data(buffer(), non_neg_integer()) ->
    {buffer(), binary()}.
read_data(This, 0) ->
    {This, <<>>};
read_data(This, Len) ->
    Give = min(byte_size(This), Len),
    <<Result:Give/binary, Remaining/binary>> = This,
    {Remaining, Result}.

%%

impl_transport_new(Buf) when is_list(Buf) ->
    iolist_to_binary(Buf);
impl_transport_new(Buf) when is_binary(Buf) ->
    Buf.
