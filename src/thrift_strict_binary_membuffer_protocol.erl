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

-module(thrift_strict_binary_membuffer_protocol).

-compile(inline).

-export([new/0,
         new/1,
         write/2,
         read/2,
         read/3,
         skip/2,
         validate/1,
         close/1
        ]).

-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

-opaque protocol() :: binary().

-export_type([protocol/0]).

-spec new() -> protocol().
new() ->
    new(<<>>).

-spec new(iodata()) -> protocol().
new(Buf) ->
    impl_transport_new(Buf).

-spec close(protocol()) -> binary().
close(Proto) ->
    Proto.

typeid_to_atom(?tType_STOP) -> field_stop;
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
typeid_to_atom(?tType_LIST) -> list.

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

%% Structure is like:
%%    [{Fid, Type}, ...]
-spec read(protocol(), {struct, _Flavour, _StructDef}, atom()) -> {protocol(), {ok, tuple()}}.
read(IProto0, {struct, union, StructDef}, _Tag)
  when is_list(StructDef) ->
    % {IProto1, ok} = read_frag(IProto0, struct_begin),
    {IProto1, RTuple} = read_union_loop(IProto0, enumerate(1, StructDef)),
    case RTuple of
      [{_, _} = Data] -> {IProto1, Data};
      [] ->              {IProto1, empty};
      [_ | _] ->         {IProto1, {multiple, RTuple}}
    end;
read(IProto0, {struct, _, StructDef}, Tag)
  when is_list(StructDef), is_atom(Tag) ->
    % {IProto1, ok} = read_frag(IProto0, struct_begin),
    {Offset, RTuple0} = construct_default_struct(Tag, StructDef),
    read_struct_loop(IProto0, enumerate(Offset + 1, StructDef), RTuple0).

construct_default_struct(Tag, StructDef) ->
    case Tag of
        undefined ->
            Tuple = erlang:make_tuple(length(StructDef), undefined),
            {0, fill_default_struct(1, StructDef, Tuple)};
        _ ->
            % If we want a tagged tuple, we need to offset all the tuple indices
            % by 1 to avoid overwriting the tag.
            Tuple = erlang:make_tuple(length(StructDef) + 1, undefined),
            {1, fill_default_struct(2, StructDef, erlang:setelement(1, Tuple, Tag))}
    end.

fill_default_struct(_N, [], Record) ->
    Record;
fill_default_struct(N, [{_Fid, _Req, _Type, _Name, Default} | Rest], Record) ->
    fill_default_struct(N + 1, Rest, erlang:setelement(N, Record, Default)).

enumerate(N, [{Fid, _Req, Type, Name, _Default} | Rest]) ->
    [{N, Fid, Type, Name} | enumerate(N + 1, Rest)];
enumerate(_, []) ->
    [].

%% NOTE: Keep this in sync with thrift_protocol_behaviour:read
-spec read
        (protocol(), {struct, _Flavour, _Info}) ->
            {ok, tuple(), protocol()} | {error, _Reason};
        (protocol(), tprot_cont_tag()) ->
            {ok, any(), protocol()} | {error, _Reason};
        (protocol(), tprot_empty_tag()) ->
            {ok, ok, protocol()} | {error, _Reason};
        (protocol(), tprot_header_tag()) ->
            {ok, tprot_header_val(), protocol()} | {error, _Reason};
        (protocol(), tprot_data_tag()) ->
            {ok, any(), protocol()} | {error, _Reason}.

read(IProto, Type) ->
    try read_frag(IProto, Type) of
        {IProto2, Data} ->
            case validate({Type, Data}) of
                ok    -> {ok, Data, IProto2};
                Error -> Error
            end
    catch
        throw:Reason ->
            {error, Reason}
    end.

read_frag(IProto, {struct, union, {Module, StructureName}}) when
  is_atom(Module), is_atom(StructureName) ->
    read(IProto, Module:struct_info(StructureName), undefined);
read_frag(IProto, {struct, _, {Module, StructureName}}) when
  is_atom(Module), is_atom(StructureName) ->
    read(IProto, Module:struct_info(StructureName), Module:record_name(StructureName));

read_frag(IProto, S = {struct, _, Structure}) when is_list(Structure) ->
    read(IProto, S, undefined);

read_frag(IProto, {enum, {Module, EnumName}}) when is_atom(Module) ->
    read_frag(IProto, Module:enum_info(EnumName));

read_frag(IProto, {enum, Fields}) when is_list(Fields) ->
    {IProto2, IVal} = impl_read_i32(IProto),
    {EnumVal, IVal} = lists:keyfind(IVal, 2, Fields),
    {IProto2, EnumVal};

read_frag(IProto0, {list, Type}) ->
    {IProto1, #protocol_list_begin{etype = EType, size = Size}} = impl_read_list_begin(IProto0),
    {EType, EType} = {term_to_typeid(Type), EType},
    read_list_loop(IProto1, Type, Size);
    % {IProto3, ok} = read_frag(IProto2, list_end),

read_frag(IProto0, {map, KeyType, ValType}) ->
    {IProto1, #protocol_map_begin{size = Size, ktype = KType, vtype = VType}} = impl_read_map_begin(IProto0),
    _ = case Size of
      0 -> 0;
      _ ->
        {KType, KType} = {term_to_typeid(KeyType), KType},
        {VType, VType} = {term_to_typeid(ValType), VType}
    end,
    read_map_loop(IProto1, KeyType, ValType, Size);
    % {IProto3, ok} = read_frag(IProto2, map_end),

read_frag(IProto0, {set, Type}) ->
    {IProto1, #protocol_set_begin{etype = EType, size = Size}} = impl_read_set_begin(IProto0),
    {EType, EType} = {term_to_typeid(Type), EType},
    read_set_loop(IProto1, Type, Size);
    % {IProto3, ok} = read_frag(IProto2, set_end),

read_frag(Proto, bool) ->
    impl_read_bool(Proto);
read_frag(Proto, byte) ->
    impl_read_byte(Proto);
read_frag(Proto, i16) ->
    impl_read_i16(Proto);
read_frag(Proto, i32) ->
    impl_read_i32(Proto);
read_frag(Proto, i64) ->
    impl_read_i64(Proto);
read_frag(Proto, double) ->
    impl_read_double(Proto);
read_frag(Proto, string) ->
    impl_read_string(Proto).

-spec read_list_loop(protocol(), any(), non_neg_integer()) -> {protocol(), [any()]}.
read_list_loop(Proto0, ValType, Size) ->
    read_list_loop(Proto0, ValType, Size, []).

read_list_loop(Proto0, _ValType, 0, List) ->
    {Proto0, lists:reverse(List)};
read_list_loop(Proto0, ValType, Left, List) ->
    {Proto1, Val} = read_frag(Proto0, ValType),
    read_list_loop(Proto1, ValType, Left - 1, [Val | List]).

-spec read_map_loop(protocol(), any(), any(), non_neg_integer()) -> {protocol(), map()}.
read_map_loop(Proto0, KeyType, ValType, Size) ->
    read_map_loop(Proto0, KeyType, ValType, Size, #{}).

read_map_loop(Proto0, _KeyType, _ValType, 0, Map) ->
    {Proto0, Map};
read_map_loop(Proto0, KeyType, ValType, Left, Map) ->
    {Proto1, Key} = read_frag(Proto0, KeyType),
    {Proto2, Val} = read_frag(Proto1, ValType),
    read_map_loop(Proto2, KeyType, ValType, Left - 1, maps:put(Key, Val, Map)).

-spec read_set_loop(protocol(), any(), non_neg_integer()) -> {protocol(), ordsets:ordset(any())}.
read_set_loop(Proto0, ValType, Size) ->
    read_set_loop(Proto0, ValType, Size, ordsets:new()).

read_set_loop(Proto0, _ValType, 0, Set) ->
    {Proto0, Set};
read_set_loop(Proto0, ValType, Left, Set) ->
    {Proto1, Val} = read_frag(Proto0, ValType),
    read_set_loop(Proto1, ValType, Left - 1, ordsets:add_element(Val, Set)).

read_struct_loop(IProto0, StructIndex, RTuple) ->
  read_struct_loop(
    IProto0, StructIndex,
    fun ({N, _, Val}, Acc) -> setelement(N, Acc, Val) end,
    RTuple
  ).

read_union_loop(IProto0, StructIndex) ->
  read_struct_loop(
    IProto0, StructIndex,
    fun ({_, Name, Val}, Was) -> [{Name, Val} | Was] end,
    []
  ).

read_struct_loop(IProto0, StructIndex, Fun, Acc) ->
    {IProto1, #protocol_field_begin{type = FType, id = Fid}} = impl_read_field_begin(IProto0),
    case FType of
        ?tType_STOP ->
            % {IProto2, ok} = read_frag(IProto1, struct_end),
            {IProto1, Acc};
        _Else ->
            case lists:keyfind(Fid, 2, StructIndex) of
                {N, Fid, Type, Name} ->
                    case term_to_typeid(Type) of
                        FType ->
                            {IProto2, Val} = read_frag(IProto1, Type),
                            % {IProto3, ok} = read_frag(IProto2, field_end),
                            NewAcc = Fun({N, Name, Val}, Acc),
                            read_struct_loop(IProto2, StructIndex, Fun, NewAcc);
                        _Expected ->
                            error_logger:info_msg(
                                "Skipping field ~p with wrong type: ~p~n",
                                [Name, typeid_to_atom(FType)]),
                            skip_field(FType, IProto1, StructIndex, Acc)
                    end;
                false ->
                    error_logger:info_msg(
                        "Skipping unknown field [~p] with type: ~p~n",
                        [Fid, typeid_to_atom(FType)]),
                    skip_field(FType, IProto1, StructIndex, Acc)
            end
    end.

skip_field(FType, IProto0, StructIndex, Acc) ->
    FTypeAtom = typeid_to_atom(FType),
    {IProto1, ok} = skip(IProto0, FTypeAtom),
    % {IProto2, ok} = read_frag(IProto1, field_end),
    read_struct_loop(IProto1, StructIndex, Acc).

-spec skip(protocol(), any()) -> {protocol(), ok}.

skip(Proto0, struct) ->
    % Proto1 = read_frag(Proto0, struct_begin),
    Proto1 = skip_struct_loop(Proto0),
    % Proto3 = read_frag(Proto2, struct_end),
    Proto1;

skip(Proto0, map) ->
    {Proto1, Map} = impl_read_map_begin(Proto0),
    Proto2 = skip_map_loop(Proto1, Map),
    % Proto3 = read_frag(Proto2, map_end),
    Proto2;

skip(Proto0, set) ->
    {Proto1, Set} = impl_read_set_begin(Proto0),
    Proto2 = skip_set_loop(Proto1, Set),
    % Proto3 = read_frag(Proto2, set_end),
    Proto2;

skip(Proto0, list) ->
    {Proto1, List} = impl_read_list_begin(Proto0),
    Proto2 = skip_list_loop(Proto1, List),
    % Proto3 = read_frag(Proto2, list_end),
    Proto2;

skip(Proto0, Type) when is_atom(Type) ->
    {Proto1, _Ignore} = read_frag(Proto0, Type),
    Proto1;

skip(Proto0, Type) when is_integer(Type) ->
    skip(Proto0, typeid_to_atom(Type)).

skip_struct_loop(Proto0) ->
    {Proto1, #protocol_field_begin{type = Type}} = impl_read_field_begin(Proto0),
    case Type of
        ?tType_STOP ->
            Proto1;
        _Else ->
            Proto2 = skip(Proto1, Type),
            % Proto3, ok} = read(Proto2, field_end),
            skip_struct_loop(Proto2)
    end.

skip_map_loop(Proto0, #protocol_map_begin{size = 0}) ->
    Proto0;
skip_map_loop(Proto0, Map = #protocol_map_begin{ktype = Ktype, vtype = Vtype, size = Size}) ->
    Proto1 = skip(Proto0, Ktype),
    Proto2 = skip(Proto1, Vtype),
    skip_map_loop(Proto2, Map#protocol_map_begin{size = Size - 1}).

skip_set_loop(Proto0, #protocol_set_begin{size = 0}) ->
    Proto0;
skip_set_loop(Proto0, Map = #protocol_set_begin{etype = Etype, size = Size}) ->
    Proto1 = skip(Proto0, Etype),
    skip_set_loop(Proto1, Map#protocol_set_begin{size = Size - 1}).

skip_list_loop(Proto0, #protocol_list_begin{size = 0}) ->
    Proto0;
skip_list_loop(Proto0, Map = #protocol_list_begin{etype = Etype, size = Size}) ->
    Proto1 = skip(Proto0, Etype),
    skip_list_loop(Proto1, Map#protocol_list_begin{size = Size - 1}).

%%

-spec write(protocol(), any()) -> {ok, protocol()} | {error, _Reason}.

write(Proto, #protocol_message_begin{name = Name, type = Type, seqid = SeqId}) ->
    {ok, impl_write_message_begin(Proto, Name, Type, SeqId)};
write(Proto, message_end) ->
    {ok, impl_write_message_end(Proto)};

write(Proto, TypeData) ->
    write_safe(Proto, TypeData).

write_safe(Proto, {Type, Data}) ->
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
    catch error:badarg ->
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

write_frag(Proto0, {list, Type}, Data, Path)
  when is_list(Data) ->
    Proto1 = impl_write_list_begin(Proto0, term_to_typeid(Type), length(Data)),
    Proto2 = lists:foldl(fun(Elem, ProtoIn) ->
                            write_frag(ProtoIn, Type, Elem, Path)
                         end,
                         Proto1,
                         Data),
    % Proto3 = impl_write_list_end(Proto2),
    Proto2;

write_frag(Proto0, {map, KeyType, ValType}, Data, Path)
  when is_map(Data) ->
    Proto1 = impl_write_map_begin(Proto0, term_to_typeid(KeyType), term_to_typeid(ValType), map_size(Data)),
    Proto2 = maps:fold(fun(KeyData, ValData, ProtoS0) ->
                               ProtoS1 = write_frag(ProtoS0, KeyType, KeyData, Path),
                               ProtoS2 = write_frag(ProtoS1, ValType, ValData, Path),
                               ProtoS2
                       end,
                       Proto1,
                       Data),
    % Proto3 = impl_write_map_end(Proto2),
    Proto2;

write_frag(Proto0, {set, Type}, Data, Path)
  when is_list(Data) ->
    Proto1 = impl_write_set_begin(Proto0, term_to_typeid(Type), ordsets:size(Data)),
    Proto2 = ordsets:fold(fun(Elem, ProtoIn) ->
                            write_frag(ProtoIn, Type, Elem, Path)
                       end,
                       Proto1,
                       Data),
    % Proto3 = impl_write_set_end(Proto2),
    Proto2;

write_frag(Proto0, string, Data, _)
  when is_binary(Data) ->
    impl_write_string(Proto0, Data);
write_frag(Proto0, i64, Data, _)
  when is_integer(Data), Data >= -(1 bsl 63), Data < (1 bsl 63) ->
    impl_write_i64(Proto0, Data);
write_frag(Proto0, i32, Data, _)
  when is_integer(Data), Data >= -(1 bsl 31), Data < (1 bsl 31) ->
    impl_write_i32(Proto0, Data);
write_frag(Proto0, i16, Data, _)
  when is_integer(Data), Data >= -(1 bsl 15), Data < (1 bsl 15) ->
    impl_write_i16(Proto0, Data);
write_frag(Proto0, byte, Data, _)
  when is_integer(Data), Data >= -(1 bsl 7), Data < (1 bsl 7) ->
    impl_write_byte(Proto0, Data);
write_frag(Proto0, double, Data, _)
  when is_float(Data) ->
    impl_write_double(Proto0, Data);
write_frag(Proto0, bool, Data, _)
  when is_boolean(Data) ->
    impl_write_bool(Proto0, Data);

write_frag(Proto0, {struct, union, StructDef}, Data, Path) ->
    write_union(Proto0, StructDef, Data, Path);

write_frag(Proto0, {struct, _, StructDef}, Data, Path) ->
    % Proto1 = impl_write_struct_begin(Proto0, element(1, Data)),
    Proto1 = struct_write_loop(Proto0, StructDef, Data, 2, Path),
    % Proto3 = impl_write_struct_end(Proto2),
    Proto1;

write_frag(_Proto, Type, Data, Path) ->
    throw({invalid, Path, Type, Data}).

struct_write_loop(Proto0, [{Fid, Req, Type, Name, _Default} | RestStructDef], Struct, Idx, Path) ->
    Data = element(Idx, Struct),
    NewProto = case Data of
                   undefined when Req =:= required ->
                       throw({invalid, [Name | Path], Type, Data});
                   undefined ->
                       Proto0; % null fields are skipped in response
                   _ ->
                       Proto1 = impl_write_field_begin(Proto0, Name, term_to_typeid(Type), Fid),
                       Proto2 = write_frag(Proto1, Type, Data, [Name | Path]),
                       % Proto3 = impl_write_field_end(Proto2),
                       Proto2
               end,
    struct_write_loop(NewProto, RestStructDef, Struct, Idx + 1, Path);
struct_write_loop(Proto, [], _, _, _) ->
    impl_write_field_stop(Proto).

-spec validate(tprot_header_val() | tprot_header_tag() | tprot_empty_tag() | field_stop | TypeData) ->
    ok | {error, {invalid, Location :: [atom()], Value :: term()}} when
        TypeData :: {Type, Data},
        Type :: tprot_data_tag() | tprot_cont_tag() | {enum, _Def} | {struct, _Flavour, _Def},
        Data :: term().

validate(#protocol_message_begin{}) -> ok;
validate(#protocol_struct_begin{}) -> ok;
validate(#protocol_field_begin{}) -> ok;
validate(#protocol_map_begin{}) -> ok;
validate(#protocol_list_begin{}) -> ok;
validate(#protocol_set_begin{}) -> ok;
validate(message_end) -> ok;
validate(field_stop) -> ok;
validate(field_end) -> ok;
validate(struct_end) -> ok;
validate(list_end) -> ok;
validate(set_end) -> ok;
validate(map_end) -> ok;

validate({Type, Data}) ->
    try validate(required, Type, Data, []) catch
        throw:{invalid, Path, _Type, Value} ->
            {error, {invalid, lists:reverse(Path), Value}}
    end.

validate(Req, _Type, undefined, _Path)
  when Req =:= optional orelse Req =:= undefined ->
    ok;
validate(_Req, {list, Type}, Data, Path)
  when is_list(Data) ->
    lists:foreach(fun (E) -> validate(required, Type, E, Path) end, Data);
validate(_Req, {set, Type}, Data, Path)
  when is_list(Data) ->
    lists:foreach(fun (E) -> validate(required, Type, E, Path) end, ordsets:to_list(Data));
validate(_Req, {map, KType, VType}, Data, Path)
  when is_map(Data) ->
    maps:fold(fun (K, V, _) ->
        validate(required, KType, K, Path),
        validate(required, VType, V, Path),
        ok
    end, ok, Data);
validate(Req, {struct, union, {Mod, Name}}, Data = {_, _}, Path) ->
    validate(Req, Mod:struct_info(Name), Data, Path);
validate(_Req, {struct, union, StructDef} = Type, Data = {Name, Value}, Path)
  when is_list(StructDef) andalso is_atom(Name) ->
    case lists:keyfind(Name, 4, StructDef) of
        {_, _, SubType, Name, _Default} ->
            validate(required, SubType, Value, [Name | Path]);
        false ->
            throw({invalid, Path, Type, Data})
    end;
validate(Req, {struct, _Flavour, {Mod, Name} = Type}, Data, Path)
  when is_tuple(Data) ->
    try Mod:record_name(Name) of
      RName when RName =:= element(1, Data) ->
        validate(Req, Mod:struct_info(Name), Data, Path);
      _ ->
        throw({invalid, Path, Type, Data})
    catch error:badarg ->
        throw({invalid, Path, Type, Data})
    end;
validate(_Req, {struct, _Flavour, StructDef}, Data, Path)
  when is_list(StructDef) andalso tuple_size(Data) =:= length(StructDef) + 1 ->
    validate_struct_fields(StructDef, Data, 2, Path);
validate(_Req, {struct, _Flavour, StructDef}, Data, Path)
  when is_list(StructDef) andalso tuple_size(Data) =:= length(StructDef) ->
    validate_struct_fields(StructDef, Data, 1, Path);
validate(_Req, {enum, _Fields}, Value, _Path) when is_atom(Value), Value =/= undefined ->
    ok;
validate(_Req, string, Value, _Path) when is_binary(Value) ->
    ok;
validate(_Req, bool, Value, _Path) when is_boolean(Value) ->
    ok;
validate(_Req, byte, Value, _Path)
  when is_integer(Value), Value >= -(1 bsl 7), Value < (1 bsl 7) ->
    ok;
validate(_Req, i8,  Value, _Path)
  when is_integer(Value), Value >= -(1 bsl 7), Value < (1 bsl 7) ->
    ok;
validate(_Req, i16, Value, _Path)
  when is_integer(Value), Value >= -(1 bsl 15), Value < (1 bsl 15) ->
    ok;
validate(_Req, i32, Value, _Path)
  when is_integer(Value), Value >= -(1 bsl 31), Value < (1 bsl 31) ->
    ok;
validate(_Req, i64, Value, _Path)
  when is_integer(Value), Value >= -(1 bsl 63), Value < (1 bsl 63) ->
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

impl_write_message_end(Trans) -> Trans.

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

impl_write_bool(Trans, true)  -> impl_write_byte(Trans, 1);
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

-define(read_byte(V), V:8/integer-signed-big).
-define(read_i16(V), V:16/integer-signed-big).
-define(read_i32(V), V:32/integer-signed-big).
-define(read_i64(V), V:64/integer-signed-big).
-define(read_double(V), V:64/float-signed-big).

impl_read_message_begin(<<?read_i32(Sz), This1/binary>>) ->
    impl_read_message_begin(This1, Sz).

impl_read_message_begin(This0, Sz) when Sz band ?VERSION_MASK =:= ?VERSION_1 ->
    %% we're at version 1
    {<<?read_i32(SeqId), This1/binary>>, Name} = impl_read_string(This0),
    {This1, #protocol_message_begin{name  = Name,
                                    type  = Sz band ?TYPE_MASK,
                                    seqid = SeqId}};
impl_read_message_begin(_This, Sz) when Sz < 0 ->
    %% there's a version number but it's unexpected
    throw({bad_binary_protocol_version, Sz});
impl_read_message_begin(_This, _) ->
    %% strict_read is true and there's no version header; that's an error
    throw(no_binary_protocol_version).

% impl_read(This, message_end) -> {This, ok};

% impl_read(This, struct_begin) -> {This, ok};
% impl_read(This, struct_end) -> {This, ok};

impl_read_field_begin(<<?read_byte(?tType_STOP), This/binary>>) ->
    {This, #protocol_field_begin{type = ?tType_STOP}};
impl_read_field_begin(<<?read_byte(Type), ?read_i16(Id), This/binary>>) ->
    {This, #protocol_field_begin{type = Type, id = Id}}.

% impl_read(This, field_end) -> {This, ok};

impl_read_map_begin(<<?read_byte(Ktype), ?read_byte(Vtype), ?read_i32(Size), This/binary>>) ->
    {This, #protocol_map_begin{ktype = Ktype, vtype = Vtype, size = Size}}.

% impl_read(This, map_end) -> {This, ok};

impl_read_list_begin(<<?read_byte(Etype), ?read_i32(Size), This/binary>>) ->
    {This, #protocol_list_begin{etype = Etype, size = Size}}.

% impl_read(This, list_end) -> {This, ok};

impl_read_set_begin(<<?read_byte(Etype), ?read_i32(Size), This/binary>>) ->
    {This, #protocol_set_begin{etype = Etype, size = Size}}.

% impl_read(This, set_end) -> {This, ok};

impl_read_field_stop(<<?read_byte(?tType_STOP), This/binary>>) ->
    This.

impl_read_bool(<<?read_byte(Byte), This/binary>>) ->
    {This, Byte /= 0}.

impl_read_byte(<<?read_byte(Val), This/binary>>) ->
    {This, Val}.

impl_read_i16(<<?read_i16(Val), This/binary>>) ->
    {This, Val}.

impl_read_i32(<<?read_i32(Val), This/binary>>) ->
    {This, Val}.

impl_read_i64(<<?read_i64(Val), This/binary>>) ->
    {This, Val}.

impl_read_double(<<?read_double(Val), This/binary>>) ->
    {This, Val}.

impl_read_string(<<?read_i32(Sz), This/binary>>) ->
    read_data(This, Sz).

-spec read_data(protocol(), non_neg_integer()) ->
    {protocol(), binary()}.
read_data(This, 0) ->
    {This, <<>>};
read_data(This, Len) when is_integer(Len) andalso Len > 0 ->
    impl_transport_read(This, Len).

%%

impl_transport_new(Buf) when is_list(Buf) ->
  iolist_to_binary(Buf);
impl_transport_new(Buf) when is_binary(Buf) ->
  Buf.

impl_transport_read(State, Len) ->
  Give = min(byte_size(State), Len),
  {Result, Remaining} = split_binary(State, Give),
  {Remaining, Result}.