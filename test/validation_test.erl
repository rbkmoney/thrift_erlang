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

-module(validation_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("gen-erlang/thrift_test_thrift.hrl").

write_test_() ->
    list_test_cases(fun write/2).

write_codec_test_() ->
    list_test_cases(fun write_codec/2).

roundtrip_test_() ->
    [
        ?_test(roundtrip({struct, struct, {thrift_test_thrift, 'BoolTest'}}, #'BoolTest'{b = false})),
        ?_test(
            roundtrip(
                {struct, struct, {thrift_test_thrift, 'VersioningTestV2'}},
                #'VersioningTestV2'{newset = ordsets:from_list([3, 2, 1])}
            )
        )
    ].

list_test_cases(Writer) ->
    [
        ?_assertMatch(ok, Writer(byte, +42)),
        ?_assertMatch(ok, Writer(byte, -42)),
        ?_assertMatch({error, {invalid, [], _}}, Writer(byte, -200)),
        ?_assertMatch(ok, Writer(string, <<"1337">>)),
        ?_assertMatch({error, {invalid, [], _}}, Writer(string, "1337")),
        ?_assertMatch({error, {invalid, [], _}}, Writer(string, 1337)),
        ?_assertMatch(
            ok,
            Writer({struct, struct, {thrift_test_thrift, 'EmptyStruct'}}, #'EmptyStruct'{})
        ),
        ?_assertMatch(
            ok,
            Writer(
                {struct, struct, {thrift_test_thrift, 'VersioningTestV2'}},
                #'VersioningTestV2'{newset = ordsets:from_list([3, 2, 1])}
            )
        ),
        ?_assertMatch(
            {error, {invalid, [newset], _}},
            Writer(
                {struct, struct, {thrift_test_thrift, 'VersioningTestV2'}},
                #'VersioningTestV2'{newset = [3, 2, 1]}
            )
        ),
        ?_assertMatch(
            ok,
            Writer({struct, struct, {thrift_test_thrift, 'OneField'}}, #'OneField'{field = #'EmptyStruct'{}})
        ),
        ?_assertMatch(
            {error, {invalid, [field], _}},
            Writer({struct, struct, {thrift_test_thrift, 'OneField'}}, #'OneField'{field = #'Bools'{}})
        ),
        ?_assertMatch(
            ok,
            Writer({struct, union, {thrift1151_thrift, 'UnionA'}}, {a, {'StructA', 32767}})
        ),
        ?_assertMatch(
            {error, {invalid, [], _}},
            Writer({struct, union, {thrift1151_thrift, 'UnionA'}}, {d, {'StructD', 65535}})
        )
    ].

write(Type, Data) ->
    {ok, Trans} = thrift_membuffer_transport:new(),
    {ok, Proto} = thrift_binary_protocol:new(Trans, [{strict_read, true}, {strict_write, true}]),
    {_Proto, Result} = thrift_protocol:write(Proto, {Type, Data}),
    Result.

write_codec(Type, Data) ->
    Proto = thrift_strict_binary_codec:new(),
    case thrift_strict_binary_codec:write(Proto, Type, Data) of
        {ok, _} -> ok;
        Error -> Error
    end.

roundtrip(Type, Value) ->
    B0 = thrift_strict_binary_codec:new(),
    {ok, B1} = thrift_strict_binary_codec:write(B0, Type, Value),
    {ok, Value, B2} = thrift_strict_binary_codec:read(B1, Type),
    <<>> = thrift_strict_binary_codec:close(B2).
