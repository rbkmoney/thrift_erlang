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

-module(generic_codec_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("gen-erlang/thrift_test_thrift.hrl").

-define(SERVICE, {thrift_test_thrift, 'ThriftTest'}).
-define(CODEC, thrift_strict_binary_codec).
-define(SEQID, 42).

call_test_() ->
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({?CODEC, '_', '_'}, x),
    [
        ?_test(
            roundtrip(
                ?SERVICE,
                'testVoid',
                {},
                {reply, ok}
            )
        ),
        ?_test(
            roundtrip(
                ?SERVICE,
                'testString',
                {<<"thing">>},
                {reply, <<"thing">>}
            )
        ),
        ?_test(
            roundtrip(
                ?SERVICE,
                'testStruct',
                {#'Xtruct'{byte_thing = 127}},
                {reply, #'Xtruct'{i64_thing = (1 bsl 63) - 1}}
            )
        ),
        ?_test(
            roundtrip(
                ?SERVICE,
                'testMapMap',
                {-1337},
                {reply, #{1 => #{3 => 37}, 32767 => #{4 => 20}}}
            )
        ),
        ?_test(
            roundtrip(
                ?SERVICE,
                'testException',
                {<<"blarg">>},
                {exception, #'Xception'{errorCode = 1337}}
            )
        )
    ].

oneway_test_() ->
    [
        ?_test(
            roundtrip(
                ?SERVICE,
                oneway,
                'testOneway',
                {420},
                ok
            )
        )
    ].

error_test_() ->
    [
        ?_assertMatch(
            {error, {invalid, ['$reply'], _}},
            write_result(?SERVICE, 'testStruct', {reply, blarg})
        )
    ].

roundtrip(Service, Function, Args, Result) ->
    roundtrip(Service, call, Function, Args, Result).

roundtrip(Service, Type, Function, Args, Result) ->
    B0 = ?CODEC:new(),
    {ok, B1} = thrift_client_codec:write_function_call(B0, ?CODEC, Service, Function, Args, ?SEQID),
    ReadCallResult = thrift_processor_codec:read_function_call(B1, ?CODEC, Service),
    ?assertMatch({ok, ?SEQID, {Type, Function, Args}, _}, ReadCallResult),
    {ok, _, _, B2} = ReadCallResult,
    {ok, B3} = thrift_processor_codec:write_function_result(B2, ?CODEC, Service, Function, Result, ?SEQID),
    ReadReplyResult = thrift_client_codec:read_function_result(B3, ?CODEC, Service, Function, ?SEQID),
    ?assertMatch({ok, Result, _}, ReadReplyResult).

write_result(Service, Function, Result) ->
    B0 = ?CODEC:new(),
    thrift_processor_codec:write_function_result(B0, ?CODEC, Service, Function, Result, ?SEQID).
