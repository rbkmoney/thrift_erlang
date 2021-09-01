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

-module(thrift_client_codec).

-type service() :: {module(), atom()}.
-type fn() :: atom().
-type schema() :: {struct, _, _}.
-type args() :: tuple().
-type seqid() :: integer().

%% API
-export([write_function_call/6]).
-export([read_function_result/5]).

-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

-spec write_function_call(Buffer, module(), service(), fn(), args(), seqid()) ->
    {ok, Buffer} | {error, any()}.
write_function_call(Buffer, Codec, Service, Function, Args, SeqId) ->
    Schema = get_function_info(Service, Function, params_type),
    MsgType =
        case get_function_info(Service, Function, reply_type) of
            oneway_void -> ?tMessageType_ONEWAY;
            _ -> ?tMessageType_CALL
        end,
    FName = atom_to_binary(Function, latin1),
    Buffer1 = Codec:write_message_begin(Buffer, FName, MsgType, SeqId),
    write_message(Buffer1, Codec, Schema, Args).

-spec write_message(Buffer, module(), schema(), args()) ->
    {ok, Buffer} | {error, any()}.
write_message(Buffer, Codec, Schema, Args) ->
    case Codec:write(Buffer, Schema, Args) of
        {ok, Buffer1} ->
            {ok, Codec:write_message_end(Buffer1)};
        Error ->
            Error
    end.

-spec read_function_result(Buffer, module(), service(), fn(), seqid()) ->
    {ok, ok | {reply | exception, any()}, Buffer} | {error, any()}.
read_function_result(Buffer, Codec, Service, Function, SeqId) ->
    ReplyType = get_function_info(Service, Function, reply_type),
    read_result(Buffer, Codec, Service, Function, ReplyType, SeqId).

read_result(Buffer, _Codec, _Service, _Function, oneway_void, _SeqId) ->
    {ok, ok, Buffer};
read_result(Buffer, Codec, Service, Function, ReplyType, SeqId) ->
    case Codec:read_message_begin(Buffer) of
        {ok, MessageBegin, Buffer1} ->
            case MessageBegin of
                #protocol_message_begin{seqid = RetSeqId} when RetSeqId =/= SeqId ->
                    {error, {bad_seq_id, RetSeqId}};
                #protocol_message_begin{type = ?tMessageType_EXCEPTION} ->
                    read_application_exception(Buffer1, Codec);
                #protocol_message_begin{type = ?tMessageType_REPLY} ->
                    read_reply(Buffer1, Codec, Service, Function, ReplyType)
            end;
        Error ->
            Error
    end.

-define(REPLY_TAG, '$reply').

read_reply(Buffer, Codec, Service, Function, ReplyType) ->
    {struct, _, ExceptionFields} = get_function_info(Service, Function, exceptions),
    ReplyStructDef = {struct, union, [{0, undefined, ReplyType, ?REPLY_TAG, undefined}] ++ ExceptionFields},
    case Codec:read(Buffer, ReplyStructDef) of
        {ok, Reply, Buffer1} ->
            {ok, ok, Buffer2} = Codec:read_message_end(Buffer1),
            case Reply of
                {?REPLY_TAG, ReplyData} ->
                    {ok, {reply, ReplyData}, Buffer2};
                undefined when ReplyType == {struct, struct, []} ->
                    {ok, {reply, ok}, Buffer2};
                {_Name, Exception} ->
                    {ok, {exception, Exception}, Buffer2}
            end;
        Error ->
            Error
    end.

read_application_exception(Buffer, Codec) ->
    case Codec:read(Buffer, ?TApplicationException_Structure) of
        {ok, Exception, Buffer1} ->
            {ok, ok, Buffer2} = Codec:read_message_end(Buffer1),
            XRecord = erlang:insert_element(1, Exception, 'TApplicationException'),
            true = is_record(XRecord, 'TApplicationException'),
            {exception, XRecord, Buffer2};
        Error ->
            Error
    end.

get_function_info({Module, ServiceName}, Function, Info) ->
    Module:function_info(ServiceName, Function, Info).
