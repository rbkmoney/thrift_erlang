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

-module(thrift_processor_codec).

-export([read_function_call/3]).
-export([write_function_result/6]).

-export([match_exception/3]).

-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

-type service() :: {module(), atom()}.
-type fn() :: atom().
-type args() :: tuple().
-type seqid() :: integer().

-spec read_function_call(Buffer, module(), service()) ->
    {ok, seqid(), {call | oneway, fn(), args()}, Buffer} | {error, any()}.

read_function_call(Buffer, Codec, Service) ->
    case Codec:read_message_begin(Buffer) of
        {ok,
            #protocol_message_begin{
                name = FName,
                type = Type,
                seqid = SeqId
            },
            Buffer1} when Type =:= ?tMessageType_CALL; Type =:= ?tMessageType_ONEWAY ->
            try erlang:binary_to_existing_atom(FName, latin1) of
                Function -> read_function_params(Buffer1, Codec, Service, Function, Type, SeqId)
            catch
                error:badarg -> {error, {bad_function_name, FName}}
            end;
        {error, _} = Error ->
            Error
    end.

read_function_params(Buffer, Codec, Service, Function, IType, SeqId) ->
    InParams = get_function_info(Service, Function, params_type),
    Type =
        case IType of
            ?tMessageType_CALL -> call;
            ?tMessageType_ONEWAY -> oneway
        end,
    case Codec:read(Buffer, InParams) of
        {ok, Params, Buffer1} ->
            {ok, ok, Buffer2} = Codec:read_message_end(Buffer1),
            {ok, SeqId, {Type, Function, Params}, Buffer2};
        Error ->
            Error
    end.

-type exception_typename() ::
    {_Type, _Name :: atom()}.

-type result() ::
    ok
    | {reply, any()}
    | {exception, tuple()}
    | {exception, exception_typename(), tuple()}
    | {error, {_Class :: atom(), _Reason, _Stacktrace}}.

-spec write_function_result(Buffer, module(), service(), fn(), result(), seqid()) ->
    {ok, Buffer} | {error, any()}.

-define(REPLY_TAG, '$reply').

write_function_result(Buffer, Codec, Service, Function, Result, SeqId) ->
    ReplyType = get_function_info(Service, Function, reply_type),
    case Result of
        ok when ReplyType == {struct, struct, []} ->
            write_reply(Buffer, Codec, Function, ?tMessageType_REPLY, ReplyType, {?REPLY_TAG}, SeqId);
        {reply, ok} when ReplyType == {struct, struct, []} ->
            write_reply(Buffer, Codec, Function, ?tMessageType_REPLY, ReplyType, {?REPLY_TAG}, SeqId);
        {reply, ReplyData} ->
            ReplySchema = {struct, union, [{0, undefined, ReplyType, ?REPLY_TAG, undefined}]},
            Reply = {?REPLY_TAG, ReplyData},
            write_reply(Buffer, Codec, Function, ?tMessageType_REPLY, ReplySchema, Reply, SeqId);
        ok when ReplyType == oneway_void ->
            %% no reply for oneway void
            {ok, Buffer};
        {exception, Exception} ->
            write_exception(Buffer, Codec, Service, Function, Exception, SeqId);
        {exception, TypeName, Exception} ->
            write_exception(Buffer, Codec, Service, Function, TypeName, Exception, SeqId);
        {error, Error} ->
            write_error(Buffer, Codec, Function, Error, SeqId)
    end.

write_reply(Buffer, Codec, Function, ReplyMessageType, ReplySchema, Reply, SeqId) ->
    FName = atom_to_binary(Function, latin1),
    Buffer1 = Codec:write_message_begin(Buffer, FName, ReplyMessageType, SeqId),
    case Codec:write(Buffer1, ReplySchema, Reply) of
        {ok, Buffer2} ->
            Buffer3 = Codec:write_message_end(Buffer2),
            {ok, Buffer3};
        Error ->
            Error
    end.

write_exception(Buffer, Codec, Service, Function, Exception, SeqId) ->
    case match_exception(Service, Function, Exception) of
        {ok, TypeName} ->
            write_exception(Buffer, Codec, Service, Function, TypeName, Exception, SeqId);
        Error ->
            Error
    end.

write_exception(Buffer, Codec, Service, Function, {_Type, Name}, Exception, SeqId) ->
    {struct, _, XInfo} = get_function_info(Service, Function, exceptions),
    ReplySchema = {struct, union, XInfo},
    Reply = {Name, Exception},
    write_reply(Buffer, Codec, Function, ?tMessageType_REPLY, ReplySchema, Reply, SeqId).

-spec match_exception(service(), fn(), _Exception) ->
    {ok, exception_typename()}
    | {error, any()}.
match_exception(Service, Function, Exception) when is_tuple(Exception) ->
    ExceptionType = element(1, Exception),
    {struct, _, XInfo} = get_function_info(Service, Function, exceptions),
    %% Assuming we had a type1 exception, we'd get: [undefined, Exception, undefined]
    %% e.g.: [{-1, type0}, {-2, type1}, {-3, type2}]
    FindExcFun = fun({_Fid, _, {struct, exception, {Module, Type}}, _, _}) ->
        Module:record_name(Type) =/= ExceptionType
    end,
    case lists:dropwhile(FindExcFun, XInfo) of
        [{_Fid, _, Type, Name, _} | _] ->
            {ok, {Type, Name}};
        [] ->
            {error, bad_exception}
    end;
match_exception(_Service, _Function, _Exception) ->
    {error, bad_exception}.

write_error(Buffer, Codec, Function, Error, SeqId) ->
    Message = unicode:characters_to_binary(io_lib:format("An error occurred: ~0p~n", [Error])),
    ReplySchema = ?TApplicationException_Structure,
    Reply = #'TApplicationException'{
        message = Message,
        type = ?TApplicationException_UNKNOWN
    },
    write_reply(Buffer, Codec, Function, ?tMessageType_EXCEPTION, ReplySchema, Reply, SeqId).

get_function_info({Module, ServiceName}, Function, Info) ->
    Module:function_info(ServiceName, Function, Info).
