%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 SyncFree Consortium.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(lasp_eleveldb_backend).
-author("Christopher Meiklejohn <cmeiklejohn@basho.com>").

-include("lasp.hrl").

-export([start/1,
         put/3,
         get/2]).

-behaviour(lasp_backend).

-define(OPEN_OPTS, [{create_if_missing, true}]).
-define(READ_OPTS, []).
-define(WRITE_OPTS, []).

%% @doc Initialize the backend.
-spec start(atom()) -> {ok, eleveldb:db_ref()} | {error, atom()}.
start(Identifier) ->
    %% Get the data root directory
    Config = app_helper:get_env(?APP),
    DataDir = filename:join(app_helper:get_prop_or_env(data_root, Config, eleveldb),
                            atom_to_list(Identifier)),

    %% Ensure directory.
    ok = filelib:ensure_dir(filename:join(DataDir, "dummy")),

    case eleveldb:open(DataDir, ?OPEN_OPTS) of
        {ok, Ref} ->
            {ok, Ref};
        {error, Reason} ->
            lager:info("Failed to open backend: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc Write a record to the backend.
-spec put(store(), id(), variable()) -> ok | {error, atom()}.
put(Store, Id, Record) ->
    StorageKey = encode(Id),
    StorageValue = encode(Record),
    % lager:info("Writing id: ~p from store: ~p; storagekey: ~p",
    %            [Id, Store, StorageKey]),
    Updates = [{put, StorageKey, StorageValue}],
    case eleveldb:write(Store, Updates, ?WRITE_OPTS) of
        ok ->
            ok;
        {error, Reason} ->
            lager:info("Error writing object: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc Retrieve a record from the backend.
-spec get(store(), id()) -> {ok, variable()} | {error, not_found} | {error, atom()}.
get(Store, Id) ->
    StorageKey = encode(Id),
    % lager:info("Retrieving id: ~p from store: ~p; storagekey: ~p",
    %            [Id, Store, StorageKey]),
    case eleveldb:get(Store, StorageKey, ?READ_OPTS) of
        {ok, Value} ->
            {ok, decode(Value)};
        not_found ->
            lager:info("Object not found!"),
            {error, not_found};
        {error, Reason} ->
            lager:info("Error reading object: ~p", [Reason]),
            {error, Reason}
    end.

encode(X) ->
    term_to_binary(X).

decode(X) ->
    binary_to_term(X).