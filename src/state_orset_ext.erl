%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Christopher Meiklejohn.  All Rights Reserved.
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

-module(state_orset_ext).
-author("Christopher S. Meiklejohn <christopher.meiklejohn@gmail.com>").
-define(TYPE, state_orset).

-export([intersect/2,
         map/2,
         union/2,
         product/2,
         filter/2]).

-export([
    delta_operations/2
]).

union(LValue, RValue) ->
    state_orset:merge(LValue, RValue).

product({?TYPE, LValue}, {?TYPE, RValue}) ->
    FolderFun = fun({X, XCausality}, {?TYPE, Acc}) ->
        {?TYPE, Acc ++ [{{X, Y}, causal_product(XCausality, YCausality)} || {Y, YCausality} <- RValue]}
    end,
    lists:foldl(FolderFun, new(), LValue).

intersect({?TYPE, LValue}, RValue) ->
    lists:foldl(intersect_folder(RValue), new(), LValue).

%% @private
intersect_folder({?TYPE, RValue}) ->
    fun({X, XCausality}, {?TYPE, Acc}) ->
            Values = case lists:keyfind(X, 1, RValue) of
                         {_Y, YCausality} ->
                             [{X, causal_union(XCausality, YCausality)}];
                         false ->
                             []
                     end,
            {?TYPE, Acc ++ Values}
    end.

map(Function, {?TYPE, V}) ->
    FolderFun = fun({X, Causality}, {?TYPE, Acc}) ->
                        {?TYPE, Acc ++ [{Function(X), Causality}]}
                end,
    lists:foldl(FolderFun, new(), V).

filter(Function, {?TYPE, V}) ->
    FolderFun = fun({X, Causality}, {?TYPE, Acc}) ->
                        case Function(X) of
                            true ->
                                {?TYPE, Acc ++ [{X, Causality}]};
                            false ->
                                {?TYPE, Acc}
                        end
                end,
    lists:foldl(FolderFun, new(), V).

%% @private
new() ->
    state_orset:new().

%% @private
causal_product(Xs, Ys) ->
    lists:foldl(fun({X, XActive}, XAcc) ->
                lists:foldl(fun({Y, YActive}, YAcc) ->
                            [{[X, Y], XActive andalso YActive}] ++ YAcc
                    end, [], Ys) ++ XAcc
        end, [], Xs).

%% @private
causal_union(Xs, Ys) ->
        Xs ++ Ys.

%% @private
get_operations(Orddict, Value, Token, true) ->
    case orddict:find(Value, Orddict) of
        error -> [{add, Value}];
        {ok, Tokens} ->
            case orddict:find(Token, Tokens) of
                error -> [{add, Value}];
                {ok, true} -> [];
                {ok, false} ->
                    erlang:error("Not a strict inflation")
            end
    end;
get_operations(Orddict, Value, Token, false) ->
    case orddict:find(Value, Orddict) of
        error -> [{add, Value}, {rmv, Value}];
        {ok, Tokens} ->
            case orddict:find(Token, Tokens) of
                error -> [{add, Value}, {rmv, Value}];
                {ok, false} -> [];
                {ok, true} -> [{rmv, Value}]
            end
    end.

delta_operations({?TYPE, A}=ORSet1, {?TYPE, B}=ORSet2) ->
    case state_orset:is_strict_inflation(ORSet1, ORSet2) of
        true ->
            orddict:fold(fun(Value, Tokens, Acc1) ->
                Acc1 ++ orddict:fold(fun(Token, Flag, Acc2) ->
                    Acc2 ++ get_operations(A, Value, Token, Flag)
                end, [], Tokens)
            end, [], B);
        false -> []
    end.