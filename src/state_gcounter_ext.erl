-module(state_gcounter_ext).
-define(TYPE, state_gcounter).

-export([
    sum/2,
    min/3,
    max/3
]).

-export([
    delta_operations/2
]).

sum({?TYPE, LValue}, {?TYPE, RValue}) ->
    {?TYPE, orddict:merge(
        fun(_, Value1, Value2) ->
            Value1 + Value2
        end,
        LValue,
        RValue
    )}.

max({?TYPE, _}=GCounter1, {?TYPE, _}=GCounter2, Actor) ->
    A = state_gcounter:query(GCounter1),
    B = state_gcounter:query(GCounter2),
    Max = erlang:max(A, B),
    {?TYPE, orddict:store(Actor, Max, orddict:new())}.

min({?TYPE, _}=GCounter1, {?TYPE, _}=GCounter2, Actor) ->
    A = state_gcounter:query(GCounter1),
    B = state_gcounter:query(GCounter2),
    Min = erlang:min(A, B),
    {?TYPE, orddict:store(Actor, Min, orddict:new())}.

delta_operations({?TYPE, A}=GCounter1, {?TYPE, B}=GCounter2) ->
    case state_gcounter:is_strict_inflation(GCounter1, GCounter2) of
        true ->
            I1 = orddict:fold(fun(_, Inc, Sum) ->
                Inc + Sum
            end, 0, A),
            I2 = orddict:fold(fun(_, Inc, Sum) ->
                Inc + Sum
            end, 0, B),
            [{increment, I2 - I1}];
        false -> []
    end.