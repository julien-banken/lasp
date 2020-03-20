-module(state_gcounter_ext).

-export([
    sum/2,
    min/3,
    max/3
]).

sum({state_gcounter, LValue}, {state_gcounter, RValue}) ->
    {state_gcounter, orddict:merge(
        fun(_, Value1, Value2) ->
            Value1 + Value2
        end,
        LValue,
        RValue
    )}.

max({state_gcounter, _}=GCounter1, {state_gcounter, _}=GCounter2, Actor) ->
    A = state_gcounter:query(GCounter1),
    B = state_gcounter:query(GCounter2),
    Max = erlang:max(A, B),
    {state_gcounter, orddict:store(Actor, Max, orddict:new())}.

min({state_gcounter, _}=GCounter1, {state_gcounter, _}=GCounter2, Actor) ->
    A = state_gcounter:query(GCounter1),
    B = state_gcounter:query(GCounter2),
    Min = erlang:min(A, B),
    {state_gcounter, orddict:store(Actor, Min, orddict:new())}.
