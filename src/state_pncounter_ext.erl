-module(state_pncounter_ext).
-define(TYPE, state_pncounter).

-export([
    delta_operations/2
]).

delta_operations({?TYPE, A}=PNCounter1, {?TYPE, B}=PNCounter2) ->
    case state_pncounter:is_strict_inflation(PNCounter1, PNCounter2) of
        true ->
            {I1, D1} = orddict:fold(fun(_, {I, D}, {Inc, Dec}) ->
                {I + Inc, D + Dec}
            end, {0, 0}, A),
            {I2, D2} = orddict:fold(fun(_, {I, D}, {Inc, Dec}) ->
                {I + Inc, D + Dec}
            end, {0, 0}, B),
            L = [{increment, I2 - I1}, {decrement, D2 - D1}],
            Predicate = fun({_, Value}) -> not (Value == 0) end,
            lists:filter(Predicate, L);
        false -> []
    end.