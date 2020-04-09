-module(state_twopset_ext).
-define(TYPE, state_twopset).

-export([
    delta_operations/2
]).

delta_operations({?TYPE, A}=TwoPSet1, {?TYPE, B}=TwoPSet2) ->
    case state_twopset:is_strict_inflation(TwoPSet1, TwoPSet2) of
        true ->
            {AddSet1, RemoveSet1} = A,
            {AddSet2, RemoveSet2} = B,
            L1 = ordsets:to_list(ordsets:subtract(AddSet2, AddSet1)),
            L2 = ordsets:to_list(ordsets:subtract(RemoveSet2, RemoveSet1)),
            F1 = fun(Element) -> {add, Element} end,
            F2 = fun(Element) -> {rmv, Element} end,
            lists:map(F1, L1) ++ lists:map(F2, L2);
            % io:format("TwoPSet1=~p~n", [TwoPSet1]),
            % io:format("TwoPSet2=~p~n", [TwoPSet2]),
        false -> []
    end.
