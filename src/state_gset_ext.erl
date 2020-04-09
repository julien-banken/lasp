-module(state_gset_ext).
-define(TYPE, state_gset).

-export([
    delta_operations/2
]).

delta_operations({?TYPE, _}=GSet1, {?TYPE, _}=GSet2) ->
    case state_gset:is_strict_inflation(GSet1, GSet2) of
        true ->
            {_, A} = GSet1,
            {_, B} = GSet2,
            L = lists:subtract(B, A),
            F = fun(Element) -> {add, Element} end,
            lists:map(F, L);
        false -> []
    end.
