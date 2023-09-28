-module(test_prop).
-export([
    test_prop_all/0,
    mkstrategy/1, 
    prop_value_preservation/0,
    prop_total_trades/0,
    prop_aggregate_api_call/0
]).

-include_lib("eqc/include/eqc.hrl").

tests() ->
    [
        prop_value_preservation(),
        prop_total_trades(),
        prop_aggregate_api_call()
    ].

test_prop_all() ->
    Tests = [eqc:quickcheck(T) || T <- tests()],
    AllPassed = lists:all(fun(X) -> X =:= true end, Tests),
    if 
        AllPassed ->
            io:format("All ~p property tests passed.~n", [length(Tests)]);
        true ->
            NumOfFailures = length(lists:filter(fun(X) -> X =/= true end, Tests)),
            io:format("~p property test(s) failed.~n", [NumOfFailures])
    end.

% ------------------------ helper functions ------------------------

% helper function that generates a terminating deterministic 
% trader strategy function from the argument Opr.
mkstrategy(buy_everything) -> 
    fun(_)-> accept end;

mkstrategy({buy_cheap, Price}) -> 
    fun({_, StockPirce})->
        case StockPirce =< Price of
            true -> accept;
            false -> reject
        end
    end;

mkstrategy({buy_only, StockNames}) -> 
    fun({Name, _})->
        case lists:member(Name, StockNames) of
            true -> accept;
            false -> reject
        end
    end;

mkstrategy({all, Oprs}) ->
    fun(Stock)->
        Strategies = [mkstrategy(Opr) || Opr <- Oprs],
        Res = [Strategy(Stock) || Strategy <- Strategies],
        case lists:member(reject, Res) of
            true -> reject;
            false -> accept
        end
    end;

mkstrategy({either, Oprs}) ->
    fun(Stock)->
        Strategies = [mkstrategy(Opr) || Opr <- Oprs],
        Res = [Strategy(Stock) || Strategy <- Strategies],
        case lists:member(accept, Res) of
            true -> accept;
            false -> reject
        end
    end.

available_stocks() ->
    [
        haskell_ltd, 
        boa_llc, 
        erlang_inc, 
        elixir_ltd
    ].

total_money(Hoddings) ->
    lists:sum([Balance || {Balance, _} <- Hoddings]).
total_money_api(Accts) ->
    Holdings = [erlst:account_balance(Acct) || Acct <- Accts],
    total_money(Holdings).

total_stocks(Hoddings) ->
    Stocks = #{},
    lists:foldl(
    fun({_, HStocks}, Acc)->
        lists:foldl(
        fun({Name, Amount}, Acc_)->
            case maps:is_key(Name, Acc_) of
                true -> maps:update_with(Name, fun(X)-> X + Amount end, Acc_);
                false -> maps:put(Name, Amount, Acc_)
            end
        end, Acc, HStocks)
    end, Stocks, Hoddings).

total_stocks_api(Accts) ->
    Holdings = [erlst:account_balance(Acct) || Acct <- Accts],
    total_stocks(Holdings).

pick_random(E) ->
    lists:nth(rand:uniform(length(E)), E).

execute_sym_commands(_, [], _, _, Executed) -> Executed;
execute_sym_commands(S, SymCmds, RescindOfferArgs, RemoveTraderArgs, Executed) ->
    [Cmd | Rest] = SymCmds,
    case Cmd of
        {call, erlst, make_offer, [Acct | _Offer]} ->
            {ok, OfferId} = eval(Cmd),
            execute_sym_commands(S, Rest, 
                [{Acct, OfferId} | RescindOfferArgs],
                RemoveTraderArgs,
                [Cmd | Executed]);
        {call, erlst, rescind_offer, _} ->
            case RescindOfferArgs of
                [] -> 
                    execute_sym_commands(S, Rest, 
                        RescindOfferArgs,
                        RemoveTraderArgs, 
                        Executed);
                _ ->
                    {Acct, OfferId}=  pick_random(RescindOfferArgs),
                    NewCmd = {call, erlst, rescind_offer, [Acct, OfferId]},
                    eval(NewCmd),
                    execute_sym_commands(S, Rest, 
                        lists:delete({Acct, OfferId}, RescindOfferArgs),
                        RemoveTraderArgs,
                        [NewCmd | Executed])  
            end;
        {call, erlst, add_trader, [Acct | _Strategy]} ->
            TraderId = eval(Cmd),
            execute_sym_commands(S, Rest, 
                RescindOfferArgs,
                [{Acct, TraderId} | RemoveTraderArgs],
                [Cmd | Executed]);
        {call, erlst, remove_trader, _} ->
            case RemoveTraderArgs of
                [] ->
                    execute_sym_commands(S, Rest, 
                        RescindOfferArgs,
                        RemoveTraderArgs,
                        Executed);
                _ ->
                    {Acct, TraderId} = pick_random(RemoveTraderArgs),
                    NewCmd = {call, erlst, remove_trader, [Acct, TraderId]},
                    eval(NewCmd),
                    execute_sym_commands(S, Rest, 
                        RescindOfferArgs,
                        lists:delete({Acct, TraderId}, RemoveTraderArgs),
                        [NewCmd | Executed])
            end;
        _ ->
            eval(Cmd),
            execute_sym_commands(S, Rest, 
                RescindOfferArgs,
                RemoveTraderArgs,
                [Cmd | Executed])
    end.

% ------------------------ generators ------------------------
stock() -> oneof(available_stocks()).

isk() -> nat().
stock_amount() -> ?SUCHTHAT(X, nat(), X > 0).
stocks() ->
    ?LET(Stocks, map(stock(), stock_amount()), 
        maps:to_list(Stocks)
    ).

offer() ->
    ?LET({Price, Stock}, {isk(), stock()}, 
        {Stock, Price}
    ).

holdings() ->
    ?LET(Balance, isk(),
    ?LET(Stocks, stocks(),
        {Balance, Stocks}
    )).

nonempty_list_of(G) ->
    ?SUCHTHAT(X, list(G), X =/= []).

nonempty_stock_names() ->
    ?SUCHTHAT(X, sublist(available_stocks()), X =/= []).

opr() ->
    ?LAZY(
        frequency([
            {2, buy_everything},
            {3, ?LET(Price, isk(), {buy_cheap, Price})},
            {3, ?LET(StockNames, nonempty_stock_names(),
                {buy_only, StockNames})},
            {1, ?LET(Opr1, opr(), 
                ?LET(Opr2, opr(), {all, [Opr1, Opr2]}))},
            {1, ?LET(Opr1, opr(), 
                ?LET(Opr2, opr(), {either, [Opr1, Opr2]}))}
        ])
    ).


% A QuickCheck generator that generates a symbolic call which
% can be evaluated to a trader strategy function.
reliable_strategy() ->
    ?LET(Opr, opr(), 
    {call, test_prop, mkstrategy, [Opr]}).

api_call(Accts) ->
    ?LET(Acct, oneof(Accts), 
        frequency([
            {1, {call, erlst, account_balance, [Acct]}},
            {3, {call, erlst, make_offer, [Acct, offer()]}},
            {3, {call, erlst, add_trader, [Acct, reliable_strategy()]}},
            % for the following sym calls
            % parameters should be injected at runtime
            {1, {call, erlst, rescind_offer, []}},
            {1, {call, erlst, remove_trader, []}}
        ])
    ).

% ------------------------ properties ------------------------
% checks that the total amount of money and total amount 
% of each stock on the exchange does not change as long 
% as no new accounts are opened
prop_value_preservation() ->
    ?FORALL(Holdings, nonempty_list_of(holdings()), 
    begin
        {ok, S} = erlst:launch(),
        Accts = [ erlst:open_account(S, Holding) 
                || Holding <- Holdings],
        ?FORALL(Cmds, nonempty_list_of(api_call(Accts)),
        begin
            MoneyBefore = total_money(Holdings),
            StocksBefore = total_stocks(Holdings),
            execute_sym_commands(S, Cmds, [], [], []),
            timer:sleep(50), % wait for the exchange to process the commands 
            MoneyAfter = total_money_api(Accts),
            StocksAfter = total_stocks_api(Accts),
            MoneyBefore =:= MoneyAfter andalso StocksBefore =:= StocksAfter
        end)
    end).

% checks that the number of trades returned by shutdown/1
% is less than or equal to the number of calls to make_offer/2.
prop_total_trades() ->
    ?FORALL(Holdings, nonempty_list_of(holdings()), 
    begin
        {ok, S} = erlst:launch(),
        Accts = [ erlst:open_account(S, Holding) 
                || Holding <- Holdings],
        ?FORALL(Cmds, nonempty_list_of(api_call(Accts)),
        begin
            execute_sym_commands(S, Cmds, [], [], []),
            timer:sleep(50), % wait for the exchange to process the commands 
            NumOfTrades = erlst:shutdown(S),
            NumOfOfferCalls = lists:sum([1 || {call, erlst, make_offer, _} <- Cmds]),
            NumOfTrades =< NumOfOfferCalls
        end)
    end).


% ------------------------ measurements ------------------------

normalize_sym_api_call(Cmd) ->
    case Cmd of
        {call, erlst, account_balance, _} ->
            {call, erlst, account_balance, []};
        {call, erlst, make_offer, _} ->
            {call, erlst, make_offer, []};
        {call, erlst, add_trader, _} ->
            {call, erlst, add_trader, []};
        {call, erlst, rescind_offer, _} ->
            {call, erlst, rescind_offer, []};
        {call, erlst, remove_trader, _} ->
            {call, erlst, remove_trader, []}
    end.

prop_aggregate_api_call() ->
    ?FORALL(Holdings, nonempty_list_of(holdings()), 
    begin
        {ok, S} = erlst:launch(),
        Accts = [ erlst:open_account(S, Holding) 
                || Holding <- Holdings],
        ?FORALL(Cmds, nonempty_list_of(api_call(Accts)),
        begin
            Executed = execute_sym_commands(S, Cmds, [], [], []),
            aggregate([normalize_sym_api_call(Cmd) || Cmd <- Executed], true)
        end)
    end).

        
