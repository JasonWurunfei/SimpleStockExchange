-module(test_api).
-export([test_api_all/0]).

-include_lib("eunit/include/eunit.hrl").

test_api_all() ->
    eunit:test(
      [
        test_launch(),
        test_shutdown_1(),
        test_open_account(),
        test_account_balance(),
        test_make_offer_1(),
        test_make_offer_2(),
        test_make_invalid_offer(),
        test_rescind_offer_non_blocking(),
        test_add_trader(),
        test_remove_trader(),
        test_trade(),
        test_trade_not_excute_1(),
        test_trade_not_excute_2(),
        test_trade_not_excute_3(),
        test_trade_3(),
        test_trade_4(),
        test_shutdown_2(),
        test_rescind_offer_terminates_strategy()
      ], [verbose]).

test_launch() -> 
    {"test launch", 
    fun() ->
        case erlst:launch() of
            {ok, _} -> ok;
            _ -> ?assert(false)
        end
    end}.

test_shutdown_1() -> 
    {"test shutdown right after launch", 
    fun() ->
        {ok, S} = erlst:launch(),
        ?assertEqual(0, erlst:shutdown(S))
    end}.

test_open_account() -> 
    {"test open account", 
    fun() ->
        {ok, S} = erlst:launch(),
        % should execute without error
        erlst:open_account(S, {1000, [{a, 5}, {b, 10}]})
    end}.

test_account_balance() ->
    {"test account balance", 
    fun() ->
        {ok, S} = erlst:launch(),
        Hoddings = {1000, [{a, 5}, {b, 10}]},
        Acct = erlst:open_account(S, Hoddings),
        ?assertEqual(Hoddings, erlst:account_balance(Acct))
    end}.

test_make_offer_1() ->
    {"test make offer when the account has the stock", 
    fun() ->
        {ok, S} = erlst:launch(),
        Hoddings = {1000, [{a, 5}, {b, 10}]},
        Acct = erlst:open_account(S, Hoddings),
        % should execute without error
        case erlst:make_offer(Acct, {a, 500}) of
            {ok, _} -> ok;
            _ -> ?assert(false)
        end
    end}.

test_make_offer_2() ->
    {"test make offer when the account does not have the stock", 
    fun() ->
        {ok, S} = erlst:launch(),
        Hoddings = {1000, [{a, 5}, {b, 10}]},
        Acct = erlst:open_account(S, Hoddings),
        % should execute without error
        case erlst:make_offer(Acct, {c, 500}) of
            {ok, _} -> ok;
            _ -> ?assert(false)
        end
    end}.

test_make_invalid_offer() ->
    {"test make invalid offer", 
    fun() ->
        {ok, S} = erlst:launch(),
        Hoddings = {1000, [{a, 5}, {b, 10}]},
        Acct = erlst:open_account(S, Hoddings),
        case erlst:make_offer(Acct, {c, -500}) of
            {error, _} -> ok;
            _ -> ?assert(false)
        end
    end}.

test_rescind_offer_non_blocking() ->
    {"test rescind offer non blocking", 
    fun() ->
        {ok, S} = erlst:launch(),
        Hoddings = {1000, [{a, 5}, {b, 10}]},
        Acct = erlst:open_account(S, Hoddings),
        {ok, OfferId} = erlst:make_offer(Acct, {a, 500}),
        % should execute without error
        ?assertEqual(ok, erlst:rescind_offer(Acct, OfferId))
    end}.

test_add_trader() ->
    {"test add trader", 
    fun() ->
        {ok, S} = erlst:launch(),
        Hoddings = {1000, [{a, 5}, {b, 10}]},
        Acct = erlst:open_account(S, Hoddings),
        % should execute without error
        erlst:add_trader(Acct, fun({a, _}) -> accept end)
    end}.

test_remove_trader() ->
    {"test remove trader", 
    fun() ->
        {ok, S} = erlst:launch(),
        Hoddings = {1000, [{a, 5}, {b, 10}]},
        Acct = erlst:open_account(S, Hoddings),
        Strategy = fun({a, _}) -> accept end,
        TraderId = erlst:add_trader(Acct, Strategy),
        % should execute without error
        ?assertEqual(ok, erlst:remove_trader(Acct, TraderId))
    end}.

test_trade() ->
    {"test trade success", 
    fun() ->
        {ok, S} = erlst:launch(),
        Hoddings1 = {1000, [{a, 5}, {b, 10}]},
        Hoddings2 = {500, []},
        Offer = {a, 500},
        Strategy = fun({a, _}) -> accept end,

        Acct1 = erlst:open_account(S, Hoddings1),
        Acct2 = erlst:open_account(S, Hoddings2),
        erlst:make_offer(Acct1, Offer),
        erlst:add_trader(Acct2, Strategy),

        % Acct2 should buy 1 share of a from Acct1
        timer:sleep(100), % wait for the trade to happen
        
        ?assertEqual({1500, [{a, 4}, {b, 10}]}, 
            erlst:account_balance(Acct1)),
        ?assertEqual({0, [{a, 1}]},
            erlst:account_balance(Acct2)),
        ?assertEqual(1, erlst:shutdown(S))
    end}.

test_trade_not_excute_1() ->
    {"Trader does not have enough money",
    fun() ->
        {ok, S} = erlst:launch(),
        Hoddings1 = {1000, [{a, 5}, {b, 10}]},
        Hoddings2 = {100, []},
        Offer = {a, 500},
        Strategy = fun({a, _}) -> accept end,

        Acct1 = erlst:open_account(S, Hoddings1),
        Acct2 = erlst:open_account(S, Hoddings2),
        erlst:make_offer(Acct1, Offer),
        erlst:add_trader(Acct2, Strategy),

        % Acct2 can not efford to buy 1 share of a from Acct1
        timer:sleep(100), % wait for possible trade to happen
        
        ?assertEqual(Hoddings1, 
            erlst:account_balance(Acct1)),
        ?assertEqual(Hoddings2,
            erlst:account_balance(Acct2)),
        ?assertEqual(0, erlst:shutdown(S))
    end}.

test_trade_not_excute_2() ->
    {"No offer to trade",
    fun() ->
        {ok, S} = erlst:launch(),
        Hoddings1 = {1000, [{a, 5}, {b, 10}]},
        Hoddings2 = {1000, []},
        Strategy = fun({a, _}) -> accept end,

        Acct1 = erlst:open_account(S, Hoddings1),
        Acct2 = erlst:open_account(S, Hoddings2),
        erlst:add_trader(Acct2, Strategy),

        % nothing to trade
        timer:sleep(100), % wait for possible trade to happen
        
        ?assertEqual(Hoddings1, 
            erlst:account_balance(Acct1)),
        ?assertEqual(Hoddings2,
            erlst:account_balance(Acct2)),
        ?assertEqual(0, erlst:shutdown(S))
    end}.

test_trade_not_excute_3() ->
    {"Offerer does not have enough stock",
    fun() ->
        {ok, S} = erlst:launch(),
        Hoddings1 = {1000, [{a, 5}, {b, 10}]},
        Hoddings2 = {500, []},
        Offer = {c, 500},
        Strategy = fun({c, _}) -> accept end,

        Acct1 = erlst:open_account(S, Hoddings1),
        Acct2 = erlst:open_account(S, Hoddings2),
        erlst:make_offer(Acct1, Offer),
        erlst:add_trader(Acct2, Strategy),

        % Acct2 can not buy 1 share of c from Acct1
        % because Acct1 does not have any c
        timer:sleep(100), % wait for possible trade to happen
        
        ?assertEqual(Hoddings1, 
            erlst:account_balance(Acct1)),
        ?assertEqual(Hoddings2,
            erlst:account_balance(Acct2)),
        ?assertEqual(0, erlst:shutdown(S))
    end}.


test_trade_3() ->
    {"Offer does not have enough stock initially but has enough after a trade",
    fun() ->
        {ok, S} = erlst:launch(),
        Hoddings1 = {1000, []},
        Hoddings2 = {500, []},
        Hoddings3 = {1000, [{c, 10}]},
        Offer1 = {c, 500},
        Offer2 = {c, 400},
        Strategy = fun({c, _}) -> accept end,

        Acct1 = erlst:open_account(S, Hoddings1),
        Acct2 = erlst:open_account(S, Hoddings2),
        Acct3 = erlst:open_account(S, Hoddings3),

        erlst:make_offer(Acct1, Offer1),
        Tid1 = erlst:add_trader(Acct2, Strategy),
        % Acct2 can not buy 1 share of c from Acct1
        % because Acct1 does not have any c
        timer:sleep(100), % wait for possible trade to happen
        % nothing has changed
        ?assertEqual(Hoddings1, 
            erlst:account_balance(Acct1)),
        ?assertEqual(Hoddings2,
            erlst:account_balance(Acct2)),
        ?assertEqual(Hoddings3,
            erlst:account_balance(Acct3)),
        
        % remove trader of Acct2 so that Acct1 can buy c from Acct3
        % otherwise Acct2 may buy c from Acct1 first
        erlst:remove_trader(Acct2, Tid1), % non-blocking
        timer:sleep(100), % wait for the server to remove the trader
    
        erlst:add_trader(Acct1, Strategy),
        erlst:make_offer(Acct3, Offer2),
        % Acct1 should buy 2 share of c from Acct3
        timer:sleep(100), % wait for possible trade to happen
        ?assertEqual({600, [{c, 1}]}, 
            erlst:account_balance(Acct1)),
        ?assertEqual(Hoddings2,
            erlst:account_balance(Acct2)),
        ?assertEqual({1400, [{c, 9}]},
            erlst:account_balance(Acct3)),
        
        erlst:add_trader(Acct2, Strategy),
        % Acct2 should buy 1 share of c from Acct1
        timer:sleep(100), % wait for possible trade to happen
        ?assertEqual({1100, []}, 
            erlst:account_balance(Acct1)),
        ?assertEqual({0, [{c, 1}]},
            erlst:account_balance(Acct2)),
        ?assertEqual({1400, [{c, 9}]},
            erlst:account_balance(Acct3)),

        % In total, 2 trades should happen. 
        % 1 for acct1 buying 2 shares of c from acct3
        % 1 for acct2 buying 1 share of c from acct1
        ?assertEqual(2, erlst:shutdown(S))
    end}.

test_trade_4() ->
    {"fast strategy always trade first",
    fun() ->
        {ok, S} = erlst:launch(),
        Hoddings1 = {1000, []},
        Hoddings2 = {1000, []},
        Hoddings3 = {1000, [{c, 10}]},

        Offer = {c, 100},
        Strategy1 = fun({c, _}) -> accept end,
        Strategy2 = fun({c, _}) -> timer:sleep(1000000000), accept end,

        Acct1 = erlst:open_account(S, Hoddings1),
        Acct2 = erlst:open_account(S, Hoddings2),
        Acct3 = erlst:open_account(S, Hoddings3),

        erlst:add_trader(Acct1, Strategy1),
        erlst:add_trader(Acct2, Strategy2),

        erlst:make_offer(Acct3, Offer),
        timer:sleep(100), % wait for possible trade to happen
        % Acct1 should buy 10 share of c from Acct3 and
        % Acct2 can not buy any since it is too slow
        ?assertEqual({900, [{c, 1}]}, 
            erlst:account_balance(Acct1)),
        ?assertEqual(Hoddings2,
            erlst:account_balance(Acct2)),
        ?assertEqual({1100, [{c, 9}]},
            erlst:account_balance(Acct3)),
        ?assertEqual(1, erlst:shutdown(S))
    end}.

test_shutdown_2() -> 
    {"shutdown should not be delayed by slow trader strategy",
    fun() ->
        {ok, S} = erlst:launch(),
        Hoddings1 = {1000, []},
        Hoddings2 = {1000, [{c, 10}]},
        Offer = {c, 100},
        Strategy = fun({c, _}) -> timer:sleep(1000000000000), accept end,

        Acct1 = erlst:open_account(S, Hoddings1),
        Acct2 = erlst:open_account(S, Hoddings2),

        erlst:make_offer(Acct2, Offer),
        erlst:add_trader(Acct1, Strategy),
        timer:sleep(100), % does not matter we wait or not here, 
                          % since the strategy is too slow but I
                          % still want to stay uniform

        ?assertEqual(Hoddings1, 
            erlst:account_balance(Acct1)),
        ?assertEqual(Hoddings2,
            erlst:account_balance(Acct2)),
        % no trade should happen
        ?assertEqual(0, erlst:shutdown(S))
    end}.

test_rescind_offer_terminates_strategy() ->
    {"rescind offer should terminate relevant strategy evaluation",
    fun() ->
        {ok, S} = erlst:launch(),
        Hoddings1 = {1000, []},
        Hoddings2 = {1000, [{c, 1}]},
        Hoddings3 = {1000, [{c, 1}]},
        Offer = {c, 100},
        Strategy = fun({c, _}) -> timer:sleep(100), accept end,

        Acct1 = erlst:open_account(S, Hoddings1),
        Acct2 = erlst:open_account(S, Hoddings2),
        Acct3 = erlst:open_account(S, Hoddings3),

        erlst:make_offer(Acct2, Offer),
        erlst:add_trader(Acct1, Strategy),
        timer:sleep(200), % wait for possible trade to happen
        % a trade should take place
        ?assertEqual({900, [{c, 1}]}, 
            erlst:account_balance(Acct1)),
        ?assertEqual({1100, []},
            erlst:account_balance(Acct2)),
        ?assertEqual(Hoddings3,
            erlst:account_balance(Acct3)),

        {ok, OfferID} = erlst:make_offer(Acct3, Offer),
        timer:sleep(10), % just to let the strategy start
        erlst:rescind_offer(Acct3, OfferID), % non-blocking
        timer:sleep(10), % wait for the server to remove the offer
        
        timer:sleep(200), % if the strategy is not terminated,
                          % the trade should happened already
        % nothing should happen since the offer is removed
        ?assertEqual({900, [{c, 1}]},
            erlst:account_balance(Acct1)),
        ?assertEqual({1100, []},
            erlst:account_balance(Acct2)),
        ?assertEqual(Hoddings3,
            erlst:account_balance(Acct3))
    end}.