-module(erlst).

% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called erlst.

-import(async, [new/2, wait/1, poll/1, wait_catch/1, wait_any/1]).

% gen_server behaviour
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).

% Export at least the API:
-export([launch/0,
         shutdown/1,
         open_account/2,
         account_balance/1,
         make_offer/2,
         rescind_offer/2,
         add_trader/2,
         remove_trader/2
        ]).

% You may have other exports as well
-export([show_state/1]).

-type stock_exchange() :: term().
-type account_id() :: term().
-type offer_id() :: term().
-type trader_id() :: term().
-type stock() :: atom().
-type isk() :: non_neg_integer().
-type stock_amount() :: pos_integer().
-type holdings() :: {isk(), [{stock(), stock_amount()}]}.
-type offer() :: {stock(), isk()}.
-type decision() :: accept | reject.
-type trader_strategy() :: fun((offer()) -> decision()).

-spec launch() -> {ok, stock_exchange()} | {error, term()}.
launch() ->
    try
        gen_server:start(?MODULE, {}, [])
    catch
        Type:Reason:Stacktrace ->
            {error, {Type, Reason, Stacktrace}}
    end.

-spec shutdown(S :: stock_exchange()) -> non_neg_integer().
shutdown(S) ->
    gen_server:call(S, shutdown).

-spec open_account(S :: stock_exchange(), holdings()) -> account_id().
open_account(S, Holdings) ->
    gen_server:call(S, {open_account, Holdings}, infinity).

-spec account_balance(Acct :: account_id()) -> holdings().
account_balance(Acct) ->
    {S, ARef} = Acct,
    gen_server:call(S, {account_balance, ARef}, infinity).

-spec make_offer(Acct :: account_id(), Terms :: offer()) -> {ok, offer_id()} | {error, term()}.
make_offer(Acct, Offer) ->
    {S, ARef} = Acct,
    gen_server:call(S, {make_offer, ARef, Offer}, infinity).

-spec rescind_offer(Acct :: account_id(), Offer :: offer_id()) -> ok.
rescind_offer(Acct, OfferId) ->
    {S, _} = Acct,
    gen_server:cast(S, {rescind_offer, OfferId}).

-spec add_trader(Acct :: account_id(), Strategy :: trader_strategy()) -> trader_id().
add_trader(Acct, Strategy) ->
    {S, ARef} = Acct,
    gen_server:call(S, {add_trader, ARef, Strategy}, infinity).

-spec remove_trader(Acct :: account_id(), Trader :: trader_id()) -> ok.
remove_trader(Acct, TraderId) ->
    {S, _} = Acct,
    gen_server:cast(S, {remove_trader, TraderId}).

show_state(S) ->
    gen_server:call(S, debug, infinity).


init({}) -> 
    % initialize the stock exchange state
    %{
    %   num_of_trades      => non_neg_integer(),
    %   next_account_ref() => integer(), -- not to mixup with account_id()
    %   next_offer_id()    => integer(),
    %   next_trader_id()   => integer(),
    %   accounts()         => #{account_ref() => holdings()},
    %   offers()           => #{offer_id()    => {account_ref(), offer()}},
    %   traders()          => #{trader_id()   => {account_ref(), trader_strategy()}}
    %   trading_pids()     => [pid()]
    %   trading_eval_info()=> [{offer_id(), trader_id(), pid()}]
    %}
    {ok, 
        #{
            num_of_trades        => 0,
            next_account_ref     => 0,
            next_offer_id        => 0,
            next_trader_id       => 0,
            accounts             => #{},
            offers               => #{},
            traders              => #{},
            trading_pids         => [],
            trading_eval_records => []
        }
    }.

clean_up(State) ->
    % clean up all the trading processes and return the fresh state.
    % abort all trading processes as they are not relevant anymore
    TradingPids = maps:get(trading_pids, State),
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, TradingPids),

    % abort all trading evaluation processes as they are not relevant anymore
    TradeEvelRecords = maps:get(trading_eval_records, State),
    lists:foreach(fun({_, _, Pid}) -> exit(Pid, kill) end, TradeEvelRecords),
    State#{
        trading_pids         => [],
        trading_eval_records => []
    }.

handle_call(debug, _From, State) ->
    {reply, State, State};

handle_call(shutdown, From, State) ->
    NumOfTrades = maps:get(num_of_trades, State),
    gen_server:reply(From, NumOfTrades),
    {stop, normal, ok, clean_up(State)};

handle_call({open_account, Holdings}, _From, State) ->
    NARef = maps:get(next_account_ref, State),
    Accounts = maps:get(accounts, State),
    NewState = State#{
        next_account_ref := NARef + 1,
        accounts := Accounts#{NARef => Holdings}
    },
    Acct = {self(), NARef},
    {reply, Acct, NewState};

handle_call({account_balance, ARef}, _From, State) ->
    Accounts = maps:get(accounts, State),
    Holdings = maps:get(ARef, Accounts),
    {reply, Holdings, State};

handle_call({make_offer, ARef, {StockName, Price}}, _From, State) ->
    if Price < 0 -> {reply, {error, invalid_price}, State};
    true ->
        Accounts = maps:get(accounts, State),
        % check if account exists
        case maps:get(ARef, Accounts, undefined) of
            undefined ->
                {reply, {error, account_not_found}, State};
            _ ->
                % create new offer
                NOId = maps:get(next_offer_id, State),
                Offers = maps:get(offers, State),
                NewState = State#{
                    next_offer_id := NOId + 1,
                    offers := Offers#{NOId => {ARef, {StockName, Price}}}
                },
                % new offer added triggers new trade evaluation
                {reply, {ok, NOId}, trade_eval(NewState)}
        end
    end;

handle_call({add_trader, ARef, Strategy}, _From, State) ->
    NTId = maps:get(next_trader_id, State),
    Traders = maps:get(traders, State),
    NewState = State#{
        next_trader_id := NTId + 1,
        traders := Traders#{NTId => {ARef, Strategy}}
    },
    % new trader added triggers new trade evaluation
    {reply, NTId, trade_eval(NewState)}.

terminate_record(byOfferId, OfferId, State) ->
    TradeEvelRecords = maps:get(trading_eval_records, State),
    TargetRecords = lists:filter(
        fun({OId, _, _}) -> OId =:= OfferId end, TradeEvelRecords),
    RemainingRecords = lists:filter(
        fun({OId, _, _}) -> OId =/= OfferId end, TradeEvelRecords),
    lists:foreach(fun({_, _, Pid}) -> exit(Pid, kill) end, TargetRecords),
    State#{trading_eval_records := RemainingRecords};

terminate_record(byTraderId, TraderId, State) ->
    TradeEvelRecords = maps:get(trading_eval_records, State),
    TargetRecords = lists:filter(
        fun({_, TId, _}) -> TId =:= TraderId end, TradeEvelRecords),
    RemainingRecords = lists:filter(
        fun({_, TId, _}) -> TId =/= TraderId end, TradeEvelRecords),
    lists:foreach(fun({_, _, Pid}) -> exit(Pid, kill) end, TargetRecords),
    State#{trading_eval_records := RemainingRecords}.

handle_cast({rescind_offer, OfferID}, State) ->
    Offers = maps:get(offers, State),
    case maps:take(OfferID, Offers) of
        error -> {noreply, State};
        {_, NewOffers} ->
            State_ = terminate_record(byOfferId, OfferID, State),
            NewState = State_#{offers := NewOffers},
            {noreply, NewState}
    end;

handle_cast({remove_trader, TraderID}, State) ->
    Traders = maps:get(traders, State),
    NewTraders = maps:remove(TraderID, Traders),
    State_ = terminate_record(byTraderId, TraderID, State),
    NewState = State_#{
        traders := NewTraders
    },
    {noreply, NewState};

handle_cast({trade, NewState}, _) ->
    % abort all trading processes as they are not relevant anymore
    FreshState = clean_up(NewState),
    % New trade happened, it triggers new trade evaluation
    {noreply, trade_eval(FreshState)};

handle_cast({register, TradeEvelRecord}, State) ->
    TradeEvelRecords = maps:get(trading_eval_records, State),
    NewState = State#{
        trading_eval_records := [TradeEvelRecord | TradeEvelRecords]
    },
    {noreply, NewState}.

wait_success(Funs, Timeout) ->
    % run all the functions concurrently wait for the 
    % first one to succeed and return its result
    % if none of them succeed within the timeout
    % return {error, timeout}
    Self = self(),
    Refs = lists:map(fun(Fun) -> 
        spawn(fun() -> 
            try 
                case Fun() of
                    {ok, Res} -> Self ! {ok, Res};
                    _ -> ignore
                end
            catch
                _ -> ignore
            end
        end) 
    end, Funs),
    receive
        {ok, Result} -> 
            % kill all the processes
            lists:foreach(fun(Ref) -> exit(Ref, kill) end, Refs),
            {ok, Result}
    after Timeout ->
        lists:foreach(fun(Ref) -> exit(Ref, kill) end, Refs),
        {error, timeout}
    end.

wait_success(Funs) -> wait_success(Funs, infinity).

trade_eval(State) ->
    S = self(),
    Pid = spawn(fun() ->
        Offers = maps:get(offers, State),
        Res = wait_success([
            fun() -> match_making(OfferEntry, State, S) end
            || OfferEntry <- maps:to_list(Offers)
        ]),
        case Res of
            {ok, NewState} -> gen_server:cast(S, {trade, NewState});
            _ -> ignore
        end
    end),
    TradingPids = maps:get(trading_pids, State),
    State#{trading_pids := [Pid | TradingPids]}.

% takes an offer and tries to match it with all traders
% if a trade succeeds it returns the new state
match_making(OfferEntry, State, ServerPid) ->
    Traders = maps:get(traders, State),
    {OfferID, OfferItem} = OfferEntry,
    wait_success([
        fun() -> 
            {TraderId, TraderItem} = TraderEntry,
            gen_server:cast(ServerPid, {register, {OfferID, TraderId, self()}}),
            case check_and_trade(OfferItem, TraderItem, State) of
                {ok, NewState} -> 
                    % remove the offer
                    NewOffers = maps:remove(OfferID, maps:get(offers, NewState)),
                    {ok, NewState#{offers := NewOffers}};
                {fail, Reason} -> {fail, Reason}
            end
        end
        || TraderEntry <- maps:to_list(Traders)
    ]).

% takes an offer and a trader and tries to match them
% if a trade succeeds, it returns the new state
% for a trade to succeed:
%   - There is an offer on the exchange (already satified by the setup)
%   - the trader strategy must accept the offer
%   - the offerer must have at least one share of the stock
%   - the trader must have enough money to buy the stock
check_and_trade(OfferItem, TraderItem, State) ->
    % getting the variables ready
    {OARef, {StockName, Price}} = OfferItem,
    {TARef, Strategy} = TraderItem,
    Accounts = maps:get(accounts, State),
    {_, OStocks} = maps:get(OARef, Accounts),
    {TBalance, _} = maps:get(TARef, Accounts),

    % check if the traderer and offerer are the same
    case OARef =:= TARef of
    false ->
        % check if the trader account has enough money to buy the stock
        case TBalance >= Price of
        true ->
            % check if the offerer account has at least one share of the stock
            case lists:keyfind(StockName, 1, OStocks) of
            false ->
                {fail, {offerer_has_no_stock, StockName}};
            _ -> 
                % check if the trader strategy accepts the offer
                case Strategy({StockName, Price}) of
                accept ->
                    % trade is successful
                    {ok, trade(OfferItem, TraderItem, State)};
                reject ->
                    {fail, rejected}
                end
            end;
        false ->
            {fail, not_enough_money}
        end;
    true ->
        {fail, same_account}
    end.

% Perform the trade action
trade(OfferItem, TraderItem, State) ->
    % get the account references
    {OfferAcctRef, {Stock, Price}} = OfferItem,
    {TraderAcctRef, _} = TraderItem,

    % get the holdings
    Accounts = maps:get(accounts, State),
    {OfferAcctBalance, OfferAcctStocks} = maps:get(OfferAcctRef, Accounts),
    {TraderAcctBalance, TraderAcctStocks} = maps:get(TraderAcctRef, Accounts),

    % update the balances
    NewOfferAcctBalance = OfferAcctBalance + Price,
    NewTraderAcctBalance = TraderAcctBalance - Price,

    % update the stocks
    {Stock, StockAmount} = lists:keyfind(Stock, 1, OfferAcctStocks),
    NewStockAmount = StockAmount - 1,
    if NewStockAmount =< 0 -> 
        NewOfferAcctStocks = lists:delete(
            {Stock, StockAmount}, OfferAcctStocks);
    true ->
        NewOfferAcctStocks = lists:keyreplace(
            Stock, 1, OfferAcctStocks, {Stock, NewStockAmount})
    end,

    case lists:keyfind(Stock, 1, TraderAcctStocks) of
        false ->
            NewTraderAcctStocks = [{Stock, 1} | TraderAcctStocks];
        {Stock, TraderStockAmount} ->
            NewTraderAcctStocks = lists:keyreplace(
                Stock, 1, TraderAcctStocks, {Stock, TraderStockAmount + 1})
    end,

    % update the accounts
    NewOfferAcctHoldings = {NewOfferAcctBalance, NewOfferAcctStocks},
    NewTraderAcctHoldings = {NewTraderAcctBalance, NewTraderAcctStocks},
    NewAccounts1 = maps:put(OfferAcctRef, NewOfferAcctHoldings, Accounts),
    NewAccounts2 = maps:put(TraderAcctRef, NewTraderAcctHoldings, NewAccounts1),

    % upadate the number of trades
    NumOfTrades = maps:get(num_of_trades, State),
    NewNumOfTrades = NumOfTrades + 1,

    % return the new state
    State#{accounts := NewAccounts2, num_of_trades := NewNumOfTrades}.
