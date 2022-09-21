-module(mining_server).
-behaviour(gen_server).
-export([start_link/0, getCoins/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast({global,?MODULE}, stop).

getCoins(NumberOfLeadingZeroes) ->
  gen_server:call({global,?MODULE},{getCoins,NumberOfLeadingZeroes}).

init([]) ->
  process_flag(trap_exit, true),
  io:format("~p (~p) starting.... ~n", [{global, ?MODULE}, self()]),
  {ok, []}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call({getCoins,NumberOfLeadingZeroes}, _From, State) ->
  {reply, {bitcoinminer:getCoins(NumberOfLeadingZeroes)},State};

handle_call(_Request, _From, State) ->
  {reply,error, State}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("~p (~p) terminating.... ~n", [{global, ?MODULE}, self()]),
  {ok, []}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.