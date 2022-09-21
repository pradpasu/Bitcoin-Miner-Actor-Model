-module(mining_client).

-export([start/0,stop/0,getCoins/1]).

start()->
  mining_server:start_link().

stop()->
  mining_server:stop().

getCoins(NumberOfLeadingZeroes)->
  mining_server:getCoins(NumberOfLeadingZeroes).
