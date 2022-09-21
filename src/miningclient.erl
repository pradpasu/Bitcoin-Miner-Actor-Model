-module(miningclient).

-export([triggerMiningOnServer/1]).

triggerMiningOnServer(NumberOfLeadingZeroes)->
  rpc:call('prad@10.20.166.37', miningserver, triggerMiningOnServer,[NumberOfLeadingZeroes]).
