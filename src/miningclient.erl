-module(miningclient).

-export([triggerMiningOnServer/1]).

triggerMiningOnServer(NumberOfLeadingZeroes)->
  rpc:call('myserver@10.20.166.52', miningserver, triggerMiningOnServer,[NumberOfLeadingZeroes]).
