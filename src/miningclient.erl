-module(miningclient).

-export([triggerMiningOnServer/1]).
-define(ServerName, "prad@10.20.166.37").

triggerMiningOnServer(NumberOfLeadingZeroes)->
  rpc:call(?ServerName, miningserver, triggerMiningOnServer,[NumberOfLeadingZeroes]).
