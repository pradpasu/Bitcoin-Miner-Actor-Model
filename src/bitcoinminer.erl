-module(bitcoinminer).

-import(binary, [decode_unsigned/1]).
-import(crypto, [hash/1]).
-export([getRandomStringFromCrypto/0, performMiningRecursively/3, getCoins/1, miner/0, collectCoins/0]).
-define(CommonPrefix, "ppasumarty").
-define(PrefixAndKeySeparator, ";").
-define(RecursionKillPoint, 1000).

performMiningRecursively(NumberOfLeadingZeroes, Counter, BossActor) ->
	RandomString = bitcoinminer:getRandomStringFromCrypto(),
	StringToBeHashed = string:concat(?CommonPrefix, string:concat(?PrefixAndKeySeparator, RandomString)),
	HashAsInteger = binary:decode_unsigned(crypto:hash(sha256,StringToBeHashed)),
	HashAsString = io_lib:format("~64.16.0b", [HashAsInteger]),
	LeadingZeroesString = io_lib:format("~*..0b", [NumberOfLeadingZeroes, 0]),
	case (string:str(HashAsString, LeadingZeroesString) == 1) of
	    true -> 
			NewCounter = Counter + 1,
			BossActor ! {StringToBeHashed, HashAsString},
			if 
				NewCounter < ?RecursionKillPoint ->
					bitcoinminer:performMiningRecursively(NumberOfLeadingZeroes, NewCounter, BossActor);
				true ->
					ok
			end;
	    false ->
			bitcoinminer:performMiningRecursively(NumberOfLeadingZeroes, Counter, BossActor)
  	end.

getRandomStringFromCrypto() -> base64:encode_to_string(crypto:strong_rand_bytes(6)).

getCoins(NumberOfLeadingZeroes) ->
	io:fwrite("Coin collection started"),
	{ok, FilePointer} = file:open("CollectedBitcoins.txt", [write]),
	io:format(FilePointer, "", []),
	file:close("CollectedBitcoins.txt"),
	BossActor = spawn(fun bitcoinminer:collectCoins/0),
	NumberOfActors = 4,
	lists:foldl(
		fun(_, _) -> 
			ChildActorPID = spawn(fun bitcoinminer:miner/0),
			timer:kill_after(5000, ChildActorPID),
			ChildActorPID ! {NumberOfLeadingZeroes, BossActor}
		end, 
		[], 
		lists:seq(1, NumberOfActors)
	).

miner() ->
	receive
		{NumberOfLeadingZeroes, BossActor} ->
			bitcoinminer:performMiningRecursively(NumberOfLeadingZeroes, 0, BossActor)
	end.

collectCoins() ->
	receive
		{StringToBeHashed, HashedString} ->
			{ok, FilePointer} = file:open("CollectedBitcoins.txt", [append]),
			io:format(FilePointer, "~p	~p~n", [StringToBeHashed, HashedString]),
			io:fwrite("~p	~p~n", [StringToBeHashed, HashedString]),
			collectCoins()
	end.