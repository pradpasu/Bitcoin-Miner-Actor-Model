-module(bitcoinminer).

-import(binary, [decode_unsigned/1]).
-import(crypto, [hash/1]).
-export([getRandomStringFromCrypto/0, performMiningRecursively/3, getCoins/1, miner/0, collectCoins/0]).

performMiningRecursively(NumberOfLeadingZeroes, Counter, BossActor) ->
	CommonPrefix = "ppasumarty",
	PrefixAndKeySeparator = ";",
	RandomString = bitcoinminer:getRandomStringFromCrypto(),
	StringToBeHashed = string:concat(CommonPrefix, string:concat(PrefixAndKeySeparator, RandomString)),
	HashAsInteger = binary:decode_unsigned(crypto:hash(sha256,StringToBeHashed)),
	HashAsString = io_lib:format("~64.16.0b", [HashAsInteger]),
	LeadingZeroesString = io_lib:format("~*..0b", [NumberOfLeadingZeroes, 0]),
	case (string:str(HashAsString, LeadingZeroesString) == 1) of
	    true -> 
			NewCounter = Counter + 1,
			BossActor ! {StringToBeHashed, HashAsString},
			if 
				NewCounter < 5 ->
					bitcoinminer:performMiningRecursively(NumberOfLeadingZeroes, NewCounter, BossActor);
				true ->
					ok
			end;
	    false ->
			bitcoinminer:performMiningRecursively(NumberOfLeadingZeroes, Counter, BossActor)
  	end.

getRandomStringFromCrypto() -> base64:encode_to_string(crypto:strong_rand_bytes(3)).

getCoins(NumberOfLeadingZeroes) ->
	BossActor = spawn(fun bitcoinminer:collectCoins/0),
	BossActor ! {'initialize coin collection'},
	NumberOfActors = 4,
	lists:foldl(
		fun(_, _) -> 
			spawn(fun bitcoinminer:miner/0) ! {self(), NumberOfLeadingZeroes, BossActor}
		end, 
		[], 
		lists:seq(1, NumberOfActors)
	).

miner() ->
	receive
		{_, NumberOfLeadingZeroes, BossActor} ->
			bitcoinminer:performMiningRecursively(NumberOfLeadingZeroes, 0, BossActor)
	end.

collectCoins() ->
	receive
		{'initialize coin collection'} ->
			io:fwrite("Coin collecting initialized in Boss Actor ~n"),
			collectCoins();
		{StringToBeHashed, HashedString} ->
			io:fwrite("~p	~p~n", [StringToBeHashed, HashedString]),
			collectCoins()
	end.


%% cd("E:/DOSP Projects/Bitcoin-Miner-Actor-Model/src").