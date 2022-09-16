-module(bitcoinminer).

-import(binary, [decode_unsigned/1]).
-import(crypto, [hash/1]).
-export([getRandomStringFromCrypto/0, performMiningRecursively/2, getCoins/1, miner/0]).

performMiningRecursively(NumberOfLeadingZeroes, Counter) ->
	CommonPrefix = "ppasumarty",
	PrefixAndKeySeperator = ";",
	RandomString = bitcoinminer:getRandomStringFromCrypto(),
	StringToBeHashed = string:concat(CommonPrefix, string:concat(PrefixAndKeySeperator, RandomString)),
	HashAsInteger = binary:decode_unsigned(crypto:hash(sha256,StringToBeHashed)),
	HashAsString = io_lib:format("~64.16.0b", [HashAsInteger]),
	LeadingZeroesString = io_lib:format("~*..0b", [NumberOfLeadingZeroes, 0]),
	case (string:str(HashAsString, LeadingZeroesString) == 1) of
	    true -> 
			NewCounter = Counter + 1,
			io:fwrite("(~p): ", [NewCounter]),
			io:fwrite("Hashed Value of ~p is ~p~n", [StringToBeHashed, HashAsString]),
			if 
				NewCounter < 5 ->
					bitcoinminer:performMiningRecursively(NumberOfLeadingZeroes, NewCounter);
				true ->
					io:fwrite("")
			end;
	    false ->
			bitcoinminer:performMiningRecursively(NumberOfLeadingZeroes, Counter)
  	end.

getRandomStringFromCrypto() -> base64:encode_to_string(crypto:strong_rand_bytes(3)).

getCoins(NumberOfLeadingZeroes) -> 
	NumberOfActors = 4,
	lists:foldl(
		fun(_, _) -> 
			spawn(fun bitcoinminer:miner/0) ! {self(), NumberOfLeadingZeroes} 
		end, 
		[], 
		lists:seq(1, NumberOfActors)
	).

miner() ->
	receive
		{From, NumberOfLeadingZeroes} -> 
			bitcoinminer:performMiningRecursively(NumberOfLeadingZeroes, 0),
			io:fwrite("Mining finished by Actor ~p", [From])
	end.