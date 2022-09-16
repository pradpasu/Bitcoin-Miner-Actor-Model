-module(bitcoinminer).

-import(binary, [decode_unsigned/1]).
-import(crypto, [hash/1]).
-export([getHash/1, getRandomStringFromCrypto/0, performGetHashRecursive/2]).

getHash(NumberOfLeadingZeroes) ->
	bitcoinminer:performGetHashRecursive(NumberOfLeadingZeroes, 0).

performGetHashRecursive(NumberOfLeadingZeroes, Counter) ->
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
				NewCounter < 50 ->
					bitcoinminer:performGetHashRecursive(NumberOfLeadingZeroes, NewCounter);
				true ->
					io:fwrite("Mining Completed \n")
			end;
	    false ->
			bitcoinminer:performGetHashRecursive(NumberOfLeadingZeroes, Counter)
  	end.

getRandomStringFromCrypto() -> 
	base64:encode_to_string(crypto:strong_rand_bytes(8)).