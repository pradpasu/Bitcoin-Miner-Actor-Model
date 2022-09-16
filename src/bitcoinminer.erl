-module(bitcoinminer).

-import(binary, [decode_unsigned/1]).
-import(crypto, [hash/1]).
-export([getHash/1, getRandomString/2, getRandomStringFromCrypto/0]).

getHash(NumberOfLeadingZeroes) ->
	CommonPrefix = "ppasumarty",
	PrefixAndKeySeperator = ";",
	RandomString = tut:getRandomStringFromCrypto(),
	StringToBeHashed = string:concat(CommonPrefix, string:concat(PrefixAndKeySeperator, RandomString)),
	HashAsInteger = binary:decode_unsigned(crypto:hash(sha256,StringToBeHashed)),
	HashAsString = io_lib:format("~64.16.0b", [HashAsInteger]),
	LeadingZeroesString = io_lib:format("~*..0b", [NumberOfLeadingZeroes, 0]),
	case (string:str(HashAsString, LeadingZeroesString) == 1) of
	    true -> io:fwrite("Hashed Value of ~p is ~p~n", [StringToBeHashed, HashAsString]);
	    false -> tut:getHash(NumberOfLeadingZeroes)
  	end.

getRandomStringFromCrypto() -> 
	base64:encode_to_string(crypto:strong_rand_bytes(8)).