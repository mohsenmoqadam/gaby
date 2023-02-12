-module(gaby_os_oid).

-export([encode/3,
         decode/1]).

-include("gaby.hrl").

encode(CRA, UID, Type) ->
    OID = http_uri:encode(base58:binary_to_base58(term_to_binary({Type, CRA, UID}))),
    {ok, OID}.

decode(OID0) when is_bitstring(OID0) ->
    OID = bitstring_to_list(OID0),
    case catch binary_to_term(base58:base58_to_binary(http_uri:decode(OID))) of
        {Type, CRA, UID} ->
            {ok, Type, CRA, UID};
        _ ->
            {error, invalid_oid}
    end;
decode(OID) when is_list(OID) ->
    case catch binary_to_term(base58:base58_to_binary(http_uri:decode(OID))) of
        {Type, CRA, UID} ->
            {ok, Type, CRA, UID};
        _ ->
            {error, invalid_oid}
    end;
decode(_OID) ->
    {error, invalid_oid}.
