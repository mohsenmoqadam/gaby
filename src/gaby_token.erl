-module(gaby_token).

-export([encode/1,
         decode/1
        ]).

-include("gaby.hrl").
-include("token.hrl").

-spec encode({ast, gaby_uid(), gaby_aid()} | {ost, gaby_uid(), gaby_aid()}) -> 
	  {ok, token()} | {error, invalid_params}.
encode(Params) ->
    case Params of
	{ost, UID, AID} ->
	    encode_ost(UID, AID);
	{ast, UID, AID} ->
	    encode_ast(UID, AID);
	_ -> 
	    {error, invalid_params}
    end.

-spec encode_ast(gaby_uid(), gaby_aid()) -> {ok, token()}.
encode_ast(UID, AID) ->
    {ok, _Now, Key, _Expiration} = get_jwt_conf(),
    Claims = [{type, ast}, {uid, UID}, {aid, AID}],
    Token = list_to_binary(http_uri:encode(binary_to_list(jwerl:sign(Claims, hs256, Key)))),
    {ok, Token}.

-spec encode_ost(gaby_uid(), gaby_aid()) -> {ok, token()}.
encode_ost(UID, AID) ->
    {ok, _Now, Key, _Expiration} = get_jwt_conf(),
    Claims = [{type, ost}, {uid, UID}, {aid, AID}],
    Token = list_to_binary(http_uri:encode(binary_to_list(jwerl:sign(Claims, hs256, Key)))),
    {ok, Token}.

-spec decode(token()) -> {ok, token_info()} | {error, invalid_token}.
decode(Token0) ->
    try
	{ok, Now, Key, _Expiration} = get_jwt_conf(),
	Token = http_uri:decode(Token0),
	{ok, Claims} = jwerl:verify(Token, hs256, Key),
	TokenInfo0 = #token_info{ type = list_to_atom(binary_to_list(maps:get(type, Claims)))
				, uid = maps:get(uid, Claims)
				, aid = maps:get(aid, Claims)
				},
	EXP = maps:get(expire, Claims, Now + 1),
	TokenInfo1 =
	    if EXP =< Now -> TokenInfo0#token_info{is_expired = true};
	       true -> TokenInfo0#token_info{is_expired = false}
	    end,
	{ok, TokenInfo1}
    catch 
	_:_ ->
	    {error, invalid_token}
    end.

get_jwt_conf() ->
    {ok, JwtKey} = gaby_conf:get('gaby.jwt.key'),
    {ok, JwtExpiration} = gaby_conf:get('gaby.jwt.expiration'),
    {ok, ?NOW_SEC(), list_to_binary(JwtKey), JwtExpiration}.
