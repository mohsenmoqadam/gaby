-module(gaby_os_messages).

-export([ reply/3
	, error/3
	]).

reply(Req0, Opts, OID) ->
    Resp = "{\"verb\": \"done\", \"oid\":\"" ++ OID ++ "\"}\n",
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, list_to_binary(Resp), Req0),
    {ok, Req1, Opts}.

error(Req0, Opts, ErrorCode) ->
    case ErrorCode of
	500 ->
	    Resp = "{\"verb\": \"error\", \"reason\": \"internal server error\"}\n",
	    Req1 = cowboy_req:reply(ErrorCode, #{<<"content-type">> => <<"application/json">>}, list_to_binary(Resp), Req0),
	    {ok, Req1, Opts};
	404 ->
	    Resp = "{\"verb\": \"error\", \"reason\": \"not found\"}\n",
	    Req1 = cowboy_req:reply(ErrorCode, #{<<"content-type">> => <<"application/json">>}, list_to_binary(Resp), Req0),
	    {ok, Req1, Opts};
	403 ->
	    Resp = "{\"verb\": \"error\", \"reason\": \"forbidden\"}\n",
	    Req1 = cowboy_req:reply(ErrorCode, #{<<"content-type">> => <<"application/json">>}, list_to_binary(Resp), Req0),	
	    {ok, Req1, Opts}
    end.
