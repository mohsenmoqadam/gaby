-module(gaby_os_download_general).

-export([init/2]).

-include("gaby.hrl").
-include("token.hrl").
-include("os.hrl").

init(Req0, Opts) ->
    try
	{ ok 
	, #token_info{ type = ost
		     , is_expired = false
		     , uid = RUID
		     }
	} = gaby_token:decode(cowboy_req:binding(ost, Req0)),
	{ok, ?GENERAL, CRA, UID} = gaby_os_oid:decode(binary_to_list(cowboy_req:binding(oid, Req0))),
	case gaby_os:download(Req0, ?GENERAL, CRA, UID) of
	    {ok, Req1} ->
		?LOG_DEBUG("[GABY-OS] One General Image (Uploaded By: ~p) Downloaded By: ~p", [UID, RUID]),
		{ok, Req1, Opts};
	    {error, not_exist} ->
		gaby_os_messages:error(Req0, Opts, 404)
	end
    catch 
	_:_ ->
	    gaby_os_messages:error(Req0, Opts, 500)
    end.
