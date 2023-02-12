-module(gaby_os_download_avatar).

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
	TUID = binary_to_integer(cowboy_req:binding(uid, Req0)),
	case gaby_os:download(Req0, ?AVATAR, dummy, TUID) of
	    {ok, Req1} ->
		?LOG_DEBUG("[GABY-OS] One Avatar Image (Owner: ~p) Downloaded By: ~p", [TUID, RUID]),
		{ok, Req1, Opts};
	    {error, not_exist} ->
		gaby_os_messages:error(Req0, Opts, 404)
	end
    catch 
	_:_ ->
	    gaby_os_messages:error(Req0, Opts, 500)
    end.
    
