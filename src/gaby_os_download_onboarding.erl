-module(gaby_os_download_onboarding).

-export([init/2]).

-include("gaby.hrl").
-include("token.hrl").
-include("os.hrl").

init(Req0, Opts) ->
    try
	{ok, ?ONBOARDING, CRA, UID} = gaby_os_oid:decode(binary_to_list(cowboy_req:binding(oid, Req0))),
	case gaby_os:download(Req0, ?ONBOARDING, CRA, UID) of
	    {ok, Req1} ->
		?LOG_DEBUG("[GABY-OS] One Onboarding Image Downloaded.", []),
		{ok, Req1, Opts};
	    {error, not_exist} ->
		gaby_os_messages:error(Req0, Opts, 404)
	end
    catch 
	_:_ ->
	    gaby_os_messages:error(Req0, Opts, 500)
    end.
    
