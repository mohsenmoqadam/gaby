-module(gaby_os_upload_onboarding).

-export([init/2]).

-include("gaby.hrl").
-include("token.hrl").
-include("os.hrl").

init(Req0, Opts) ->
    try
	case cowboy_req:parse_header(<<"content-type">>, Req0) of
	    {<<"multipart">>, <<"form-data">>, _} -> 
		{ ok
		, #token_info{ type = ost
			     , is_expired = false
			     , uid = UID
			     }
		} = gaby_token:decode(cowboy_req:binding(ost, Req0)),
		{ok, Req1, OID} = do_store(Req0, UID),
		?LOG_DEBUG("[GABY-OS] One Onboarding Image Uploaded By: ~p", [UID]), 
		gaby_os_messages:reply(Req1, Opts, OID);
	    _Else -> 
		gaby_os_messages:error(Req0, Opts, 403)
	end
    catch
	_:_ ->
	    gaby_os_messages:error(Req0, Opts, 500)
    end.

do_store(Req0, UID) ->
    {ok, Headers, Req1} = cowboy_req:read_part(Req0),
    {file, _FieldName, _Filename, _CType} = cow_multipart:form_data(Headers),
    {ok, Req2, OID} = gaby_os:upload(?ONBOARDING, Req1, UID),

    {ok, Req2, OID}.
