%%%-------------------------------------------------------------------
%% @doc gaby public API
%% @end
%%%-------------------------------------------------------------------

-module(gaby_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ok = application:ensure_started(ranch),
    ok = application:ensure_started(cowlib),
    ok = application:ensure_started(cowboy),
    application:ensure_all_started(inets),

    %% === Start Object Storage Pool
    {ok, OsIp0} = gaby_conf:get('gaby.host'),
    {ok, OsIp} = inet:parse_ipv4_address(OsIp0),
    {ok, OsPort} = gaby_conf:get('gaby.port'),
    {ok, _OsPoolSize} = gaby_conf:get('gaby.pool.size'),
    %% --- General Images Configurations:
    {ok, OsUploadGeneralPath} = gaby_conf:get('gaby.upload.general.path'),
    {ok, OsDownloadGeneralPath} = gaby_conf:get('gaby.download.general.path'),
    {ok, TemGeneralPath} = gaby_conf:get('gaby.temp.general.path'),
    {ok, FinalGeneralPath} = gaby_conf:get('gaby.final.general.path'),
    %% --- Avatar Images Configurations:
    {ok, OsUploadAvatarPath} = gaby_conf:get('gaby.upload.avatar.path'),
    {ok, OsDownloadAvatarPath} = gaby_conf:get('gaby.download.avatar.path'),
    {ok, TemAvatarPath} = gaby_conf:get('gaby.temp.avatar.path'),
    {ok, FinalAvatarPath} = gaby_conf:get('gaby.final.avatar.path'),
    %% --- Onboarding Images Configurations:
    {ok, OsUploadOnboardingPath} = gaby_conf:get('gaby.upload.onboarding.path'),
    {ok, OsDownloadOnboardingPath} = gaby_conf:get('gaby.download.onboarding.path'),
    {ok, TemOnboardingPath} = gaby_conf:get('gaby.temp.onboarding.path'),
    {ok, FinalOnboardingPath} = gaby_conf:get('gaby.final.onboarding.path'),
    %% --- Create path (if not exist):
    ok = filelib:ensure_dir(TemGeneralPath ++ "/"),
    ok = filelib:ensure_dir(FinalGeneralPath ++ "/"),
    ok = filelib:ensure_dir(TemAvatarPath ++ "/"),
    ok = filelib:ensure_dir(FinalAvatarPath ++ "/"),
    ok = filelib:ensure_dir(TemOnboardingPath ++ "/"),
    ok = filelib:ensure_dir(FinalOnboardingPath ++ "/"),
    ok = gaby_os:init(),
    %% --- Initialize OS
    OsDispatch = cowboy_router:compile(
		   [{'_', [ {OsUploadGeneralPath, gaby_os_upload_general, []}
			  , {OsDownloadGeneralPath, gaby_os_download_general, []}
			  , {OsUploadAvatarPath, gaby_os_upload_avatar, []}
			  , {OsDownloadAvatarPath, gaby_os_download_avatar, []}
			  , {OsUploadOnboardingPath, gaby_os_upload_onboarding, []}
			  , {OsDownloadOnboardingPath, gaby_os_download_onboarding, []}
			  ]
		    }]),
    {ok, _} = cowboy:start_clear(object_storage, [{ip, OsIp}, {port, OsPort}],
                                 #{env => #{dispatch => OsDispatch}}),

    gaby_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
