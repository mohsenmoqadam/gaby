-module(gaby_os).

-export([ upload/3
	, download/4
	, init/0
	]).

-include("gaby.hrl").
-include("os.hrl").

init() ->
    Priv = code:priv_dir(gaby),
    DefaultAvatar = Priv ++ "/default_avatar/0",
    DST = final_path(?AVATAR) ++ "/0",
    {ok, _} = file:copy(DefaultAvatar, DST),
    ok.
upload(?GENERAL = Type, UploadRequest0, UID) ->
    %% 1. Make file names whith path.
    CRA = ?NOW_MICRO(),
    FileName = file_name(Type, CRA, UID),
    TmpFile = temp_path(Type) ++ "/" ++ FileName,
    FinalFile = final_path(Type) ++ "/" ++ FileName,

    %% 2. Save stream to temp file.
    {ok, UploadRequest1} = to_temp(UploadRequest0, TmpFile),

    %% 3. Move tmp file to final path.
    ok = to_final(TmpFile, FinalFile),

    %% 4. Make OID
    {ok, OID} = gaby_os_oid:encode(CRA, UID, Type),

    %% 5. Return OID.
    {ok, UploadRequest1, OID};
upload(?AVATAR = Type, UploadRequest0, UID) ->
    %% 1. Make file names whith path.
    CRA = ?NOW_MICRO(),
    FileName = file_name(Type, CRA, UID),
    TmpFile = temp_path(Type) ++ "/" ++ FileName,
    FinalFile = final_path(Type) ++ "/" ++ FileName,

    %% 2. Save stream to temp file.
    {ok, UploadRequest1} = to_temp(UploadRequest0, TmpFile),

    %% 3. Move tmp file to final path.
    ok = to_final(TmpFile, FinalFile),

    %% 4. Make OID
    {ok, OID} = gaby_os_oid:encode(CRA, UID, Type),

    %% 5. Return OID.
    {ok, UploadRequest1, OID};
upload(?ONBOARDING = Type, UploadRequest0, UID) ->
    %% 1. Make file names whith path.
    CRA = ?NOW_MICRO(),
    FileName = file_name(Type, CRA, UID),
    TmpFile = temp_path(Type) ++ "/" ++ FileName,
    FinalFile = final_path(Type) ++ "/" ++ FileName,

    %% 2. Save stream to temp file.
    {ok, UploadRequest1} = to_temp(UploadRequest0, TmpFile),

    %% 3. Move tmp file to final path.
    ok = to_final(TmpFile, FinalFile),

    %% 4. Make OID
    {ok, OID} = gaby_os_oid:encode(CRA, UID, Type),

    %% 5. Return OID.
    {ok, UploadRequest1, OID}.

download(DownloadRequest0, ?GENERAL = Type, CRA, UID) ->
    %% 1. Make File name.
    File = final_path(Type) ++ "/" ++ file_name(Type, CRA, UID),

    %% 2. Get file size.
    case  file:read_file_info(File) of
	{ok, #file_info{size = Size}} ->
	    %% 3. Send file.
	    DownloadRequest1 = cowboy_req:reply( 200
					       , #{<<"content-type">> => <<"image/*">>}
					       , {sendfile, 0, Size, File}
					       , DownloadRequest0),
	    {ok, DownloadRequest1};
	{error, _} ->
	    {error, not_exist}
    end;
download(DownloadRequest0, ?AVATAR = Type, Dummy, TUID) ->
    %% 1. Make File name.
    File = final_path(Type) ++ "/" ++ file_name(Type, Dummy, TUID),

    %% 2. Get file size.
    case  file:read_file_info(File) of
	{ok, #file_info{size = Size}} ->
	    %% 3. Send file.
	    DownloadRequest1 = cowboy_req:reply( 200
					       , #{<<"content-type">> => <<"image/*">>}
					       , {sendfile, 0, Size, File}
					       , DownloadRequest0),
	    {ok, DownloadRequest1};
	{error, _} ->
	    {error, not_exist}
    end;
download(DownloadRequest0, ?ONBOARDING = Type, CRA, UID) ->
    %% 1. Make File name.
    File = final_path(Type) ++ "/" ++ file_name(Type, CRA, UID),

    %% 2. Get file size.
    case  file:read_file_info(File) of
	{ok, #file_info{size = Size}} ->
	    %% 3. Send file.
	    DownloadRequest1 = cowboy_req:reply( 200
					       , #{<<"content-type">> => <<"image/*">>}
					       , {sendfile, 0, Size, File}
					       , DownloadRequest0),
	    {ok, DownloadRequest1};
	{error, _} ->
	    {error, not_exist}
    end.

%% =================================================================================
%% ========= Internal Functions
%% =================================================================================     
%% === Create File Name:
file_name(?GENERAL, CRA, UID) ->
    FileName = integer_to_list(UID) ++ "." ++ integer_to_list(CRA),
    FileName;
file_name(?AVATAR, _Dummy, UID) ->
    FileName = integer_to_list(UID),
    FileName;
file_name(?ONBOARDING, CRA, UID) ->
    FileName = integer_to_list(UID) ++ "." ++ integer_to_list(CRA),
    FileName.

%% === Get File Temp Path:
temp_path(?GENERAL) ->
    {ok, TempBasePath} = gaby_conf:get('gaby.temp.general.path'),
    TempBasePath;
temp_path(?AVATAR) ->
    {ok, TempBasePath} = gaby_conf:get('gaby.temp.avatar.path'),
    TempBasePath;
temp_path(?ONBOARDING) ->
    {ok, TempBasePath} = gaby_conf:get('gaby.temp.onboarding.path'),
    TempBasePath.

%% === Get File Final Path:
final_path(?GENERAL) ->
    {ok, FinalBasePath} = gaby_conf:get('gaby.final.general.path'),
    FinalBasePath;
final_path(?AVATAR) ->
    {ok, FinalBasePath} = gaby_conf:get('gaby.final.avatar.path'),
    FinalBasePath;
final_path(?ONBOARDING) ->
    {ok, FinalBasePath} = gaby_conf:get('gaby.final.onboarding.path'),
    FinalBasePath.

%% === Save File In Temp:
to_temp(UploadReq0, FileName) ->
    {ok, IoDevice} = file:open(FileName, [write]),
    {ok, UploadReq1} = write_to_file(UploadReq0, IoDevice),
    ok = file:close(IoDevice),
    {ok, UploadReq1}.

%% === Save File In File (temp):
write_to_file(UploadReq0, IoDevice) ->
    case cowboy_req:read_part_body(UploadReq0) of
        {ok, LastBodyChunk, Req} ->
            file:write(IoDevice, LastBodyChunk),
            {ok, Req};
        {more, BodyChunk, Req} ->
            ok = file:write(IoDevice, BodyChunk),
            write_to_file(Req, IoDevice)
    end.

%% === Save File In File (final):
to_final(SRC, DST) ->
    ok = file:rename(SRC, DST),
    ok.

