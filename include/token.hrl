%% -*- mode:erlang -*-

-ifndef(HEADER_ROVER_TOKEN).
-define(HEADER_ROVER_TOKEN, true).

-record(token_info, { type :: token_type()
		    , is_expired :: true | false
		    , uid :: gaby_uid()
		    , aid :: gaby_aid()
		    }).

-type token_info() :: #token_info{}.
-type token_type() :: ast | ost.
-type token() :: bitstring().

-endif.
