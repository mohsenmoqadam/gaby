%% -*- mode:erlang -*-

-ifndef(HEADER_GABY).
-define(HEADER_GABY, true).

-type gaby_uid() :: non_neg_integer().
-type gaby_aid() :: non_neg_integer().

-define(NOW_SEC(), erlang:system_time(seconds)).
-define(NOW_MILLI(), erlang:system_time(milli_seconds)).
-define(NOW_MICRO(), erlang:system_time()).
-define(NOW(), erlang:system_time()).

-ifdef(TEST).
-define(LOG_ERROR(Format, Args), ct:print(default, 50, Format, Args)).
-define(LOG_INFO(Format, Args), ?LOG_ERROR(Format, Args)).
-define(LOG_DEBUG(Format, Args), ?LOG_ERROR(Format, Args)).
-else.
-define(LOG_ERROR(Format, Args), lager:error(Format, Args)).
-define(LOG_INFO(Format, Args), lager:info(Format, Args)).
-define(LOG_DEBUG(Format, Args), lager:debug(Format, Args)).
-endif.

-endif.
