%%%-------------------------------------------------------------------
%%% File    : time_util.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Time managing utility functions. Mostly for convert-
%%%   stamps to/from microseconds and display formatting
%%%
%%% Created :  03 Feb 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(time_util).
-author("Brian E. Williams").
-compile([debug_info, export_all]).

now_stamp_us({MegaSecs,Secs,MicroSecs}) -> 
        (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

now_us() -> now_stamp_us(erlang:now()).
