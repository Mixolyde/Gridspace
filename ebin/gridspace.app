%%%-------------------------------------------------------------------
%%% File    : gridspace.app
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Gridspace app resource file
%%%
%%% Created :  10 Aug 2010 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------
%% This is the application resource file (.app file) for the 'base'
%% application.
{application, gridspace,
       [{description,  "A sci-fi mud written in Erlang" },
	{vsn,           "1.0" },
	{modules,       [gs_app, gs_super, gs_player_auth, gs_echo_server, gen_listener_tcp, gs_player_fsm,
		map_util, cellular_map_gen]},
	{registered,    [gs_super, gs_player_auth, gs_echo_server]},
	{applications,  [kernel,stdlib]},
	{mod,           {gs_app,[]}},
	{start_phases,  []}
       ]
}.