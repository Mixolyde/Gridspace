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
	  {modules,       [gs_app, gs_super, 
      gs_player_db, 
      gs_telnet_server, gs_telnet_client, gs_gen_listener_tcp, 
      gs_login_fsm,
      gs_command_parser, gs_command_executor, gs_prompt]},
  	{registered,    [gs_super, gs_player_db, gs_telnet_server]},
  	{applications,  [kernel,stdlib]},
  	{mod,           {gs_app,[]}},
  	{start_phases,  []}
  ]
}.
