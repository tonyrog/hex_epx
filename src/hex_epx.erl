%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Hex epx plugin 
%%% @end
%%% Created :  7 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_epx).

-behaviour(hex_plugin).

-export([validate_event/2, 
	 init_event/2,
	 add_event/3, 
	 del_event/1, 
	 output/2]).

%%
%%  add_event(Flags::[{atom(),term()}, Signal::signal(), Cb::function()) ->    
%%     {ok, Ref:reference()} | {error, Reason}
%%
add_event(Flags, Signal, Cb) ->
    hex_epx_server:add_event(Flags, Signal, Cb).

%%
%%  del_event(Ref::reference()) ->
%%     ok.
del_event(Ref) ->
    hex_epx_server:del_event(Ref).

%%
%% output(Flags::[{atom(),term()}], Env::[{atom(),term()}]) ->
%%    ok.
%%
output(Flags, Env) ->
    hex_epx_server:output_event(Flags, Env).

%%
%% init_event(in | out, Flags::[{atom(),term()}])
%%
init_event(Dir, Flags) ->
    hex_epx_server:init_event(Dir, Flags).

%%
%% validate_event(in | out, Flags::[{atom(),term()}])
%%
validate_event(_Dir, Flags) ->
    hex:validate_flags(Flags, spec()).

spec() ->
    [{type,mandatory,
      {alt,[{const,button},{const,slider},{const,value},
	    {const,rectangle},{const,ellipse},{const,line},
	    {const,image},{const,text}]}, undefined},
     {id,mandatory,atom,undefined},
     {x,mandatory,integer,0},
     {y,mandatory,integer,0},
     {width,mandatory,unsigned,32},
     {height,mandatory,unsigned,32},
     {text,optional,string,""},
     {image,optional,{alt,[string,{record,epx_pixmap}]},undefined},
     {font,optional,
      {alt,[{list,{alt,[{tuple,[{const,name},string]},
			{tuple,[{const,resolution},integer]},
			{tuple,[{const,weight},
				{alt,[{const,none},{const,medium},
				      {const,bold},{const,demibold}]}]},
			{tuple,[{const,slant},
				{alt,[{const,roman},
				      {const,italic},
				      {const,oblique},
				      {const,reverse_italic},
				      {const,reverse_oblique},
				      {const,other}]}]},
			{tuple,[{const,size},integer]}]}},
	    {record,epx_font}]}, undefined},
     {color,optional,unsigned32,16#ff000000},
     {fill,optional,{alt,[{const,solid},{const,blend},{const,none}]}, none},
     {halign,optional,{alt,[{const,left},{const,right},{const,center}]},center},
     {valign,optional,{alt,[{const,top},{const,bottom},{const,center}]},center},
     {min,optional,number,undefined},
     {max,optional,number,undefined},
     {value,optional,number,0},
     {format,optional,string,"~w"}
    ].
