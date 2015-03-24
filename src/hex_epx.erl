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
	 event_spec/1,
	 init_event/2,
	 mod_event/2,
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
%%  mod_event(Ref::reference(), Flags::[{atom(),term()}]) ->
%%     ok | {error, Reason}
%%
mod_event(Ref, Flags) ->
    hex_epx_server:mod_event(Ref, Flags).

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
validate_event(Dir, Flags) ->
    hex:validate_flags(Flags, event_spec(Dir)).

event_spec(_Dir) ->
    Number = {type,union,
	      [{type,int32,[]},
	       {type,decimal64,[{'fraction-digits',6,[]}]}]},
    [{leaf,type, 
      [{type, enumeration,
	[{enum,window,[]},
	 {enum,panel,[]},
	 {enum,button,[]},     %% spring-back
	 {enum,switch,[]},     %% on/off
	 {enum,slider,[]},
	 {enum,value,[]},
	 {enum,rectangle,[]},
	 {enum,ellipse,[]},
	 {enum,line,[]},
	 {enum,image,[]},
	 {enum,text,[]}]},
       {mandatory, true, []}
       ]},
     
     {leaf,id,
      [{type,string,[]},
       {mandatory, true, []}
      ]},

     {leaf,static,
      [{type,boolean,[]},
       {default,false,[]}
      ]},

     {'leaf-list',tabs,
      [{type,string,[]},
       {default,[],[]}]},
      
     {leaf,x,[{type,int32,[]},{default,0,[]}]},
     {leaf,y,[{type,int32,[]},{default,0,[]}]},

     {leaf,width,[{type,uint32,[]},{default,32,[]}]},
     {leaf,height,[{type,uint32,[]},{default,32,[]}]},

     {leaf,text,[{type,string,[]},{default,"",[]}]},
     
     {leaf,image,[{type,string,[]}]},

     {leaf,animation,[{type,string,[]}]},

     {leaf,animate,[
		    {type,enumeration,
		     [{enum,continuous,[]},
		      {enum,sequence,[]}]}]},

     {leaf,frame,[Number]}, 

     {leaf,fps,[Number]},

    {container,font,  %% fixme: match / #epx_font{}
      [{leaf,name,[{type,string,[]}]},
       {leaf,resolution,[{type,int32,[]}]},
       {leaf,weight,[{type,enumeration,
		      [{enum,none,[]},{enum,medium,[]},
		       {enum,bold,[]},{enum,demibold,[]}]}]},
       {leaf,slant,[{type,enumeration,
		     [{enum,roman,[]},{enum,italic,[]},
		      {enum,oblique,[]},{enum,reverse_italic,[]},
		      {enum,reverse_oblique,[]},{enum,other,[]}]}]},
       {leaf,size,[{type,uint32,[]}]}
      ]},

     {leaf,font_color,
      [{description, "Color in 0xAARRGGBB format or X11 name", []},
       {type,union,
	[{type,uint32,[]},
	 {type,string,[]}
	]},
       {default,16#00000000,[]}]},

     {leaf,color, 
      [{description, "Color in 0xAARRGGBB format or X11 name", []},
       {type,uint32,[]},
       {type,union,
	[{type,uint32,[]},
	 {type,string,[]}
	]},
       {default,16#ff000000,[]}]},

     {leaf,fill,[{type,enumeration,
		  [{enum,solid,[]},
		   {enum,blend,[]},
		   {enum,none,[]}]},
		 {default,none,[]}]},
     {leaf,halign,[{type,enumeration,
		    [{enum,left,[]},{enum,right,[]},{enum,center,[]}]},
		   {default,center,[]}]},
     {leaf,valign,[{type,enumeration,
		    [{enum,top,[]},{enum,bottom,[]},{enum,center,[]}]},
		   {default,center,[]}]},
     {leaf,min,[Number]},
     {leaf,max,[Number]},
     {leaf,value,[Number]},
     {leaf,format,[{type,string,[]},
		   {default,"~w",[]}]}

    ].
