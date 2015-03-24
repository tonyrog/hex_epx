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
%%%    epx test GUI server for hex
%%% @end
%%% Created :  7 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------

-module(hex_epx_server).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/0]).
-export([add_event/3, mod_event/2, del_event/1]).
-export([init_event/2, output_event/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("epx/include/epx.hrl").
-include_lib("epx/include/epx_image.hrl").

-define(SERVER, ?MODULE).
-define(DICT_T, term()).  %% dict:dict()
-define(SETS_T, term()).  %% sets:set()

%% FIXME: configure this
-define(DEFAULT_WIDTH, 320).
-define(DEFAULT_HEIGHT, 240).

-define(is_string(Cs), is_list((Cs))).

-record(widget,
	{
	  id,                %% named widgets
	  type,              %% button,rectangle,slider ...
	  window = screen,   %% id of base window (if type != window)
	  state  = normal,   %% or active,selected,closed ..
	  static = false,    %% object may not be deleted
	  x = 0   :: integer(),
	  y = 0   :: integer(),
	  width  = 32 :: non_neg_integer(),
	  height = 32 :: non_neg_integer(),
	  text = "",
	  border  :: number(),
	  orientation = horizontal :: horizontal|vertical,
	  image   :: epx:epx_pixmap(),
	  image2   :: epx:epx_pixmap(),
	  topimage :: epx:epx_pixmap(),
	  animation :: epx:epx_animation(),
	  animation2 :: epx:epx_animation(),
	  frame :: number(),
	  color = 16#ff000000,
	  color2,
	  font_color = 16#00000000,
	  fill   = none :: epx:epx_fill_style(),
	  events = []   :: epx:epx_window_event_flags(),
	  halign  = center :: top|bottom|center,
	  valign  = center :: left|right|center,
	  min     :: number(),          %% type=value|slider
	  max     :: number(),          %% type=value|slider
	  format = "~w" :: string(),    %% io_lib:format format
	  value =0 :: number(),         %% type=value|slider
	  animate,                      %% animation state.
	  font    :: epx:epx_font(),    %% type=text|button|value
	  win     :: epx:epx_window(),  %% type = window
	  backing :: epx:epx_pixmap()
	}).

-record(sub,
	{
	  ref :: reference(),
	  mon :: reference(),
	  id  :: term(),
	  callback :: atom() | function(),
	  signal :: term()
	}).
	  
-record(state, {
	  joined :: boolean(),       %% joined hex server
	  redraw_timer = undefined,
	  active = [] :: [term()],   %% active widgets pressed
	  subs = [] :: [#sub{}],
	  default_font :: epx:epx_font(),
	  wset    :: ?SETS_T,  %% set of window id
	  widgets :: ?DICT_T   %% term => #widget{}
	 }).

add_event(Flags, Signal, Cb) ->
    gen_server:call(?MODULE, {add_event, self(), Flags, Signal, Cb}).

del_event(Ref) ->
    gen_server:call(?MODULE, {del_event, Ref}).

output_event(Flags, Env) ->
    gen_server:call(?MODULE, {output_event, Flags, Env}).

init_event(Dir, Flags) ->
    gen_server:call(?MODULE, {init_event, Dir, Flags}).

mod_event(Dir, Flags) ->
    gen_server:call(?MODULE, {mod_event, Dir, Flags}).

stop() ->
    gen_server:call(?MODULE, stop).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Options) ->
    hex:start_all(lager),
    application:start(epx),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    Joined = hex:auto_join(hex_epx),
    Backend = epx_backend:default(),
    Name = epx:backend_info(Backend, name),
    Width =
	if Name =:= "fb" ->
		case proplists:get_value(width, Args) of
		    undefined -> epx:backend_info(Backend, width);
		    W0 -> W0
		end;
	   true ->
		proplists:get_value(width, Args, ?DEFAULT_WIDTH)
	end,
    Height =
	if Name =:= "fb" ->
		case proplists:get_value(height, Args) of
		    undefined -> epx:backend_info(Backend, height);
		    H0 -> H0
		end;
	   true ->
		proplists:get_value(height, Args, ?DEFAULT_HEIGHT)
	end,
    Font = %% load a default font
	case epx_font:match([{name,"Arial"},{size,12}]) of
	    false -> undefined;
	    {ok,F} -> F
	end,
    Default = window_create([{id,screen},
			     {static,true},
			     {x,50},{y,50},
			     {width,Width},{height,Height},
			     {events, [key_press,key_release,
				       button_press, button_release,
				       %% configure,resize,focus,
				       %% crossing, motion
				       button,wheel]},
			     {color, 16#ffffffff}]),
    self() ! refresh,
    {ok, #state{ joined = Joined,
		 default_font = Font,
		 widgets = dict:from_list([{Default#widget.id,Default}]),
		 wset = sets:from_list([Default#widget.id])
	       }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_event,Pid,Flags,Signal,Cb}, _From, State) ->
    case lists:keyfind(id, 1, Flags) of
	false ->
	    {reply,{error,missing_id},State};
	{id,ID} ->
	    Ref = erlang:monitor(process, Pid),
	    Sub = #sub{id=ID,ref=Ref,signal=Signal,callback=Cb},
	    Subs = [Sub|State#state.subs],
	    {reply, {ok,Ref}, State#state { subs=Subs}}
    end;
handle_call({del_event,Ref}, _From, State) ->
    case lists:keytake(Ref, #sub.ref, State#state.subs) of
	false ->
	    {reply, {error, not_found}, State};
	{value, Sub, Subs} ->
	    erlang:demonitor(Sub#sub.ref, [flush]),
	    {reply, ok, State#state { subs=Subs} }
    end;
handle_call({output_event,Flags,Env}, _From, State) ->
    case lists:keyfind(id, 1, Flags) of
	false ->
	    {reply,{error,missing_id},State};
	{id,ID} ->
	    case dict:find(ID, State#state.widgets) of
		error ->
		    {reply,{error,enoent},State};
		{ok,W} ->
		    try widget_set(Flags++Env, W) of
			W1 ->
			    Ws1 = dict:store(ID,W1,State#state.widgets),
			    self() ! refresh,
			    {reply, ok, State#state{widgets=Ws1}}
		    catch
			error:Reason ->
			    {reply, {error,Reason}, State}
		    end
	    end
    end;
handle_call({init_event,_Dir,Flags}, _From, State) ->
    case lists:keyfind(id, 1, Flags) of
	false ->
	    {reply,{error,missing_id},State};
	{id,ID} ->
	    case dict:find(ID,State#state.widgets) of
		error ->
		    W0 = #widget{font=State#state.default_font},
		    try widget_set(Flags,W0) of
			W1 ->
			    %% fixme: handle extra windows
			    Ws1 = dict:store(ID,W1,State#state.widgets),
			    self() ! refresh,
			    {reply, ok, State#state{widgets=Ws1}}
		    catch
			error:Reason ->
			    io:format("widget ~p not created ~p\n",
				      [ID, Reason]),
			    {reply, {error,Reason}, State}
		    end;
		{ok,W} ->
		    try widget_set(Flags,W) of
			W1 ->
			    Ws1 = dict:store(ID,W1,State#state.widgets),
			    self() ! refresh,
			    {reply, ok, State#state{widgets=Ws1}}
		    catch
			error:Reason ->
			    io:format("widget ~p not updated ~p\n",
				      [W#widget.id, Reason]),
			    {reply, {error,Reason}, State}
		    end
	    end
    end;
handle_call({mod_event,_Dir,Flags}, _From, State) ->
    case lookup_widget(Flags, State) of
	E={error,_} ->
	    {reply,E, State};
	{ok,W} ->
	    try widget_set(Flags, W) of
		W1 ->
		    Ws1 = dict:store(W#widget.id,W1,State#state.widgets),
		    self() ! refresh,
		    {reply, ok, State#state{widgets=Ws1}}
	    catch
		error:Reason ->
		    {reply, {error,Reason}, State}
	    end
    end;
handle_call(_Request, _From, State) ->
    lager:debug("unknown call ~p", [_Request]),
    {reply, {error,bad_call}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({epx_event,Win,Event}, State) ->
    lager:debug("event: ~p", [Event]),
    %% find window widget (fixme: add reverse map at some point) ?
    case fold_windows(
	   fun(W,Acc) ->
		   if W#widget.win =:= Win -> [W|Acc];
		      true -> Acc
		   end
	   end, [], State) of
	[] ->
	    lager:error("window not found"),
	    {noreply, State};
	[W] ->  %% should only be one!
	    handle_event(Event, W, State)
    end;
handle_info({timeout,_Ref,{animate,ID,Anim}}, State) ->
    case dict:find(ID, State#state.widgets) of
	error -> {noreply, State};
	{ok,W} ->
	    W1 = widget_animate_run(W, Anim),
	    Ws = dict:store(ID,W1,State#state.widgets),
	    State1 = State#state { widgets = Ws },
	    State2 = redraw_schedule(State1),
	    {noreply,State2}
    end;
handle_info(refresh, State) ->
    {noreply, redraw_schedule(State)};

handle_info({timeout,TRef,redraw}, State) 
  when TRef =:= State#state.redraw_timer ->
    lager:debug("redraw"),
    State1 = redraw_state(State#state { redraw_timer=undefined}),
    {noreply, State1};

handle_info({'DOWN',Ref,process,_Pid,_Reason}, State) ->
    case lists:keytake(Ref, #sub.ref, State#state.subs) of
	false ->
	    {noreply, State};
	{value, _Sub, Subs} ->
	    {noreply, State#state { subs=Subs} }
    end;
handle_info(_Info, State) ->
    lager:debug("info = %p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    each_window(fun unmap_window/1, State),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

lookup_widget(Flags, State) ->
    case lists:keyfind(id, 1, Flags) of
	false ->
	    {error,missing_id};
	{id,ID} ->
	    case dict:find(ID, State#state.widgets) of
		error ->
		    {error,enoent};
		{ok,W} ->
		    {ok,W}
	    end
    end.


handle_event({key_press,_Sym,_Mod,_Code},_W,State) ->
    {noreply, State};
handle_event({key_release,_Sym,_Mod,_Code},_W,State) ->
    {noreply, State};
handle_event(Event={button_press,Button,{X,Y,_}},Window,State) ->
    case lists:member(left,Button) of
	true ->
	    %% locate an active widget at position (X,Y)
	    WinID = Window#widget.id,
	    Ws = State#state.widgets,
	    case widgets_at_location(Ws,X,Y,WinID) of
		[] ->
		    {noreply, State};
		[W|_] ->  %% z-sort ?
		    case widget_event(Event, W, Window, State) of
			W -> 
			    {noreply, State};
			W1 ->
			    ID = W1#widget.id,
			    Active = [ID | State#state.active],
			    Ws1 = dict:store(ID, W1, Ws),
			    {noreply, State#state { active = Active,
						    widgets = Ws1}}
		    end
	    end;
	false ->
	    {noreply, State}
    end;
handle_event(Event={button_release,Button,{_X,_Y,_}},Window,State) ->
    case lists:member(left,Button) of
	true ->
	    %% release "all" active widgets
	    State1 = 
		lists:foldl(
		  fun(ID, Si) ->
			  Ws = Si#state.widgets,
			  case dict:find(ID, Ws) of
			      error -> Si;
			      {ok,W} ->
				  case widget_event(Event, W, Window, Si) of
				      W -> Si; %% no changed
				      W1 ->
					  Ws1 = dict:store(W1#widget.id,W1,Ws),
					  Si#state { widgets = Ws1}
				  end
			  end
		  end, State, State#state.active),
	    {noreply, State1#state { active = [] }};
	false ->
	    {noreply, State}
    end;
handle_event(Event={motion,Button,{X,Y,_}},Window,State) ->
    case lists:member(left,Button) of
	true ->
	    %% locate an active widget at position (X,Y)
	    WinID = Window#widget.id,
	    Ws = State#state.widgets,
	    case widgets_at_location(Ws,X,Y,WinID) of
		[] ->
		    {noreply, State};
		[W|_] ->
		    case widget_event(Event, W, Window, State) of
			W -> {noreply, State}; %% no changed
			W1 ->
			    Ws1 = dict:store(W1#widget.id, W1, Ws),
			    {noreply, State#state { widgets = Ws1}}
		    end
	    end;
	false ->
	    {noreply, State}
    end;

handle_event({configure,{_X,_Y,_Width,_Height}},_Window,State) ->
    {noreply, State};
handle_event({resize,{_Width,_Height,_Depth}},_Window,State) ->
    {noreply, State};
handle_event({enter, _Where},_Window,State) ->
    {noreply, State};
handle_event({leave, _Where},_Window,State) ->
    {noreply, State};
handle_event(focus_in,_Window,State) ->
    {noreply, State};
handle_event(focus_out,_Window,State) ->
    {noreply, State};
handle_event(Event=close,Window,State) ->
    if Window#widget.static =:= true -> %% may not be deleted
	    {noreply, State};
       true ->
	    unmap_window(Window),
	    Window1 = widget_event(Event, Window, Window, State),
	    State1 = delete_widget(Window1, State),
	    {noreply, State1}
    end;
handle_event(_Event,_W,State) ->
    lager:error("unknown event: ~p", [_Event]),
    {noreply, State}.

%%
%% Find all widgets in window WinID that is hit by the 
%% point (X,Y).
%%
widgets_at_location(Ws,X,Y,WinID) ->
    dict:fold(
      fun(_ID,W,Acc) ->
	      if W#widget.window =:= WinID,
		 X >= W#widget.x, Y >= W#widget.y,
		 X =< W#widget.x + W#widget.width - 1,
		 Y =< W#widget.y + W#widget.height - 1 ->
		      [W|Acc];
		 W#widget.window =:= WinID ->
		      case topimage_at_location(W,X,Y,W#widget.topimage) of
			  true -> 
			      [W|Acc];
			  false -> 
			      case image_at_location(W,X,Y,W#widget.image) of
				  true ->
				      [W|Acc];
				  false ->
				      case animation_at_location(W,X,Y,W#widget.animation) of
					  true ->
					      [W|Acc];
					  false ->
					      Acc
				      end
			      end
		      end;
		 true ->
		      Acc
	      end
      end, [], Ws).

topimage_at_location(_W,_X,_Y,undefined) ->
    false;
topimage_at_location(W=#widget {orientation = horizontal},X,Y,Image) ->
    Height = epx:pixmap_info(Image,height),
    Y1 = W#widget.y + W#widget.height div 2 - Height div 2,
    Y2 = W#widget.y + W#widget.height div 2 + Height div 2,
    if X >= W#widget.x, Y >= Y1,
       X =< W#widget.x + W#widget.width - 1, Y =< Y2 ->
	    true;
       true ->
	    false
    end;
topimage_at_location(W=#widget {orientation = vertical},X,Y,Image) ->
    Width = epx:pixmap_info(Image,width),
    X1 = W#widget.x + W#widget.width div 2 - Width div 2,
    X2 = W#widget.x + W#widget.width div 2 + Width div 2,
    if Y >= W#widget.y, X >= X1,
       Y =< W#widget.y + W#widget.height - 1, X =< X2 ->
	    true;
       true ->
	    false
    end.

image_at_location(_W,_X,_Y,undefined) ->
    false;
image_at_location(W,X,Y,Image) ->
    Height = epx:pixmap_info(Image,height),
    Width = epx:pixmap_info(Image,width),
    if X >= W#widget.x, Y >= W#widget.y,
       X =< W#widget.x + Width - 1, 
       Y =< W#widget.y + Height - 1 ->
	    true;
       true ->
	    false
    end.

animation_at_location(_W,_X,_Y,undefined) ->
    false;
animation_at_location(W,X,Y,Anim) ->
    Height = epx:animation_info(Anim,height),
    Width = epx:animation_info(Anim,width),
    if X >= W#widget.x, Y >= W#widget.y,
       X =< W#widget.x + Width - 1, 
       Y =< W#widget.y + Height - 1 ->
	    true;
       true ->
	    false
    end.
      

%% generate a callback event and start animate the button
widget_event({button_press,_Button,Where}, W, Window, State) ->
    case W#widget.type of
	button ->
	    callback_all(W#widget.id, State#state.subs, [{value,1}]),
	    widget_animate_begin(W#widget { state=active }, press);
	switch ->
	    {WState,Value} = 
		case W#widget.state of
		    active -> {normal,0};
		    _ -> {active,1}
		end,
	    callback_all(W#widget.id, State#state.subs, [{value,Value}]),
	    widget_animate_begin(W#widget { state=WState }, press);
	slider ->
	    {X,Y,_} = Where,
	    case widget_slider_value(W, X, Y) of
		{ok,Value} ->
		    epx:window_enable_events(Window#widget.win, [motion]),
		    callback_all(W#widget.id,State#state.subs,[{value,Value}]),
		    self() ! refresh,
		    W#widget { state=active, value = Value };
		false ->
		    lager:debug("slider min/max/width error"),
		    W
	    end;
	_ ->
	    W
    end;
widget_event({button_release,_Button,_Where}, W, Window, State) ->
    case W#widget.type of
	button ->
	    callback_all(W#widget.id, State#state.subs, [{value,0}]),
	    widget_animate_begin(W#widget{state=normal}, release);
	switch ->
	    W;
	slider ->
	    epx:window_disable_events(Window#widget.win, [motion]),
	    W#widget{state=normal};
	_ ->
	    W
    end;
widget_event({motion,_Button,Where}, W, _Window, State) ->
    case W#widget.type of
	slider ->
	    {X,Y,_} = Where,
	    case widget_slider_value(W, X, Y) of
		{ok,Value} ->
		    callback_all(W#widget.id,State#state.subs,[{value,Value}]),
		    self() ! refresh,
		    W#widget { value = Value };
		false ->
		    W
	    end;
	_ ->
	    W
    end;
widget_event(close, W, _Window, State) ->
    callback_all(W#widget.id,State#state.subs,[{closed,true}]),
    W#widget { state=closed };
widget_event(_Event, W, _Window, _State) ->
    W.

widget_slider_value(W=#widget {min=Min, max=Max, orientation=horizontal}, X, _Y) ->
%% given x coordinate calculate the slider value
    Width = W#widget.width-2,
    if is_number(Min), is_number(Max), Width > 0 ->
	    X0 = W#widget.x+1,
	    X1 = X0 + Width - 1,
	    Xv = clamp(X, X0, X1),
	    R = (Xv - X0) / (X1 - X0),
	    {ok,trunc(Min + R*(Max - Min))};
       true ->
	    false
    end;
widget_slider_value(W=#widget {min=Min, max=Max, orientation=vertical}, _X, Y) ->
%% given y coordinate calculate the slider value
    Height = W#widget.height-2,
    if is_number(Min), is_number(Max), Height > 0 ->
	    Y1 = W#widget.y+1,
	    Y0 = Y1 + Height - 1,
	    Yv = clamp(Y, Y1, Y0),
	    R = (Y0 - Yv) / (Y0 - Y1),
	    {ok,trunc(Min + R*(Max - Min))};
       true ->
	    false
    end.


widget_animate_begin(W, press) ->
    lager:debug("animate_begin: down"),
    F = case W#widget.state of %% fixme: run several frames if present
	    active -> 1;
	    _ -> 0
	end,
    self() ! refresh,
    W#widget { frame = F, animate = {color,{sub,16#00333333}} };
widget_animate_begin(W, release) ->
    lager:debug("animate_begin: up"),
    self() ! refresh,
    F = case W#widget.state of
	    active -> 1;
	    _ -> 0
	    end,
    W#widget { frame=F, animate = undefined };
widget_animate_begin(W, flash) ->
    lager:debug("animate_begin"),
    Anim = {flash,0,10},
    erlang:start_timer(0, self(),{animate,W#widget.id,Anim}),
    W#widget { animate = {color,{interpolate,0.0,16#00ffffff}}}.

widget_animate_end(W, flash) ->
    lager:debug("animate_end"),
    W#widget { animate = undefined }.

widget_animate_run(W, {flash,N,N}) ->
    widget_animate_end(W, flash);
widget_animate_run(W, {flash,I,N}) ->
    lager:debug("animate flash ~w of ~w", [I, N]),
    Anim = {flash,I+1,N},
    erlang:start_timer(30, self(),{animate,W#widget.id,Anim}),
    case W#widget.animate of
	{color,{interpolate,_V,AColor}} ->
	    W#widget { animate = {color,{interpolate,(I+1)/N,AColor}}};
	_ ->
	    W
    end.

callback_all(Wid, Subs, Env) ->
    lists:foreach(
      fun(#sub{id=ID,signal=Signal,callback=Callback}) ->
	      if ID =:= Wid ->
		      callback(Callback,Signal,Env);
		 true ->
		      ok
	      end
      end, Subs).

%% note that event signals may loopback and be time consuming,
%% better to spawn them like this.
callback(undefined,_Signal,_Env)  ->
    ok;
callback(Cb,Signal,Env) when is_atom(Cb) ->
    spawn(fun() -> Cb:event(Signal, Env) end);
callback(Cb,Signal,Env) when is_function(Cb, 2) ->
    spawn(fun() -> Cb(Signal,Env) end).

window_create(Flags) ->
    W = widget_set([{type,window}|Flags], #widget{}),
    Win = epx:window_create(W#widget.x, W#widget.y,
			    W#widget.width, W#widget.height, 
			    W#widget.events),
    epx:window_attach(Win),
    Backing = epx:pixmap_create(W#widget.width, W#widget.height),
    epx:pixmap_attach(Backing),
    Px = epx:pixmap_create(W#widget.width, W#widget.height),
    W#widget { win=Win, image=Px, backing=Backing }.


widget_set([Option|Flags], W) ->
    case Option of
	{type,Type} when is_atom(Type) -> 
	    widget_set(Flags, W#widget{type=Type});
	{id,ID} when is_atom(ID); ?is_string(ID) -> 
	    widget_set(Flags, W#widget{id=ID});
	{static,Bool} when is_boolean(Bool) -> 
	    widget_set(Flags, W#widget{static=Bool});
	{x,X} when is_integer(X) ->
	    widget_set(Flags, W#widget{x=X});
	{y,Y} when is_integer(Y) ->
	    widget_set(Flags, W#widget{y=Y});
	{width,Width} when is_integer(Width), Width>=0 ->
	    widget_set(Flags, W#widget{width=Width});
	{height,Height} when is_integer(Height), Height>=0 ->
	    widget_set(Flags, W#widget{height=Height});
	{text,Text} when is_list(Text) ->
	    widget_set(Flags, W#widget{text=Text});
	{border, Border} when is_integer(Border) ->
	    widget_set(Flags, W#widget{border=Border});
	{orientation, O} when is_atom(O) ->
	    widget_set(Flags, W#widget{orientation = O});
	{image,File} when is_list(File) ->
	    case epx_image:load(hex:text_expand(File, [])) of
		{ok,Image} ->
		    lager:debug("load image file ~s.",[File]),
		    case Image#epx_image.pixmaps of
			[Pixmap] ->
			    lager:debug("pixmap created ~s.",[File]),
			    widget_set(Flags, W#widget{image=Pixmap});
			_ ->
			    lager:error("no pixmap found in ~s",[File]),
			    widget_set(Flags, W)
		    end;
		Error ->
		    lager:error("unable to load image file ~s:~p",
				[File,Error]),
		    widget_set(Flags, W)
	    end;
	{image,Image} when is_record(Image,epx_pixmap) ->
	    widget_set(Flags, W#widget{image=Image});
	{topimage,File} when is_list(File) ->
	    case epx_image:load(hex:text_expand(File, [])) of
		{ok,Image} ->
		    lager:debug("load image file ~s.",[File]),
		    case Image#epx_image.pixmaps of
			[Pixmap] ->
			    lager:debug("pixmap created ~s.",[File]),
			    widget_set(Flags, W#widget{topimage=Pixmap});
			_ ->
			    lager:error("no pixmap found in ~s",[File]),
			    widget_set(Flags, W)
		    end;
		Error ->
		    lager:error("unable to load image file ~s:~p",
				[File,Error]),
		    widget_set(Flags, W)
	    end;
	{topimage,Image} when is_record(Image,epx_pixmap) ->
	    widget_set(Flags, W#widget{topimage=Image});
	{animation, File} when is_list(File) ->
	    try epx:animation_open(hex:text_expand(File, [])) of 
		Anim ->
		    lager:debug("open animation file ~s.",[File]),
		    widget_set(Flags, W#widget{animation = Anim})	   
	    catch
		error:_Reason ->
		    lager:error("unable to open animation file ~s:~p",
				[File,_Reason]),
		    widget_set(Flags, W)
	    end;	    
	{frame, Frame} when is_integer(Frame) ->
	    widget_set(Flags, W#widget{frame=Frame});
	{font, Spec} when is_list(Spec) ->
	    case epx_font:match(Spec) of
		false ->
		    lager:error("unable to load font ~p", [Spec]),
		    widget_set(Flags, W);
		{ok,Font} ->
		    widget_set(Flags, W#widget{font=Font})
	    end;
	{font,Font} when is_record(Font,epx_font) ->
	    widget_set(Flags, W#widget{font=Font});
	{color,Color} when is_integer(Color), Color >= 0 ->
	    widget_set(Flags, W#widget{color=Color});
	{color,ColorName} when is_list(ColorName) ->
	    case epx_color:from_name(ColorName) of
		false ->
		    lager:error("no such color ~s", [ColorName]),
		    widget_set(Flags, W);
		{R,G,B} ->
		    Color = (255 bsl 24)+(R bsl 16)+(G bsl 8)+B,
		    widget_set(Flags, W#widget{color=Color})
	    end;
	{color2,Color} when is_integer(Color), Color >= 0 ->
	    widget_set(Flags, W#widget{color2=Color});
	{color2,ColorName} when is_list(ColorName) ->
	    case epx_color:from_name(ColorName) of
		false ->
		    lager:error("no such color ~s", [ColorName]),
		    widget_set(Flags, W);
		{R,G,B} ->
		    Color = (255 bsl 24)+(R bsl 16)+(G bsl 8)+B,
		    widget_set(Flags, W#widget{color2=Color})
	    end;
	{font_color,Color} when is_integer(Color), Color>=0 ->
	    widget_set(Flags, W#widget{font_color=Color});
	{font_color,ColorName} when is_list(ColorName) ->
	    case epx_color:from_name(ColorName) of
		false ->
		    lager:error("no such text color ~s", [ColorName]),
		    widget_set(Flags, W);
		{R,G,B} ->
		    Color = (R bsl 16)+(G bsl 8)+B,
		    widget_set(Flags, W#widget{font_color=Color})
	    end;
	{fill, Style} when is_atom(Style) ->
	    widget_set(Flags, W#widget{fill=Style});
	{events,Es} when is_list(Es) ->
	    widget_set(Flags, W#widget{events=Es});
	{halign,A} when A =:= left;
			A =:= right;
			A =:= center->
	    widget_set(Flags, W#widget{halign=A});
	{valign,A} when A =:= top;
			A =:= bottom;
			A =:= center->
	    widget_set(Flags, W#widget{valign=A});
	{min,Min} when is_number(Min) ->
	    V = clamp(W#widget.value, Min, W#widget.max),
	    widget_set(Flags, W#widget{value=V,min=Min});
	{max,Max} when is_number(Max) ->
	    V = clamp(W#widget.value, W#widget.min, Max),
	    widget_set(Flags, W#widget{value=V,max=Max});
	{value,V} when is_number(V) ->
	    V1 = clamp(V, W#widget.min, W#widget.max),
	    widget_set(Flags, W#widget{value=V1});
	{format,F} when is_list(F) ->
	    widget_set(Flags, W#widget{format=F});
	_ ->
	    lager:debug("option ignored ~p", [Option]),
	    widget_set(Flags, W)
    end;
widget_set([], W) ->
    W.

redraw_schedule(State) ->
    if is_reference(State#state.redraw_timer) ->
	    State;
       State#state.redraw_timer =:= undefined ->
	    Timer = erlang:start_timer(50, self(), redraw),
	    State#state { redraw_timer = Timer }
    end.

redraw_state(State) ->
    each_window(fun clear_window/1, State),
    each_widget(fun(W) ->
			case dict:find(W#widget.window, State#state.widgets) of
			   error ->
			       lager:error("missing window id=~w\n",
					   [W#widget.window]);
			    {ok,Win} ->
				draw_widget(W, Win)
			end
		end, State),
    each_window(fun update_window/1, State),
    State.

delete_widget(#widget{id=Wid,type=Type}, State) ->
    Widgets1 = dict:erase(Wid, State#state.widgets),
    Wset1 = if Type =:= window ->
		    sets:del_delement(Wid,State#state.wset);
	       true ->
		    State#state.wset
	    end,
    State#state { widgets=Widgets1, wset=Wset1 }.
    

fold_widgets(Fun, Acc, State) ->
    dict:fold(fun(_K,W,Acc1) -> Fun(W,Acc1) end, Acc, State#state.widgets).

each_widget(Fun, State) ->
    fold_widgets(fun(W,_) -> Fun(W) end, ok, State),
    ok.

fold_windows(Fun, Acc, State) ->
    sets:fold(fun(Wid,Acc1) ->
		      W = dict:fetch(Wid, State#state.widgets),
		      Fun(W, Acc1)
	      end, Acc, State#state.wset).

each_window(Fun, State) ->
    fold_windows(fun(Win,_) -> Fun(Win) end, ok, State),
    ok.


clear_window(Win) ->
    epx:pixmap_fill(Win#widget.image, Win#widget.color).

update_window(Win) ->
    epx:pixmap_copy_to(Win#widget.image, Win#widget.backing),
    epx:pixmap_draw(Win#widget.backing, 
		    Win#widget.win, 0, 0, 0, 0, 
		    Win#widget.width, 
		    Win#widget.height).

unmap_window(Win) ->
    epx:window_detach(Win#widget.win),
    epx:pixmap_detach(Win#widget.backing).


%%
%% TODO: menu border/offset/scale/background
%%
draw_widget(W, Win) ->
    case W#widget.type of
	window ->
	    %% do not draw (yet), we may use this
	    %% to draw embedded windows in the future
	    ok;

	panel ->
	    %% draw tabs according halign, valign
	    epx_gc:draw(
	      fun() ->
		      epx_gc:set_fill_style(W#widget.fill),
		      set_color(W, W#widget.color),
		      epx:draw_rectangle(Win#widget.image,
					 W#widget.x, W#widget.y,
					 W#widget.width, W#widget.height)
	      end);
	button ->
	    epx_gc:draw(
	      fun() ->
		      draw_text_box(Win, W, W#widget.text)
	      end);
	switch ->
	    epx_gc:draw(
	      fun() ->
		      draw_text_box(Win, W, W#widget.text)
	      end);
	slider ->
	    %% Fixme: draw & handle horizontal / vertical 
	    epx_gc:draw(
	      fun() ->
		      draw_background(Win, W),
		      draw_border(Win, W, W#widget.border),
		      draw_value_bar(Win, W, W#widget.topimage)
	      end);
	value ->
	    epx_gc:draw(
	      fun() ->
		      Value = W#widget.value,
		      Format = W#widget.format,
		      Text = 
			  if Value =:= undefined ->
				  "-";
			     Format =:= undefined ->
				  if is_integer(Value) ->
					  integer_to_list(Value);
				     is_float(Value) ->
					  io_lib_format:fwrite_g(Value);
				     true ->
					  "?"
				  end;
			     true ->
				  lists:flatten(io_lib:format(Format,[Value]))
			  end,
		      draw_text_box(Win, W, Text)
	      end);
	rectangle ->
	    epx_gc:draw(
	      fun() ->
		      draw_background(Win, W)
	      end);
	ellipse ->
	    epx_gc:draw(
	      fun() ->
		      epx_gc:set_fill_style(W#widget.fill),
		      set_color(W, W#widget.color),
		      epx:draw_ellipse(Win#widget.image, 
				       W#widget.x, W#widget.y,
				       W#widget.width, W#widget.height)
	      end);
	line ->
	    epx_gc:draw(
	      fun() ->	
		      set_color(W, W#widget.color),
		      epx:draw_line(Win#widget.image, 
				    W#widget.x, W#widget.y,
				    W#widget.x+W#widget.width-1,
				    W#widget.y+W#widget.height-1)
	      end);
	image ->
	    epx_gc:draw(
	      fun() ->
		      draw_background(Win, W)
	      end);
	text ->
	    epx_gc:draw(
	      fun() ->
		      draw_text_box(Win, W, W#widget.text)
	      end);
	Type ->
	    lager:debug("bad widget type ~p", [Type])
    end.

%% draw widget button/value with centered text
draw_text_box(Win, W, Text) ->
    draw_background(Win, W),
    if is_list(Text), Text =/= "" ->
	    Font = W#widget.font,
	    epx_gc:set_font(W#widget.font),
	    epx_gc:set_foreground_color(W#widget.font_color band 16#ffffff),
	    {TxW,TxH} =epx_font:dimension(epx_gc:current(), Text),
	    Xd = case W#widget.halign of
		     left  -> 0;
		     right -> W#widget.width - TxW;
		     center -> (W#widget.width-TxW) div 2
		 end,
	    Yd = case W#widget.valign of
		     top -> 0;
		     bottom -> W#widget.height - TxH;
		     center -> (W#widget.height-TxH) div 2
		 end,
	    %% draw centered text
	    X = W#widget.x + Xd,
	    Y = W#widget.y + Yd + epx:font_info(Font, ascent),
	    epx:draw_string(Win#widget.image, X, Y, Text);
       true ->
	    ok
    end.


draw_background(Win, W) ->
    #widget {min=Min, max=Max, width=Width, height=Height, x=X, y=Y} = W,
   
    if W#widget.color2 =/= undefined,
       Min =/= undefined, Max =/= undefined ->
	    draw_split_background(Win, W);
       W#widget.image2 =/= undefined,
       Min =/= undefined, Max =/= undefined ->
	    draw_split_background(Win, W);
       W#widget.animation2 =/= undefined,
       Min =/= undefined, Max =/= undefined ->
	    draw_split_background(Win, W);
       true ->
	    #widget {color = Color, image = Image, animation = Anim} = W,
	    draw_one_background(Win, W, X, Y, Width, Height, 
				Color, Image, Anim)
    end.

draw_split_background(Win, W=#widget {orientation = horizontal}) ->
    #widget {value = Value, width=Width, height=Height, x=X, y=Y} = W,
    #widget {color = Color, image = Image, animation = Anim} = W,
    #widget {color2 = Color2, image2 = Image2, animation2 = Anim2} = W,
    R = value_proportion(W),
    draw_one_background(Win, W, X, Y, 
			trunc(R*Width), Height, 
			Color, Image, Anim),
    draw_one_background(Win, W, X + trunc(R*Width), Y,
			Width - trunc(R*Width), Height, 
			Color2, Image2, Anim2);
draw_split_background(Win, W=#widget {orientation = vertical}) ->
    #widget {value=Value, width=Width, height=Height, x=X, y=Y} = W,
    lager:debug("drawing background, value ~p", [Value]),
    #widget {color = Color, image = Image, animation = Anim} = W,
    #widget {color2 = Color2, image2 = Image2, animation2 = Anim2} = W,
    R = value_proportion(W),
    Y0 = Y + Height - 1,
    %% Bottom part
    draw_one_background(Win, W, X, trunc(Y0*(1-R) + Y*R), 
			Width, trunc(R*(Y0-Y)) + 1, 
			Color, Image, Anim),
    %% Top part
    draw_one_background(Win, W, X, Y, 
			Width, trunc((1-R)*(Y0-Y)), 
			Color2, Image2, Anim2).

   
draw_one_background(Win, W, X, Y, Width, Height, Color, Image, Anim) ->
    epx_gc:set_fill_style(W#widget.fill),
    set_color(W, Color),
    epx:draw_rectangle(Win#widget.image, X, Y, Width, Height),
    
    if is_record(Image, epx_pixmap) ->
	    lager:debug("drawing image ~p", [Image]),
	    Width = epx:pixmap_info(Image,width),
	    Height = epx:pixmap_info(Image,height),
	    epx:pixmap_copy_area(Image,
				 Win#widget.image,
				 0, 0, X, Y, Width, Height,
				 [blend]);
       true ->
	    ok
    end,
    if is_record(Anim, epx_animation) ->
	    lager:debug("drawing animation ~p", [Anim]),
	    Count = epx:animation_info(Anim, count),
	    Frame = clamp(W#widget.frame, 0, Count-1),
	    %% fixme: handle count=0, frame=undefined,
	    %% fixe scaling
	    epx:animation_draw(Anim, round(Frame),
			       Win#widget.image, epx_gc:current(),
			       X,Y);
       true ->
	    ok
    end.
    
draw_border(_Win, _W, undefined) ->
    ok;
draw_border(_Win, _W, 0) ->
    ok;
draw_border(Win, W, Border) ->
    %% fixme: calculate size from border thickness
    epx_gc:set_foreground_color(16#00000000),
    epx_gc:set_fill_style(none),
    epx:draw_rectangle(Win#widget.image,
		       W#widget.x, W#widget.y,
		       W#widget.width, W#widget.height).

draw_value_bar(Win, W, TopImage) ->
    #widget { min=Min, max=Max, value=Value} = W,
    if is_number(Min),is_number(Max),is_number(Value) ->
	    R = value_proportion(W),
	    if is_record(TopImage, epx_pixmap) ->
		    draw_topimage(Win, W, TopImage, R);
	       true ->
		    draw_value_marker(Win, W, R)
	    end;
		    
       true ->
	    ok
		
    end.

value_proportion(W) ->
    #widget { min=Min, max=Max, value=Value} = W,
    Delta = abs(Max - Min),
    if Min < Max ->
		V = if Value < Min -> Min;
		       Value > Max -> Max;
		       true -> Value
		    end,
		(V - Min)/Delta;
	   Min > Max -> %% reversed axis
		V = if Value > Min -> Min;
		       Value < Max -> Max;
		       true -> Value
		    end,
		(V - Max)/Delta;
	   true ->
		0.5
	end.

draw_topimage(Win, W, TopImage, R) ->
    lager:debug("drawing topimage ~p, orientation ~p, r ~p", 
		[W#widget.topimage, W#widget.orientation, R]),
    Width = epx:pixmap_info(TopImage,width),
    Height = epx:pixmap_info(TopImage,height),
    {X, Y} = case W#widget.orientation of
		 horizontal ->
		     {trunc(W#widget.x + R*((W#widget.width-Width)-1)),
		      W#widget.y + W#widget.height div 2 - Height div 2};
		 vertical ->
		     Y0 = W#widget.y + W#widget.height - 1,
		     Y1 = W#widget.y,
		     Yv = trunc(Y0*(1-R) + Y1*R),
		     {W#widget.x + W#widget.width div 2 - Width div 2,
		      (Yv - Width div 2)}
	     end,
    epx:pixmap_copy_area(TopImage,
			 Win#widget.image,
			 0, 0, X, Y, Width, Height,
			 [blend]).

draw_value_marker(Win, W, R) ->
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(16#00000000),
    M = 3,    %% marker width/height
    case W#widget.orientation of
	horizontal ->
	    X = trunc(W#widget.x + R*((W#widget.width-M)-1)),
	    Y =  W#widget.y + 2,
	    epx:draw_rectangle(Win#widget.image,
			       X, Y, M, W#widget.height-4);
	vertical ->
	    X = W#widget.x + 2,
	    Y0 = W#widget.y + W#widget.height - 1,
	    Y1 = W#widget.y,
	    Y = trunc(Y0*(1-R) + Y1*R),
	    epx:draw_rectangle(Win#widget.image,
			       X, Y - (M div 2), W#widget.width-4, M)
	    
    end.

%% set foreground / fillcolor also using animatation state
set_color(W, Color0) ->
    Color = case W#widget.animate of
		{color,{add,AColor}} ->
		    color_add(Color0, AColor);
		{color,{sub,AColor}} ->
		    color_sub(Color0, AColor);
		{color,{interpolate,V,AColor}} ->
		    %% color from AColor -> W#widget.color
		    color_interpolate(V, AColor, Color0);
		_ -> Color0
	    end,
    epx_gc:set_foreground_color(Color),
    epx_gc:set_fill_color(Color).

color_add(C1, C2) ->
    <<C3:32>> = color_add_argb(<<C1:32>>, <<C2:32>>),
    C3.

color_sub(C1, C2) ->
    <<C3:32>> = color_sub_argb(<<C1:32>>, <<C2:32>>),
    C3.

color_interpolate(V, C1, C2) ->
    <<C3:32>> = color_interpolate_argb(V, <<C1:32>>, <<C2:32>>),
    C3.

color_add_argb(<<A1,R1,G1,B1>>,<<A2,R2,G2,B2>>) ->
    A = A1 + A2,
    R = R1 + R2,
    G = G1 + G2,
    B = B1 + B2,
    <<(clamp_byte(A)),(clamp_byte(R)),(clamp_byte(G)),(clamp_byte(B))>>.

color_sub_argb(<<A1,R1,G1,B1>>,<<A2,R2,G2,B2>>) ->
    A = A1 - A2,
    R = R1 - R2,
    G = G1 - G2,
    B = B1 - B2,
    <<(clamp_byte(A)),(clamp_byte(R)),(clamp_byte(G)),(clamp_byte(B))>>.

color_interpolate_argb(V, <<A0,R0,G0,B0>>,<<A1,R1,G1,B1>>) 
  when is_float(V), V >= 0.0, V =< 1.0 ->
    A = trunc(A0 + V*(A1-A0)),
    R = trunc(R0 + V*(R1-R0)),
    G = trunc(R1 + V*(G1-G0)),
    B = trunc(B1 + V*(B1-B0)),
    <<(clamp_byte(A)),(clamp_byte(R)),(clamp_byte(G)),(clamp_byte(B))>>.
    
clamp_byte(A) when A > 255 -> 255;
clamp_byte(A) when A < 0  -> 0;
clamp_byte(A) -> A.

%% clamp numbers
clamp(V,Min,Max) when is_number(V),is_number(Min),is_number(Max) ->
    if Min < Max -> min(max(V,Min), Max);
       Min > Max -> max(min(V,Min), Max);
       Min == Max -> Min
    end;
clamp(undefined,Min,Max) ->
    if is_number(Min) -> Min;
       is_number(Max) -> Max;
       true -> undefined
    end;
clamp(V,Min,undefined) when is_number(Min), V < Min -> Min;
clamp(V,undefined,Max) when is_number(Max), V > Max -> Max;
clamp(V,_,_) -> V.


