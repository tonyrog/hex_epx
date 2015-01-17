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
-export([start_link/0, stop/0]).
-export([add_event/3, mod_event/2, del_event/1]).
-export([init_event/2, output_event/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("epx/include/epx.hrl").
-include_lib("epx/include/epx_image.hrl").

-define(SERVER, ?MODULE).
-define(DICT_T, term()).  %% dict:dict()

-define(DEFAULT_WIDTH, 320).
-define(DEFAULT_HEIGHT, 240).

-record(widget,
	{
	  id,        %% named widgets
	  type,      %% button,rectangle,slider ...
	  window = default,  %% id of window (if type != window)
	  state  = normal,   %% or active,selected ..
	  x = 0   :: integer(),
	  y = 0   :: integer(),
	  width  = 32 :: non_neg_integer(),
	  height = 32 :: non_neg_integer(),
	  text = "",
	  image   :: epx:epx_pixmap(),
	  color = 16#ff000000,
	  fill   = none :: epx:epx_fill_style(),
	  events = []   :: epx:epx_window_event_flags(),
	  halign  = center :: top|bottom|center,
	  valign  = center :: left|right|center,
	  min     :: number(),          %% type=value|slider
	  max     :: number(),          %% type=value|slider
	  format = "~w" :: string(),   %% io:format format
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
	  windows :: ?DICT_T,  %% term => #widget{}
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
start_link() ->
    hex:start_all(lager),
    application:start(epx),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    Default = window_create([{id,default},
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
		 windows = dict:from_list([{default,Default}]),
		 widgets = dict:new()
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
	    {reply, {ok,Ref}, State#state { subs = Subs}}
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
	{_,ID} ->
	    case dict:find(ID,State#state.widgets) of
		error ->
		    W0 = #widget{font=State#state.default_font},
		    try widget_set(Flags,W0) of
			W ->
			    Ws1 = dict:store(ID,W,State#state.widgets),
			    self() ! refresh,
			    {reply, ok, State#state{widgets=Ws1}}
		    catch
			error:Reason ->
			    {reply, {error,Reason}, State}
		    end;
		{ok,_W} -> %% overwrite? merge?
		    {reply, {error, ealready}, State}
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
    case dict:fold(fun(_ID,W,Acc) ->
			   if W#widget.win =:= Win -> [W|Acc];
			      true -> Acc
			   end
		   end, [], State#state.windows) of
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
    each_widget(fun unmap_window/1, State#state.windows),
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
handle_event(close,Window,State) ->
    if Window#widget.id =:= default ->
	    {noreply, State};
       true ->
	    unmap_window(Window),
	    Windows1 = dict:erase(Window#widget.id, State#state.windows),
	    {noreply, State#state { windows = Windows1 }}
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
		 true ->
		      Acc
	      end
      end, [], Ws).

%% generate a callback event and start animate the button
widget_event({button_press,_Button,Where}, W, Window, State) ->
    case W#widget.type of
	button ->
	    callback_all(W#widget.id, State#state.subs, [{value,1}]),
	    widget_animate_begin(W#widget { state=active }, press);
	slider ->
	    {X,_,_} = Where,
	    case widget_slider_value(W, X) of
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
	slider ->
	    epx:window_disable_events(Window#widget.win, [motion]),
	    W#widget{state=normal};
	_ ->
	    W
    end;
widget_event({motion,_Button,Where}, W, _Window, State) ->
    case W#widget.type of
	slider ->
	    {X,_,_} = Where,
	    case widget_slider_value(W, X) of
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
widget_event(_Event, W, _Window, _State) ->
    W.

%% given x coordinate calculate the slider value
widget_slider_value(W=#widget { min=Min, max=Max }, X) ->
    Width = W#widget.width-2,
    if is_number(Min), is_number(Max), Width > 0 ->
	    X0 = W#widget.x+1,
	    X1 = X0 + Width - 1,
	    Xv = clamp(X, X0, X1),
	    R = (Xv - X0) / (X1 - X0),
	    {ok,trunc(Min + R*(Max - Min))};
       true ->
	    false
    end.


widget_animate_begin(W, press) ->
    lager:debug("animate_begin: down"),
    self() ! refresh,
    W#widget { animate = {color,{sub,16#00333333}} };
widget_animate_begin(W, release) ->
    lager:debug("animate_begin: up"),
    self() ! refresh,
    W#widget { animate = undefined };
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

%% note that event signals may loopback be time consuming,
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
	{id,ID} when is_atom(ID) -> 
	    widget_set(Flags, W#widget{id=ID});
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
	{image,File} when is_list(File) ->
	    case epx_image:load(File) of
		{ok,Image} ->
		    case Image#epx_image.pixmaps of
			[Pixmap] ->
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
	{color,Color} when is_integer(Color), Color>=0 ->
	    widget_set(Flags, W#widget{color=Color});
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
    each_widget(fun clear_window/1, State#state.windows),
    each_widget(fun(W) ->
			case dict:find(W#widget.window, State#state.windows) of
			   error ->
			       lager:error("missing window id=~w\n",
					   [W#widget.window]);
			    {ok,Win} ->
				draw_widget(W, Win)
			end
		end, State#state.widgets),
    each_widget(fun update_window/1, State#state.windows),
    State.


each_widget(Fun, Ws) ->
    dict:fold(fun(_K,W,_) -> Fun(W) end, [], Ws),
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
	button ->
	    epx_gc:draw(
	      fun() ->
		      draw_text_box(Win, W, W#widget.text)
	      end);
	slider ->
	    epx_gc:draw(
	      fun() ->
		      epx_gc:set_fill_style(solid),
		      epx_gc:set_fill_color(W#widget.color),
		      epx:draw_rectangle(Win#widget.image,
					 W#widget.x, W#widget.y,
					 W#widget.width, W#widget.height),
		      epx_gc:set_foreground_color(16#00000000),
		      epx_gc:set_fill_style(none),
		      epx:draw_rectangle(Win#widget.image,
					 W#widget.x, W#widget.y,
					 W#widget.width, W#widget.height),
		      %% draw value bar
		      #widget { min=Min, max=Max, value=Value} = W,
		      if is_number(Min),is_number(Max),is_number(Value) ->
			      Delta = abs(Max - Min),
			      R = if Min < Max ->
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
				  end,
			      %% draw value marker
			      Wm = 3,    %% marker width
			      X = trunc(W#widget.x + R*((W#widget.width-Wm)-1)),
			      Y = W#widget.y + 2,
			      epx_gc:set_fill_style(solid),
			      epx_gc:set_fill_color(16#00000000),
			      epx:draw_rectangle(Win#widget.image,
						 X, Y, Wm, W#widget.height-4);
			 true ->
			      ok
			      
		      end
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
		      epx_gc:set_fill_style(W#widget.fill),
		      set_color(W),
		      epx:draw_rectangle(Win#widget.image, 
					 W#widget.x, W#widget.y,
					 W#widget.width, W#widget.height)
	      end);
	ellipse ->
	    epx_gc:draw(
	      fun() ->
		      epx_gc:set_fill_style(W#widget.fill),
		      set_color(W),
		      epx:draw_ellipse(Win#widget.image, 
				       W#widget.x, W#widget.y,
				       W#widget.width, W#widget.height)
	      end);
	line ->
	    epx_gc:draw(
	      fun() ->	
		      set_color(W),
		      epx:draw_line(Win#widget.image, 
				    W#widget.x, W#widget.y,
				    W#widget.x+W#widget.width-1,
				    W#widget.y+W#widget.height-1)
	      end);
	image ->
	    epx_gc:draw(
	      fun() ->
		      if is_record(W#widget.image, epx_pixmap) ->
			      Width = epx:pixmap_info(W#widget.image,width),
			      Height = epx:pixmap_info(W#widget.image,height),
			      epx:pixmap_copy_area(W#widget.image,
						   Win#widget.image,
						   0, 0,
						   W#widget.x, W#widget.y,
						   Width, Height,
						   [solid]);
			 true ->
			      ok
		      end
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
    epx_gc:set_fill_style(W#widget.fill),
    set_color(W),
    epx:draw_rectangle(Win#widget.image, 
		       W#widget.x, W#widget.y,
		       W#widget.width, W#widget.height),
    Font = W#widget.font,
    epx_gc:set_font(W#widget.font),
    %% black text color (fixme)
    epx_gc:set_foreground_color(16#00000000),
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
    epx:draw_string(Win#widget.image, X, Y, Text).


%% set foreground / fillcolor also using animatation state
set_color(W) ->
    Color0 = W#widget.color,
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


