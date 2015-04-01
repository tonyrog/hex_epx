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

-export([color_add/2, color_interpolate/3, color_add_argb/2, 
	 color_interpolate_argb/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("epx/include/epx.hrl").
-include_lib("epx/include/epx_image.hrl").

-define(SERVER, ?MODULE).
-define(DICT_T, term()).  %% dict:dict()
-define(SETS_T, term()).  %% sets:set()
-define(ETS_T,  term()).  %% table()?

-define(DEFAULT_WIDTH,  320).
-define(DEFAULT_HEIGHT, 240).
-define(DEFAULT_WINDOW_ID, "screen").

-define(MAX_TICKS, 16#ffffffff).  %% about 49.7 days

-define(is_string(Cs), is_list((Cs))).

-record(widget,
	{
	  id :: string(),     %% (structured) name of widget
	  type,               %% button,rectangle,slider ...
	  window :: string(), %% id of base window (if type != window)
	  state  = normal,    %% or active,selected,closed ..
	  static = false,     %% object may not be deleted
	  x = 0   :: integer(),
	  y = 0   :: integer(),
	  z = 0   :: integer(),   %% define the order for overlap
	  width  = 32 :: non_neg_integer(),
	  height = 32 :: non_neg_integer(),
	  text = "",
	  tabs = [],
	  border  :: number(),
	  orientation = horizontal :: horizontal|vertical,
	  image   :: epx:epx_pixmap(),
	  image2   :: epx:epx_pixmap(),
	  topimage :: epx:epx_pixmap(),
	  animation :: epx:epx_animation(),
	  animation2 :: epx:epx_animation(),
	  frame :: number(),
	  frame2 :: number(),
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
	  animate = undefined,          %% animation state.
	  animate2 = undefined,         %% animation state of second animation.
	  font    :: epx:epx_font(),    %% type=text|button|value
	  win     :: epx:epx_window(),  %% type = window
	  backing :: epx:epx_pixmap()
	}).

-record(sub,
	{
	  ref :: reference(),
	  mon :: reference(),
	  id  :: string(),
	  callback :: atom() | function(),
	  signal :: term()
	}).
	  
-record(state, {
	  joined :: boolean(),       %% joined hex server
	  redraw_timer = undefined,
	  active = [] :: [term()],   %% active widgets pressed
	  subs = [] :: [#sub{}],
	  fps = 30.0 :: number(),       %% animation frames per second
	  mpf = 1000/30.0 :: number(),  %% millis per frame
	  clock :: reference(),         %% clock reference
	  redraw_tick :: number(),      %% aprox redraw clock
	  default_font :: epx:epx_font(),
	  wset    :: ?SETS_T,           %% set of window id
	  wtree   :: ?ETS_T             %% ets_tree of all widgets
	  %% widgets are now stored in process dictionary
	  %% widgets :: ?DICT_T   %% term => #widget{}
	 }).


-define(TABS_X_OFFSET, 10).  %% should scale with size!
-define(TABS_Y_OFFSET, 10).  %% should scale with size!
-define(TABS_X_PAD, 16).      %% should scale with font size!
-define(TABS_Y_PAD, 8).      %% should scale with font size!
-define(TABS_COLOR, 16#ffcccccc).  %% configure this

-define(EOT, '$end_of_table').

add_event(Flags, Signal, Cb) ->
    gen_server:call(?MODULE, {add_event, self(), Flags, Signal, Cb}).

del_event(Ref) ->
    gen_server:call(?MODULE, {del_event, Ref}).

output_event(Flags, Env) ->
    %% Init event is supposed to have sent all flags, so only
    %% pickout the id field and send environment
    Flags1 = [{id,proplists:get_value(id,Flags,undefined)}],
    gen_server:call(?MODULE, {output_event, Flags1, Env}).

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
    Fps = proplists:get_value(fps, Args, 30.0),
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
    Default = window_create([{id,?DEFAULT_WINDOW_ID},
			     {static,true},
			     {x,50},{y,50},
			     {width,Width},{height,Height},
			     {events, [key_press,key_release,
				       button_press, button_release,
				       %% configure,resize,focus,
				       %% crossing, motion
				       button,wheel]},
			     {color, 16#ffffffff}]),
    Tree = ets_tree:new(wtree, []),
    ets_tree:insert(Tree, {?DEFAULT_WINDOW_ID,?DEFAULT_WINDOW_ID}),
    self() ! refresh,
    %% This clock will run for 49,71 days before timeout, but we
    %% use it as a cheap? clock source.
    Clock = clock_create(),
    widget_store(Default),
    {ok, #state{ joined = Joined,
		 default_font = Font,
		 fps = Fps,
		 clock = Clock,
		 wset = sets:from_list([Default#widget.id]),
		 wtree = Tree
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
    case find_id(Flags) of
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
    case find_id(Flags) of
	false ->
	    {reply,{error,missing_id},State};
	{id,ID} ->
	    case widget_find(ID) of
		error ->
		    {reply,{error,enoent},State};
		{ok,W} ->
		    try widget_set(Flags++Env, W) of
			W1 ->
			    widget_store(W1),
			    self() ! refresh,
			    {reply, ok, State}
		    catch
			error:Reason ->
			    {reply, {error,Reason}, State}
		    end
	    end
    end;
handle_call({init_event,_Dir,Flags}, _From, State) ->
    case find_id(Flags) of
	false ->
	    {reply,{error,missing_id},State};
	{id,ID} ->
	    case widget_find(ID) of
		error ->
		    W0 = #widget{ font=State#state.default_font },
		    try widget_set(Flags,W0) of
			W1 ->
			    W2 =
				if W1#widget.type =/= window,
				   W1#widget.window =:= undefined ->
					W1#widget { window=?DEFAULT_WINDOW_ID};
				   true ->
					W1
				end,
			    Y = if W2#widget.type =:= window ->
					W2#widget.id;
				   true ->
					W2#widget.window++"."++W2#widget.id
				end,
			    ets_tree:insert(State#state.wtree,{Y,W2#widget.id}),
			    widget_store(W2),
			    self() ! refresh,
			    {reply, ok, State}
		    catch
			error:Reason ->
			    io:format("widget ~p not created ~p\n",
				      [ID, Reason]),
			    {reply, {error,Reason}, State}
		    end;
		{ok,W} ->
		    try widget_set(Flags,W) of
			W1 ->
			    widget_store(W1),
			    self() ! refresh,
			    {reply, ok, State}
		    catch
			error:Reason ->
			    io:format("widget ~p not updated ~p\n",
				      [W#widget.id, Reason]),
			    {reply, {error,Reason}, State}
		    end
	    end
    end;
handle_call({mod_event,_Dir,Flags}, _From, State) ->
    case widget_lookup(Flags) of
	E={error,_} ->
	    {reply,E, State};
	{ok,W} ->
	    try widget_set(Flags, W) of
		W1 ->
		    widget_store(W1),
		    self() ! refresh,
		    {reply, ok, State}
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
    %% lager:debug("event: ~p", [Event]),
    %% find window widget (fixme: add reverse map at some point) ?
    case fold_windows(
	   fun(W,Acc) ->
		   if W#widget.win =:= Win -> [W|Acc];
		      true -> Acc
		   end
	   end, [], State) of
	[] ->
	    lager:error("window ~p not found", [Win]),
	    {noreply, State};
	[W] ->  %% should only be one!
	    handle_event(Event, W, State)
    end;
handle_info(refresh, State) ->
    {noreply, redraw_schedule(State)};


handle_info({timeout,TRef,redraw}, State) 
  when TRef =:= State#state.redraw_timer ->
    %% lager:debug("redraw"),
    put(animations, false),
    State1 = redraw_state(State#state { redraw_timer=undefined}),
    case get(animations) of
	false ->
	    {noreply, State1};
	true ->
	    {noreply, redraw_schedule(State1)}
    end;
handle_info(clock_restart, State) ->
    %% fixme how to handle handle active timers ?
    {noreply, State#state { clock = clock_create() }};

handle_info({'DOWN',Ref,process,_Pid,_Reason}, State) ->
    case lists:keytake(Ref, #sub.ref, State#state.subs) of
	false ->
	    {noreply, State};
	{value, _Sub, Subs} ->
	    {noreply, State#state { subs=Subs} }
    end;
handle_info(_Info, State) ->
    lager:debug("info = ~p", [_Info]),
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
    fold_windows(fun unmap_window/2, State, State),
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

widget_store(W) ->
    put(W#widget.id, W),
    W.

widget_erase(ID) ->
    erase(ID).

widget_fetch(ID) ->
    case get(ID) of
	W when is_record(W, widget) -> W
    end.

widget_find(ID) ->
    case get(ID) of
	undefined ->
	    error;
	W when is_record(W, widget) -> 
	    {ok,W}
    end.

widget_lookup(Flags) ->
    case find_id(Flags) of
	false ->
	    {error,missing_id};
	{id,ID} ->
	    case widget_find(ID) of
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
	    lager:debug("Window = ~p\n", [WinID]),
	    case widgets_at_location(X,Y,WinID,State) of
		[] ->
		    {noreply, State};
		_Ws1=[W|_] ->
		    lager:debug("selected ws=~p", [[Wi#widget.id||Wi<-_Ws1]]),
		    case widget_event(Event, W, Window, State) of
			W ->
			    {noreply, State};
			W1 ->
			    ID = W1#widget.id,
			    Active = [ID | State#state.active],
			    widget_store(W1),
			    self() ! refresh,
			    {noreply, State#state { active = Active }}
		    end
	    end;
	false ->
	    {noreply, State}
    end;
handle_event(Event={button_release,Button,{_X,_Y,_}},Window,State) ->
    case lists:member(left,Button) of
	true ->
	    State1 = 
		lists:foldl(
		  fun(ID, Si) ->
			  case widget_find(ID) of
			      error -> Si;
			      {ok,W} ->
				  case widget_event(Event, W, Window, Si) of
				      W ->
					  Si; %% no changed
				      W1 ->
					  widget_store(W1),
					  self() ! refresh,
					  Si
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
	    case widgets_at_location(X,Y,WinID,State) of
		[] ->
		    {noreply, State};
		[W|_] ->
		    case widget_event(Event, W, Window, State) of
			W -> {noreply, State}; %% no changed
			W1 ->
			    widget_store(W1),
			    self() ! refresh,
			    {noreply, State}
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
	    unmap_window(Window,State),
	    Window1 = widget_event(Event, Window, Window, State),
	    State1 = widget_delete(Window1, State),
	    self() ! refresh,
	    {noreply, State1}
    end;
handle_event(_Event,_W,State) ->
    lager:error("unknown event: ~p", [_Event]),
    {noreply, State}.

%%
%% Find all widgets in window WinID that is hit by the
%% point (X,Y). Return a Z sorted list
%%
widgets_at_location(X,Y,WinID,State) ->
    Ws = select_tree(WinID,X,Y,[],State),
    %% sort according to Z order
    %% lists:sort(fun(A,B) -> A#widget.z > B#widget.z end, Ws).
    lists:reverse(Ws).

select_tree(?EOT,_X,_Y,Acc,_State) ->
    Acc;
select_tree(ID,X,Y,Acc,State) ->
    ChildID = ets_tree:first_child(State#state.wtree, ID),
    select_siblings(ChildID,X,Y,Acc,State).

select_siblings(?EOT,_X,_Y,Acc,_State) ->
    Acc;
select_siblings(ID,X,Y,Acc,State) ->
    Acc1 = select_one(ID,X,Y,Acc,true,State),
    NextSibling = ets_tree:next_sibling(State#state.wtree, ID),
    select_siblings(NextSibling,X,Y,Acc1,State).

select_one(ID,X,Y,Acc,ChildrenFirst,State) ->
    [{_,Wid}] = ets_tree:lookup(State#state.wtree,ID),
    W = widget_fetch(Wid),
    if ChildrenFirst ->
	    Acc1 = select_children(W,ID,X,Y,Acc,State),
	    select_widget(W,X,Y,Acc1,State);
       true ->
	    Acc1 = select_widget(W,X,Y,Acc,State),
	    select_children(W,ID,X,Y,Acc1,State)
    end.

select_children(W,ID,X,Y,Acc,State) when W#widget.type =:= panel ->
    case tab_at_location(W,X,Y) of
	0 -> %% check in current tab
	    V = W#widget.value,
	    N = length(W#widget.tabs),
	    if V =:= 0 ->
		    %% no child selected
		    Acc;
	       V >= 1, V =< N ->
		    Tab = lists:nth(V, W#widget.tabs),
		    TabID = ID++[list_to_binary(Tab)],
		    select_one(TabID,X,Y,Acc,false,State);
	       true ->
		    lager:error("panel tab ~w not defined in ~s\n",
				[V,W#widget.id]),
		    Acc
	    end;
	Tab ->
	    io:format("tab at (~w,~w) = ~w\n", [X,Y,Tab]),
	    [W|Acc]
    end;
select_children(_W,ID,X,Y,Acc,State) ->
    select_tree(ID,X,Y,Acc,State).

select_widget(W,X,Y,Acc,_State) ->
    case in_bounding_box(W, X, Y) of
	true ->
	    [W|Acc];
	false ->
	    case topimage_at_location(W,X,Y,
				      W#widget.topimage) of
		true ->
		    [W|Acc];
		false ->
		    Acc
	    end
    end.


%% Check if (X,Y) is within any of the panel tabs
tab_at_location(W,X,Y) ->
    {_Ascent,TextDims,MaxW,MaxH} = tabs_item_box(W),
    N = length(TextDims),
    Width =  (MaxW+?TABS_X_PAD),
    Height = (MaxH+?TABS_Y_PAD),
    case W#widget.orientation of
	horizontal ->
	    Xoffs = (W#widget.width - (Width*N)) div 2,
	    X0 = W#widget.x + Xoffs,
	    Y0 = W#widget.y + ?TABS_Y_OFFSET,
	    case in_rect(X,Y,X0,Y0,Width*N,Height) of
		false -> 0;
		true  -> ((X-Xoffs) div Width)+1
	    end;
	vertical ->
	    Yoffs = (W#widget.height - (Height*N)) div 2,
	    Y0 = W#widget.y + Yoffs,
	    X0 = W#widget.x + ?TABS_X_OFFSET,
	    case in_rect(X,Y,X0,Y0,Width,Height*N) of
		false -> 0;
		true -> ((Y-Yoffs) div Height)+1
	    end
    end.

in_bounding_box(W, X, Y) ->
    in_rect(X,Y,W#widget.x,W#widget.y, W#widget.width, W#widget.height).

in_rect(X,Y,Xr,Yr,Wr,Hr) ->
    if X >= Xr, X < Xr + Wr, Y >= Yr, Y < Yr + Hr -> true;
       true -> false
    end.
    
%%
%% Check if (X,Y) hit inside a topimage (used in slider)
%%
topimage_at_location(_W,_X,_Y,undefined) ->
    false;
topimage_at_location(W=#widget {orientation = horizontal},X,Y,Image) ->
    Height = epx:pixmap_info(Image,height),
    Y1 = W#widget.y + (W#widget.height - Height) div 2,
    Y2 = W#widget.y + (W#widget.height + Height) div 2,
    (X >= W#widget.x) andalso (X < W#widget.x + W#widget.width) andalso
	(Y >= Y1) andalso (Y =< Y2);
topimage_at_location(W=#widget {orientation = vertical},X,Y,Image) ->
    Width = epx:pixmap_info(Image,width),
    X1 = W#widget.x + (W#widget.width - Width) div 2,
    X2 = W#widget.x + (W#widget.width + Width) div 2,
    (Y >= W#widget.y) andalso (Y < W#widget.y + W#widget.height) andalso
	(X >= X1) andalso (X =< X2).

-ifdef(not_used).
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
-endif.

-ifdef(not_used).
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
-endif.
      

%% generate a callback event and start animate the button
widget_event({button_press,_Button,Where}, W, Window, State) ->
    case W#widget.type of
	button ->
	    callback_all(W#widget.id, State#state.subs, [{value,1}]),
	    W#widget { state=active, value=1 };

	switch ->
	    {WState,Value} =
		case W#widget.state of
		    active -> {normal,0};
		    _ -> {active,1}
		end,
	    callback_all(W#widget.id, State#state.subs, [{value,Value}]),
	    W#widget { frame=Value, state=WState, value=Value };

	slider ->
	    {X,Y,_} = Where,
	    case widget_slider_value(W, X, Y) of
		false ->
		    lager:debug("slider min/max/width error"),
		    W;
		Value ->
		    epx:window_enable_events(Window#widget.win, [motion]),
		    callback_all(W#widget.id,State#state.subs,[{value,Value}]),
		    W#widget { state=active, value = Value }
	    end;
	panel ->
	    {X,Y,_} = Where,
	    case tab_at_location(W,X,Y) of
		0 ->
		    lager:debug("panel box select error"),
		    W;
		Value ->
		    io:format("tab at (~w,~w) = ~w\n", [X,Y,Value]),
		    callback_all(W#widget.id,State#state.subs,[{value,Value}]),
		    W#widget { value = Value }
	    end;
	_ ->
	    W
    end;
widget_event({button_release,_Button,_Where}, W, Window, State) ->
    case W#widget.type of
	button ->
	    callback_all(W#widget.id, State#state.subs, [{value,0}]),
	    W#widget{state=normal, value=0};
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
		false -> W;
		Value ->
		    callback_all(W#widget.id,State#state.subs,[{value,Value}]),
		    W#widget { value = Value }
	    end;
	_ ->
	    W
    end;
widget_event(close, W, _Window, State) ->
    callback_all(W#widget.id,State#state.subs,[{closed,true}]),
    W#widget { state=closed };
widget_event(_Event, W, _Window, _State) ->
    W.

%% Calcuate the slider value given coordinate X,Y either horizontal or
%% vertical. return a floating point value between 0 and 1
widget_slider_value(W=#widget {min=Min,max=Max,orientation=horizontal},X,_Y) ->
    Width = W#widget.width-2,
    if is_number(Min), is_number(Max), Width > 0 ->
	    X0 = W#widget.x+1,
	    X1 = X0 + Width - 1,
	    Xv = clamp(X, X0, X1),
	    R = (Xv - X0) / (X1 - X0),
	    trunc(Min + R*(Max - Min));
       true ->
	    false
    end;
widget_slider_value(W=#widget {min=Min, max=Max, orientation=vertical},_X,Y) ->
    Height = W#widget.height-2,
    if is_number(Min), is_number(Max), Height > 0 ->
	    Y1 = W#widget.y+1,
	    Y0 = Y1 + Height - 1,
	    Yv = clamp(Y, Y1, Y0),
	    R = (Y0 - Yv) / (Y0 - Y1),
	    trunc(Min + R*(Max - Min));
       true ->
	    false
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
	{id,ID} when is_atom(ID); is_list(ID) ->
	    widget_set(Flags, W#widget{id=id_string(ID)});
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
	{tabs,Tabs} when is_list(Tabs) ->
	    widget_set(Flags, W#widget{tabs=Tabs});
	{border, Border} when is_integer(Border) ->
	    widget_set(Flags, W#widget{border=Border});
	{orientation, Orientation} when 
	      Orientation =:= horizontal; Orientation =:= vertical ->
	    widget_set(Flags, W#widget{orientation = Orientation });
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
	{frame2, Frame} when is_integer(Frame) ->
	    widget_set(Flags, W#widget{frame2=Frame});
	{animate, Style} when Style =:= continuous; Style =:= sequence ->
	    widget_set(Flags, W#widget{animate=Style});
	{animate2, Style} when Style =:= continuous; Style =:= sequence ->
	    widget_set(Flags, W#widget{animate2=Style});
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

%% find id among flags and convert tos string
find_id(Flags) ->
    case lists:keyfind(id, 1, Flags) of
	false ->
	    false;
	{id,ID} -> {id,id_string(ID)}
    end.

id_string(X) when is_atom(X) ->
    atom_to_list(X);
id_string(X) when is_list(X) ->
    X.

redraw_schedule(State) ->
    if is_reference(State#state.redraw_timer) ->
	    State;
       State#state.redraw_timer =:= undefined ->
	    RedrawTick = clock_timeout(State, State#state.mpf),
	    Timer = erlang:start_timer(trunc(State#state.mpf), self(), redraw),
	    State#state { redraw_timer = Timer,
			  redraw_tick = RedrawTick }
    end.


clock_create() ->
    erlang:start_timer(?MAX_TICKS, self(), clock_restart).

%% clock ticks since (millis) since last clock restart
clock_read(State) ->
    ?MAX_TICKS - erlang:read_timer(State#state.clock).

%% calculate an absolute timeout value (relative clock source)
clock_timeout(State, Time) when is_number(Time), Time >= 0 ->
    Tick = clock_read(State),
    trunc(Tick + Time) band ?MAX_TICKS.
    
redraw_state(State) ->
    fold_windows(fun clear_window/2, State, State),
    fold_windows(fun draw_window/2, State, State),
    fold_windows(fun update_window/2, State, State),
    State.

widget_delete(#widget{id=Wid,type=Type}, State) ->
    widget_erase(Wid),
    Wset1 = if Type =:= window ->
		    sets:del_delement(Wid,State#state.wset);
	       true ->
		    State#state.wset
	    end,
    State#state { wset=Wset1 }.


%% fold over windows
fold_windows(Fun, Acc, State) ->
    sets:fold(fun(Wid,Acc1) ->
		      Fun(widget_fetch(Wid), Acc1)
	      end, Acc, State#state.wset).

clear_window(Win,_State) ->
    epx:pixmap_fill(Win#widget.image, Win#widget.color).

update_window(Win,_State) ->
    epx:pixmap_copy_to(Win#widget.image, Win#widget.backing),
    epx:pixmap_draw(Win#widget.backing, 
		    Win#widget.win, 0, 0, 0, 0, 
		    Win#widget.width, 
		    Win#widget.height).

unmap_window(Win,_State) ->
    epx:window_detach(Win#widget.win),
    epx:pixmap_detach(Win#widget.backing).


draw_window(Win, State) ->
    draw_tree(Win#widget.id, Win, State).

draw_tree(?EOT, _Win, State) ->
    State;
draw_tree(ID, Win, State) ->
    draw_siblings(ets_tree:first_child(State#state.wtree, ID), Win, State).

draw_siblings(?EOT, _Win, State) ->
    State;
draw_siblings(ID, Win, State) ->
    State1 = draw_one(ID, Win, true, State),
    draw_siblings(ets_tree:next_sibling(State#state.wtree, ID), Win, State1).

draw_one(ID, Win, ChildrenFirst, State) ->
    [{_,Wid}] = ets_tree:lookup(State#state.wtree,ID),
    W = widget_fetch(Wid),
    if ChildrenFirst ->
	    State1 = draw_children(ID, W, Win, State),
	    lager:debug("draw ID = ~p\n", [ID]),
	    draw_widget(W, Win, State1),
	    State1;
       true ->
	    lager:debug("draw ID = ~p\n", [ID]),
	    draw_widget(W, Win, State),
	    draw_children(ID, W, Win, State)
    end.

draw_children(ID, W, Win, State) when W#widget.type =:= panel ->
    V = W#widget.value,
    N = length(W#widget.tabs),
    if V =:= 0 ->
	    %% no child selected
	    State;
       V >= 1, V =< N ->
	    Tab = lists:nth(V, W#widget.tabs),
	    %% tree children first
	    TabID = ID++[list_to_binary(Tab)],
	    draw_one(TabID, Win, false, State);
       true ->
	    lager:error("panel tab ~w not defined in ~s\n",
			[V,W#widget.id]),
	    State
    end;
draw_children(ID, _W, Win, State) ->
    draw_tree(ID, Win, State).


draw_widget(W, Win, _State) ->
    case W#widget.type of
	window ->
	    %% do not draw (yet), we may use this
	    %% to draw multiple/embedded windows in the future
	    ok;

	panel ->
	    epx_gc:draw(
	      fun() ->
		      %% draw_background(Win, W),
		      draw_tabs(Win, W)
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
	    epx_gc:set_font(Font),
	    Ascent = epx:font_info(Font, ascent),
	    epx_gc:set_foreground_color(W#widget.font_color band 16#ffffff),
	    {TxW,TxH} = epx_font:dimension(epx_gc:current(), Text),
	    draw_text(Win, Ascent, Text, TxW, TxH, 
		      W#widget.x, W#widget.y, 
		      W#widget.width, W#widget.height,
		      W#widget.halign, W#widget.valign);
       true ->
	    ok
    end.

%% 
%%  Put tabs as a row (horizontal) 
%%  or column (vertical)
%%
draw_tabs(Win, W) ->
    {Ascent,TextDims,MaxW,MaxH} = tabs_item_box(W),
    N = length(TextDims),
    Width =  (MaxW+?TABS_X_PAD),
    Height = (MaxH+?TABS_Y_PAD),
    case W#widget.orientation of
	horizontal ->
	    X0 = W#widget.x + (W#widget.width - (Width*N)) div 2,
	    Y0 = (W#widget.y + ?TABS_Y_OFFSET),
	    set_color(W, ?TABS_COLOR),
	    epx_gc:set_fill_style(solid),
	    epx:draw_rectangle(Win#widget.image, X0, Y0, Width*N, Height),
	    draw_h_tabs(Win, W, 1, Ascent, TextDims, X0, Y0, Width, Height,
			W#widget.halign, W#widget.valign);
	vertical ->
	    Y0 =  W#widget.y + (W#widget.height - (Height*N)) div 2,
	    X0 = (W#widget.x + ?TABS_X_OFFSET),
	    set_color(W, ?TABS_COLOR),
	    epx_gc:set_fill_style(solid),
	    epx:draw_rectangle(Win#widget.image, X0, Y0, Width, Height*N),
	    draw_v_tabs(Win, W, 1, Ascent, TextDims, X0, Y0, Width, Height,
			W#widget.halign, W#widget.valign)
    end.

tabs_item_box(W) ->
    Font = W#widget.font,
    epx_gc:set_font(Font),
    Ascent = epx:font_info(Font, ascent),
    Tabs = W#widget.tabs,
    TextDims = [ {epx_font:dimension(epx_gc:current(), Text),Text} || 
		   Text <- Tabs ],
    MaxW = lists:max([Wi || {{Wi,_},_} <- TextDims]),
    MaxH = lists:max([Hi || {{_,Hi},_} <- TextDims]),
    {Ascent,TextDims,MaxW,MaxH}.


draw_h_tabs(Win, W, I, Ascent, [{{TxW,TxH},Text}|TextDims],
	    Xi, Yi, Width, Height, Halign, Valign) ->
    %% darken selected field
    if I =:= W#widget.value ->
	    Color = color_sub(?TABS_COLOR, 16#00333333),
	    epx_gc:set_fill_color(Color),
	    epx_gc:set_fill_style(solid),
	    epx:draw_rectangle(Win#widget.image, Xi, Yi, Width, Height);
       true ->
	    ok
    end,
    epx_gc:set_foreground_color(16#00000000),
    epx_gc:set_fill_style(none),
    epx:draw_rectangle(Win#widget.image, Xi, Yi, Width, Height),
    epx_gc:set_foreground_color(W#widget.font_color band 16#ffffff),
    draw_text(Win, Ascent, Text, TxW, TxH, Xi, Yi,
	      Width, Height, Halign, Valign),
    draw_h_tabs(Win, W, I+1, Ascent, TextDims,
	      Xi+Width, Yi, Width, Height, Halign, Valign);
draw_h_tabs(_Win, _W, _I, _Ascent, [], 
	  Xi, _Yi, _Width, _Height, _Halign, _Valign) ->
    Xi.

draw_v_tabs(Win, W, I, Ascent, [{{TxW,TxH},Text}|TextDims],
	    Xi, Yi, Width, Height, Halign, Valign) ->
    %% darken selected field
    if I =:= W#widget.value ->
	    Color = color_sub(?TABS_COLOR, 16#00333333),
	    epx_gc:set_fill_color(Color),
	    epx_gc:set_fill_style(solid),
	    epx:draw_rectangle(Win#widget.image, Xi, Yi, Width, Height);
       true ->
	    ok
    end,
    epx_gc:set_foreground_color(16#00000000),
    epx_gc:set_fill_style(none),
    epx:draw_rectangle(Win#widget.image, Xi, Yi, Width, Height),
    epx_gc:set_foreground_color(W#widget.font_color band 16#ffffff),
    draw_text(Win, Ascent, Text, TxW, TxH, Xi, Yi,
	      Width, Height, Halign, Valign),
    draw_v_tabs(Win, W, I+1, Ascent, TextDims,
		Xi, Yi+Height, Width, Height, Halign, Valign);
draw_v_tabs(_Win, _W, _I, _Ascent, [], 
	    _Xi, Yi, _Width, _Height, _Halign, _Valign) ->
    Yi.
    
    
draw_text(Win, Ascent, Text, TxW, TxH, X, Y, Width, Height, Halign, Valign) ->
    Xd = case Halign of
	     left  -> 0;
	     right -> Width - TxW;
	     center -> (Width-TxW) div 2
	 end,
    Yd = case Valign of
	     top -> 0;
	     bottom -> Height - TxH;
	     center -> (Height-TxH) div 2
	 end,
    %% draw aligned text
    X1 = X + Xd,
    Y1 = Y + Yd + Ascent,
    epx:draw_string(Win#widget.image, X1, Y1, Text).


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
	    #widget {color = Color, image = Image, animation = Anim,
		     frame = Frame } = W,
	    draw_one_background(Win, W, X, Y, Width, Height, 
				1, Color, Image, Anim, Frame)
    end.

draw_split_background(Win, W=#widget {orientation = horizontal}) ->
    #widget {width=Width, height=Height, x=X, y=Y} = W,
    #widget {color = Color, image = Image, 
	     animation = Anim, frame = Frame} = W,
    #widget {color2 = Color2, image2 = Image2, 
	     animation2 = Anim2, frame2 = Frame2} = W,
    R = value_proportion(W),
    draw_one_background(Win, W, X, Y, 
			trunc(R*Width), Height, 
			1, Color, Image, Anim, Frame),
    draw_one_background(Win, W, X + trunc(R*Width), Y,
			Width - trunc(R*Width), Height, 
			2, Color2, Image2, Anim2, Frame2);
draw_split_background(Win, W=#widget {orientation = vertical}) ->
    #widget {width=Width, height=Height, x=X, y=Y} = W,
    #widget {color = Color, image = Image,
	     animation = Anim, frame = Frame } = W,
    #widget {color2 = Color2, image2 = Image2,
	     animation2 = Anim2, frame2 = Frame2} = W,
    R = value_proportion(W),
    Y0 = Y + Height - 1,
    %% Bottom part
    draw_one_background(Win, W, X, trunc(Y0*(1-R) + Y*R),
			Width, trunc(R*(Y0-Y)) + 1,
			1, Color, Image, Anim, Frame),
    %% Top part
    draw_one_background(Win, W, X, Y,
			Width, trunc((1-R)*(Y0-Y)),
			2, Color2, Image2, Anim2, Frame2).

draw_one_background(Win,W,X,Y,Width,Height,N,Color,Image,Anim,Frame) ->
    epx_gc:set_fill_style(W#widget.fill),
    set_color(W, Color),
    epx:draw_rectangle(Win#widget.image, X, Y, Width, Height),
    
    if is_record(Image, epx_pixmap) ->
	    %% lager:debug("drawing image ~p", [Image]),
	    IWidth  = epx:pixmap_info(Image,width),
	    IHeight = epx:pixmap_info(Image,height),
	    if IWidth =:= Width, IHeight =:= Height ->
		    epx:pixmap_copy_area(Image,
					 Win#widget.image,
					 0, 0, X, Y, Width, Height,
					 [blend]);
	       true ->
		    epx:pixmap_scale_area(Image,
					  Win#widget.image,
					  0, 0, X, Y,
					  IWidth, IHeight,
					  Width, Height,
					  [blend])
	    end;
       true ->
	    ok
    end,
    if is_record(Anim, epx_animation) ->
	    %% lager:debug("drawing animation ~p", [Anim]),
	    AWidth  = epx:animation_info(Anim,width),
	    AHeight = epx:animation_info(Anim,height),
	    Count = epx:animation_info(Anim, count),
	    Frame0 = if is_number(Frame) -> Frame;
			true -> 0
		     end,
	    Frame1 = clamp(Frame0, 0, Count-1),
	    %% lager:debug("draw frame: ~w", [Frame1]),
	    if AWidth =:= Width, AHeight =:= Height ->
		    epx:animation_draw(Anim, round(Frame1),
				       Win#widget.image, epx_gc:current(),
				       X,Y);
	       true ->
		    AFormat = epx:animation_info(Anim,pixel_format),
		    TmpImage = create_tmp_pixmap(AFormat,AWidth,AHeight),
		    epx:animation_draw(Anim, round(Frame1),
				       TmpImage, epx_gc:current(),
				       0,0),
		    epx:pixmap_scale_area(TmpImage,
					  Win#widget.image,
					  0, 0, X, Y,
					  AWidth, AHeight,
					  Width, Height,
					  [blend])
	    end,
	    %% fixme: animation step?
	    update_animation(W, N, Frame0+1, Count);
       true ->
	    ok
    end.

update_animation(W, 1, Frame, Count) ->
    case W#widget.animate of
	continuous ->
	    put(animations, true),
	    widget_store(W#widget { frame = fmod(Frame,Count)});
	sequence ->
	    if Frame >= Count ->
		    widget_store(W#widget { animate = undefined });
	       true ->
		    put(animations, true),
		    widget_store(W#widget { frame = Frame})
	    end;
	undefined ->
	    W
    end;
update_animation(W, 2, Frame, Count) ->
    case W#widget.animate2 of
	continuous ->
	    put(animations, true),
	    widget_store(W#widget { frame2 = fmod(Frame, Count) });
	sequence ->
	    if Frame >= Count ->
		    widget_store(W#widget { animate2 = undefined });
	       true ->
		    put(animations, true),
		    widget_store(W#widget { frame2 = Frame})
	    end;
	undefined ->
	    W
    end.


fmod(A, B) when is_integer(A), is_integer(B), B =/= 0 ->
    A rem B;
fmod(A, B) when is_number(A), is_number(B), B =/= 0 ->
    AB = abs(A / B),
    C = (AB - trunc(AB))*abs(B),
    if A < 0 -> -C;
       true -> C
    end.    
    

%% A bit ugly but may be efficient?
create_tmp_pixmap(Format, Width, Height) ->
    Pixmap =
	case get(tmp_pixmap) of
	    undefined ->
		epx:pixmap_create(Width, Height, Format);
	    Pixmap0 ->
		F = epx:pixmap_info(Pixmap0,pixel_format),
		W  = epx:pixmap_info(Pixmap0,width),
		H = epx:pixmap_info(Pixmap0,height),
		if Format =:= F, Width =< W, Height =< H ->
			Pixmap0;
		   true ->
			epx:pixmap_create(Width, Height, Format)
		end
	end,
    epx:pixmap_fill(Pixmap, 0),
    put(tmp_pixmap, Pixmap),
    Pixmap.

    
draw_border(_Win, _W, undefined) ->
    ok;
draw_border(_Win, _W, 0) ->
    ok;
draw_border(Win, W, _Border) ->
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
		     X0 = W#widget.x,
		     X1 = W#widget.x + W#widget.width - 1,
		     Xv = trunc(X0*(1-R) + X1*R),
		     {Xv - (Width div 2),
		      W#widget.y + (W#widget.height- Height) div 2};
		 vertical ->
		     Y0 = W#widget.y + W#widget.height - 1,
		     Y1 = W#widget.y,
		     Yv = trunc(Y0*(1-R) + Y1*R),
		     {W#widget.x + (W#widget.width - Width) div 2,
		      (Yv - (Height div 2))}
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
    Color = case W#widget.state of 
		active ->
		    color_sub(Color0, 16#00333333);  %% darken color when active
		_ ->
		    Color0
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
