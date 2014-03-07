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
-export([add_event/3, del_event/1]).
-export([init_event/2, output_event/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("epx/include/epx.hrl").

-define(SERVER, ?MODULE).

-record(widget,
	{
	  ref,       %% generate id  (inputs&outputs)
	  id,        %% named widget (outputs)
	  type,      %% button,rectangle,slider ...
	  signal,    %% signal output
	  callback,  %% input callback
	  window = default,
	  state  = normal,  %% or selected ..
	  x = 0,
	  y = 0,
	  width  = 32,
	  height = 32,
	  text = "",
	  image   :: epx:epx_window(),
	  color = 16#ffff0000,  %% red
	  fill   = none,
	  events = [],

	  %% type text
	  font    :: epx:epx_font(),
	  %% type = window
	  win     :: epx:epx_window(),
	  backing :: epx:epx_window()
	}).

-record(state, {
	  redraw_timer = undefined,
	  windows = [] :: [#widget{}],
	  widgets = [] :: [#widget{}]
	 }).

add_event(Flags, Signal, Cb) ->
    gen_server:call(?MODULE, {add_event, Flags, Signal, Cb}).

del_event(Ref) ->
    gen_server:call(?MODULE, {del_event, Ref}).

output_event(Flags, Env) ->
    gen_server:call(?MODULE, {output_event, Flags, Env}).

init_event(Dir, Flags) ->
    gen_server:call(?MODULE, {init_event, Dir, Flags}).

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
init([]) ->
    Default = window_create([{id,default},
			     {x,50},{y,50},
			     {width,320},{height,240},
			     {events, [key_press,key_release,
				       button_press, button_release,
				       %% configure,resize,focus,
				       %% crossing, motion
				       button,wheel]},
			     {color, 16#ff000000}]),
    self() ! refresh,
    {ok, #state{ windows = [Default], widgets = []}}.

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
handle_call({add_event,Flags,Signal, Cb}, _From, State) ->
    case lists:keyfind(id, 1, Flags) of
	false ->
	    try widget_create(Flags) of
		W ->
		    Ref = make_ref(),
		    W1  = W#widget { ref = Ref, 
				     callback = Cb,
				     signal   = Signal },
		    Ws = [W1|State#state.widgets],
		    self() ! refresh,
		    {reply, {ok,Ref}, State#state { widgets = Ws }}
	    catch
		error:Reason ->
		    {reply, {error,Reason}, State}
	    end;
	{id,ID} ->
	    case lists:keyfind(ID, #widget.id, State#state.widgets) of
		false ->
		    {reply, {error,enoent}, State};
		W ->
		    {reply, {ok,W#widget.ref}, State}
	    end
    end;
handle_call({del_event,Ref}, _From, State) ->
    case lists:keytake(Ref, #widget.ref, State#state.widgets) of
	false ->
	    {reply, {error, not_found}, State};
	{value, _W, Ws} ->
	    {reply, ok, State#state { widgets=Ws} }
    end;
handle_call({output_event,Flags,Env}, _From, State) ->
    case lists:keyfind(id, 1, Flags) of
	false ->
	    {reply,{error,missing_id},State};
	{id,ID} ->
	    Ws = State#state.widgets,
	    case lists:keytake(ID,#widget.id,Ws) of
		false ->
		    {reply,{error,enoent},State};
		{value,W,Ws1} ->
		    try widget_set(Env, W) of
			W1 ->
			    Ws2 = [W1|Ws1],
			    self() ! refresh,
			    {reply, ok, State#state{widgets=Ws2}}
		    catch
			error:Reason ->
			    {reply, {error,Reason}, State}
		    end
	    end
    end;
handle_call({init_event,Dir,Flags}, _From, State) ->
    case lists:keyfind(id, 1, Flags) of
	false when Dir =:= in ->
	    {reply, ok, State};
	false ->
	    {reply,{error,missing_id},State};
	{_,ID} ->
	    Ws = State#state.widgets,	
	    case lists:keyfind(ID,#widget.id,Ws) of
		false ->
		    try widget_create(Flags) of
			W ->
			    W1 = W#widget { ref=make_ref() },
			    Ws1 = [W1|Ws],
			    self() ! refresh,
			    {reply, ok, State#state{widgets=Ws1}}
		    catch
			error:Reason ->
			    {reply, {error,Reason}, State}
		    end;
		_W ->
		    {reply, {error, ealready}, State}
	    end
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
    Windows = State#state.windows,
    case lists:keyfind(Win, #widget.win, Windows) of
	false ->
	    lager:error("window not found"),
	    {noreply, State};
	W ->
	    case Event of
		{key_press,_Sym,_Mod,_Code} ->
		    {noreply, State};
		{key_release,_Sym,_Mod,_Code} ->
		    {noreply, State};
		{button_press,_Button,_Where} ->
		    {noreply, State};
		{button_release,_Button,_Where} ->
		    {noreply, State};
		{motion,_Button, _Where} ->
		    {noreply, State};
		{configure,{_X,_Y,_Width,_Height}} ->
		    {noreply, State};
		{resize,{_Width,_Height,_Depth}} ->
		    {noreply, State};
		{enter, _Where} ->
		    {noreply, State};
		{leave, _Where} ->
		    {noreply, State};
		focus_in ->
		    {noreply, State};
		focus_out ->
		    {noreply, State};
		close ->
		    if W#widget.id =:= default ->
			    {noreply, State};
		       true ->
			    unmap_window(W),
			    Windows1 = lists:keydelete(W#widget.id, #widget.id,
						       Windows),
			    {noreply, State#state { windows = Windows1 }}
		    end;
		_ ->
		    lager:error("unknown event"),
		    {noreply, State}
	    end
    end;
handle_info(refresh, State) ->
    if is_reference(State#state.redraw_timer) ->
	    {noreply, State};
       State#state.redraw_timer =:= undefined ->
	    Timer = erlang:start_timer(50, self(), redraw),
	    {noreply, State#state { redraw_timer = Timer }}
    end;
handle_info({timeout,TRef,redraw}, State) 
  when TRef =:= State#state.redraw_timer ->
    {noreply,  redraw_state(State#state { redraw_timer=undefined})};
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
    [ unmap_window(Win) || Win <- State#state.windows],
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

widget_create(Flags) ->
    widget_set(Flags, #widget{}).

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
		    widget_set(Flags, W#widget{image=Image});
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
	    widget_set(Flags, W#widget{events=Es})
    end;
widget_set([], W) ->
    W.

redraw_state(State) ->
    [clear_window(Win) || Win <- State#state.windows],
    State1 = draw_widgets(State#state.widgets, State),
    [update_window(Win) || Win <- State#state.windows],
    State1.

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



draw_widgets([W|Ws], State) ->
    %% draw widget W in it's window given by W#widget.window
    case lists:keyfind(W#widget.window, #widget.id, State#state.windows) of
	false ->
	    lager:error("missing window id=~w\n", [W#widget.window]),
	    draw_widgets(Ws, State);
	Win ->
	    draw_widget(W, Win),
	    draw_widgets(Ws, State)
    end;
draw_widgets([], State) ->
    State.


draw_widget(W, Win) ->
    case W#widget.type of
	rectangle ->
	    epx_gc:draw(
	      fun() ->
		      epx_gc:set_fill_style(W#widget.fill),
		      epx_gc:set_foreground_color(W#widget.color),
		      epx_gc:set_fill_color(W#widget.color),
		      epx:draw_rectangle(Win#widget.image, 
					 W#widget.x, W#widget.y,
					 W#widget.width, W#widget.height)
	      end);
	ellipse ->
	    epx_gc:draw(
	      fun() ->
		      epx_gc:set_fill_style(W#widget.fill),
		      epx_gc:set_foreground_color(W#widget.color),
		      epx_gc:set_fill_color(W#widget.color),
		      epx:draw_ellipse(Win#widget.image, 
				       W#widget.x, W#widget.y,
				       W#widget.width, W#widget.height)
	      end);
	line ->
	    epx_gc:draw(
	      fun() ->	
		      epx_gc:set_foreground_color(W#widget.color),    
		      epx:draw_line(Win#widget.image, 
				    W#widget.x, W#widget.y,
				    W#widget.x+W#widget.width-1,
				    W#widget.y+W#widget.height-1)
	      end);
	text ->
	    epx_gc:draw(
	      fun() ->
		      epx_gc:set_foreground_color(W#widget.color),
		      epx_gc:set_font(W#widget.font),
		      epx:draw_string(Win#widget.image,
				      W#widget.x, W#widget.y,
				      W#widget.text)
	      end)
    end.
