%%%-------------------------------------------------------------------
%%% File:      meck_send.erl
%%% @author    cliff moon <> []
%%% @copyright 2011 fast_ip
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2011-03-05 by cliff moon
%%%-------------------------------------------------------------------
-module(meck_send).
-author('').

-behaviour(gen_server).

%% API
-export([new/1, add_listener/3, add_remote/3, remote/2, unload/1, unload/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {name,mod,original,listeners,remotes}).

%% Mocks out all the send operators in Mod and starts a new gen_server to proxy
%% any messages sent from within this module.
new(Mod) ->
  Name = name(Mod),
  gen_server:start_link({local, Name}, ?MODULE, [Mod], []).

%% Adds either a pid, fun, or atom as a listener for any messages sent from within Mod
add_listener(Mod, Destination, Listener) when is_function(Listener); is_atom(Listener); is_pid(Listener) ->
  Name = name(Mod),
  gen_server:call(Name, {add_listener,Destination,Listener}).

%% Adds a fun to be invoked instead of {Module,Function}. If no function is already mapped to that {M,A},
%% then this will trigger a reload of the underlying module to swap out any calls.
add_remote(Mod, {Module,Function}, Fun) when is_function(Fun) ->
  gen_server:call(name(Mod), {add_remote, {Module,Function}, Fun}).

%% Used to proxy calls from any mocked out remote invocations.
remote(Name, {Module,Function,Args}) ->
  gen_server:call(Name, {remote, {Module,Function}, Args}).

%% Unloads any modules being mocked by meck_send
unload() ->
  lists:foldl(fun(P,L) -> meck_module:unload_if_mocked(P,L,"_meck_sender", fun unload/1) end, [], registered()).

%% Unloads Mod. Assumes it is being mocked by meck_send.
unload(Mod) ->
  Name = name(Mod),
  gen_server:call(Name, stop).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
%% @end 
%%--------------------------------------------------------------------
init([Mod]) ->
  Name = name(Mod),
  Original = meck_module:backup_original(Mod),
  process_flag(trap_exit, true),
  Forms = meck_interceptor:new(Mod, [{op, sends(Name)}]),
  meck_module:compile_forms(Forms),
  {ok, #state{name=Name,mod=Mod,original=Original,listeners=dict:new(),remotes=dict:new()}}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end 
%%--------------------------------------------------------------------
handle_call({add_remote,Remote,Fun}, _From, State = #state{mod=Mod,name=Name,remotes=Remotes}) ->
  Remotes2 = dict:store(Remote,Fun,Remotes),
  case dict:is_key(Remote,Remotes) of
    true ->
      ok;
    _ ->
      Forms = meck_interceptor:new(Mod, [{op, sends(Name)},{call, call_remotes(Mod, Remotes2)}]),
      meck_module:compile_forms(Forms)
  end,
  {reply, ok, State#state{remotes=Remotes2}};
handle_call({add_listener,Destination,Listener}, _From, State = #state{listeners=Listeners}) ->
  Pid = to_pid(Listener),
  Listeners2 = dict:store(Destination,Pid,Listeners),
  {reply, ok, State#state{listeners=Listeners2}};
handle_call({remote, Remote={M,F}, Args}, _From, State = #state{remotes=Remotes}) ->
  spawn(fun() ->
    Reply = case dict:find(Remote, Remotes) of
      {ok, Fun} ->
          apply(Fun,Args);
        error ->
          apply(M,F,Args)
      end,
      gen_server:reply(_From,Reply)
    end),
  {noreply, State};
handle_call(stop, _From, State) ->
  {stop,normal,ok,State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info({Destination,Msg}, State = #state{listeners=Listeners}) ->
  case dict:find(Destination, Listeners) of
    {ok, Pid} -> 
      Pid ! Msg;
    error ->
      Destination ! Msg
  end,
  {noreply, State};
handle_info({'EXIT',_Pid,_Reason}, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, #state{mod=Mod,original=Original}) ->
  meck_module:cleanup(Mod),
  meck_module:restore_original(Mod, Original),
  ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
name(Mod) ->
  list_to_atom(atom_to_list(Mod) ++ "_meck_sender").

sends(Name) ->
  fun({op,Line,'!',Dest,Msg}) ->
      {op,Line,'!',{atom,Line,Name},{tuple,Line,[Dest,Msg]}};
    (F) ->
      F
    end.

call_remotes(Mod, Remotes) ->
  fun
    (C = {call,L1,{remote,L2,{atom,L3,Module},{atom,L4,Fun}},Args}) ->
      case dict:is_key({Module,Fun},Remotes) of
        true -> {call,L1,
                  {remote,L2,{atom,L3,meck_send},{atom,L4,remote}},[
                    {atom,?LINE,name(Mod)},
                    {tuple,?LINE,[{atom,?LINE,Module},{atom,?LINE,Fun},if
                        length(Args) > 0 -> abstract(Args);
                        true -> {nil,?LINE}
                      end]}]};
        _ -> C
      end;
    (F) -> F
  end.

abstract([]) -> {nil,?LINE};
abstract([Arg|Args]) -> {cons,?LINE,Arg,abstract(Args)}.

to_pid(Fun) when is_function(Fun) -> spawn_link(Fun);
to_pid(A) -> A.
