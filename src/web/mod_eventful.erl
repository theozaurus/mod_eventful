%%%------------------------------------------------------------------------
%%% File    : mod_eventful.erl
%%% Author  : Theo Cushion <theo@jivatechnology.com>
%%%         : Nicolas Alpi <nicolas.alpi@gmail.com>
%%% Purpose : Enables events triggered within ejabberd to generate HTTP
%%%           POST requests to an external service.
%%% Created : 29/03/2010
%%%------------------------------------------------------------------------

-module(mod_eventful).
-author('theo@jivatechnology.com').
-author('nicolas.alpi@gmail.com').

-behaviour(gen_server).
-behaviour(gen_mod).

-define(PROCNAME, ?MODULE).

%% event handlers
-export([
    set_presence_log/4,
    unset_presence_log/4
    ]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
    ]).

%% gen_mod callbacks.
-export([
    start/2,
    stop/1,
    start_link/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(state, {host, urls, auth_user, auth_password}).

%%====================================================================
%% Event handlers
%%====================================================================

set_presence_log(User, Server, Resource, Presence) ->
    post_results(set_presence_hook, User, Server, Resource, lists:flatten(xml:element_to_string(Presence))),
    case ejabberd_sm:get_user_resources(User,Server) of
        [_] ->        
            %%% First connection, so user has just come online
            post_results(online_hook, User, Server, Resource, lists:flatten(xml:element_to_string(Presence)));
        _ ->
            false
    end,
    ok.

unset_presence_log(User, Server, Resource, Status) ->
    post_results(unset_presence_hook, User, Server, Resource, Status),
    case ejabberd_sm:get_user_resources(User,Server) of
        [] ->
            %%% No more connections, so user is totally offline
            %%% This occurs when a BOSH connection timesout
            post_results(offline_hook, User, Server, Resource, Status);
        [Resource] ->
            %%% We know that 'Resource' is no longer online, so can treat as if user is totally offline
            %%% This occurs when a user logs out
            post_results(offline_hook, User, Server, Resource, Status);
        _ ->
            false
    end,
    ok.
    
%%====================================================================
%% Internal functions
%%====================================================================

post_results(Event, User, Server, Resource, Message) ->
    Proc = gen_mod:get_module_proc(Server, ?PROCNAME),
    gen_server:call(Proc, {post_results, Event, User, Server, Resource, Message}).

url_for(Event, Urls) ->
    case lists:keysearch(Event,1,Urls) of
        {value,{_,Result}} -> Url = Result;
        _                  -> Url = undefined
    end,
    Url.

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, _Opts]) ->
    inets:start(),
    ejabberd_hooks:add(set_presence_hook,   Host, ?MODULE, set_presence_log,   50),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, unset_presence_log, 50),
    Urls         = gen_mod:get_module_opt(global, ?MODULE, url, []),
    AuthUser     = gen_mod:get_module_opt(global, ?MODULE, user,     undefined),
    AuthPassword = gen_mod:get_module_opt(global, ?MODULE, password, undefined),
    {ok, #state{host = Host, urls = Urls, auth_user = AuthUser, auth_password = AuthPassword}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({post_results, Event, User, Server, Resource, Message}, _From, State) ->
    Urls         = State#state.urls,
    Url          = url_for(Event, Urls),
    AuthUser     = State#state.auth_user,
    AuthPassword = State#state.auth_password,
    case is_list(AuthUser) andalso is_list(AuthPassword) of
        true  -> 
            UserPassword = base64:encode_to_string(AuthUser ++ ":" ++ AuthPassword),
            Headers      = [{"Authorization", "Basic " ++ UserPassword}];
        false ->
            Headers = []
    end,
    
    Data = "user="      ++ ejabberd_http:url_encode(User) ++
           "&server="   ++ ejabberd_http:url_encode(Server) ++
           "&resource=" ++ ejabberd_http:url_encode(Resource) ++ 
           "&message="  ++ ejabberd_http:url_encode(Message),
    case is_list(Url) of
        true ->
            ?INFO_MSG("Triggered: ~p, user: ~p, server: ~p, resource: ~p, message: ~p",[Event, User, Server, Resource, Message]),
            http:request(
                post, {
                    Url,
                    Headers,
                    "application/x-www-form-urlencoded", Data
                },
                [],
                [{sync, false},{stream, self}]
            );
        false ->
            false
    end,
    {reply, ok, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.
    
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({http, {RequestId, stream_start, Headers}}, State) ->
    ?DEBUG("http stream_start RequestId: ~p, Headers: ~p",[RequestId, Headers]),
    {noreply, State};
handle_info({http, {RequestId, stream, BinBodyPart}}, State) ->
    ?DEBUG("http stream RequestId: ~p, BinBodyPart: ~p",[RequestId, BinBodyPart]),
    {noreply, State};
handle_info({http, {RequestId, stream_end, Headers}}, State) ->
    ?DEBUG("http stream_end RequestId: ~p, Headers: ~p",[RequestId, Headers]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    Host = State#state.host,
    ejabberd_hooks:delete(set_presence_hook,   Host, ?MODULE, set_presence_log,   50),
    ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, unset_presence_log, 50),
    ok.
    
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =	{
        Proc,
	    {?MODULE, start_link, [Host, Opts]},
	    transient,
	    1000,
	    worker,
	    [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).
    
stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:delete_child(ejabberd_sup, Proc).
