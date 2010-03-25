%%%------------------------------------------------------------------------
%%% File    : mod_warm_bindings.erl
%%% Author  : Theo Cushion <theo@jivatechnology.com>
%%%         : Nicolas Alpi <nicolas.alpi@gmail.com>
%%% Purpose : Enables the creation of authenticated BOSH sessions via HTTP
%%% Created : 29/03/2010
%%%------------------------------------------------------------------------

-module(mod_eventful).
-author('theo@jivatechnology.com').
-author('nicolas.alpi@gmail.com').

-behaviour(gen_mod).

-export([
    start/2,
    stop/1,
    set_presence_log/4,
    unset_presence_log/4
    ]).

-include("ejabberd.hrl").
-include("jlib.hrl").

%%%----------------------------------------------------------------------
%%% EVENT HANDLERS
%%%----------------------------------------------------------------------

set_presence_log(User, Server, Resource, Presence) ->
    ?INFO_MSG("set_presence_triggered user: ~p, server: ~p, resource: ~p, presence: ~p", [User, Server, Resource, Presence]),
    post_results(set_presence_hook, User, Server, Resource, lists:flatten(xml:element_to_string(Presence))),
    ok.

unset_presence_log(User, Server, Resource, Status) ->
    ?INFO_MSG("unset_presence_triggered user: ~p, server: ~p, resource: ~p, status: ~p", [User, Server, Resource, Status]),
    post_results(unset_presence_hook, User, Server, Resource, Status),
    ok.
    
%%%----------------------------------------------------------------------
%%% HELPER FUNCTIONS
%%%----------------------------------------------------------------------

post_results(Event, User, Server, Resource, Message) ->
    Url          = url_for(Event),
    AuthUser     = gen_mod:get_module_opt(global, ?MODULE, user,     undefined),
    AuthPassword = gen_mod:get_module_opt(global, ?MODULE, password, undefined),
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
    if is_list(Url) ->
        http:request(
            post, {
                Url,
                Headers,
                "application/x-www-form-urlencoded", Data
            },
            [],
            []
        )
    end,
    
    ok.

url_for(Event) ->
    Urls = gen_mod:get_module_opt(global, ?MODULE, url, []),
    case lists:keysearch(Event,1,Urls) of
        {value,{_,Result}} -> Url = Result;
        _                  -> Url = undefined
    end,
    Url.
    
%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------
    
start(Host, _Opts) ->
    inets:start(),
    ejabberd_hooks:add(set_presence_hook,   Host, ?MODULE, set_presence_log,   50),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, unset_presence_log, 50),
    ok.
    
stop(Host) ->
    ejabberd_hooks:delete(set_presence_hook,   Host, ?MODULE, set_presence_log,   50),
    ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, unset_presence_log, 50),
    ok.
