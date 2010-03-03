%%%------------------------------------------------------------------------
%%% File    : mod_warm_bindings.erl
%%% Author  : Theo Cushion <theo@jivatechnology.com>
%%% Purpose : Enables the creation of authenticated BOSH sessions via HTTP
%%% Created : 29/03/2010
%%%------------------------------------------------------------------------

-module(mod_eventful).
-author('theo@jivatechnology.com').

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
    ok.

unset_presence_log(User, Server, Resource, Status) ->
    ?INFO_MSG("unset_presence_triggered user: ~p, server: ~p, resource: ~p, status: ~p", [User, Server, Resource, Status]),
    ok.

%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------
    
start(Host, _Opts) ->
    ejabberd_hooks:add(set_presence_hook,   Host, ?MODULE, set_presence_log,   50),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, unset_presence_log, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(set_presence_hook,   Host, ?MODULE, set_presence_log,   50),
    ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, unset_presence_log, 50),
    ok.
