-module(dawn_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [
	     {dawn_ctrl, {dawn_ctrl, start_link, []}, 
	      permanent, 10000, worker, [dawn_ctrl]}
	    ],
    {ok, {{one_for_one, 1, 5}, Procs}}.
