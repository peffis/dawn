-module(dawn).
-export([cast/2, call/2]).

%% API

%% @doc will apply Fun to Args on the next node in the cluster without 
%% waiting for the fun to return, returns ok or {dawn_error, not_available}
cast(Fun, Args) ->
    Node = dawn_ctrl:next_node(),
    cast(Node, Fun, Args).

    
%% @doc will apply Fun to Args on the next node in the cluster and 
%% will wait for the fun to return, it returns whatever the fun returned 
%% or {dawn_error, not_available}
call(Fun, Args) ->
    Node = dawn_ctrl:next_node(),
    call(Node, Fun, Args).



%% helper functions    
cast(undefined, _Fun, _Args) ->
    {dawn_error, not_available};
cast(Node, Fun, Args) ->
    spawn(Node, fun() -> _Ret = apply(Fun, Args) end),
    ok.

call(undefined, _Fun, _Args) ->
    {dawn_error, not_available};
call(Node, Fun, Args) ->
    Ref = make_ref(),
    CallerPid = self(),

    spawn(Node, fun() ->
			Ret = apply(Fun, Args),
			CallerPid ! {call_return, Ref, Ret}
		end),
    
    receive 
	{call_return, Ref, Ret} ->
	    Ret
    end.
