-module(dawn_ctrl).
-behaviour(gen_server).

-compile({no_auto_import,[nodes/1]}).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([next_node/0, nodes/0]).

-record(state, {
	  nodes
}).

%% API.

next_node() ->
    next_node(global:whereis_name(dawn_ctrl)).

nodes() ->
    nodes(global:whereis_name(dawn_ctrl)).


-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
    (global:register_name(dawn_ctrl, self()) == yes) andalso    
	net_kernel:monitor_nodes(true),

    InitialNodes = case should_be_added(node()) of %% should we add ourselves?
		       true -> [node()];

		       _ -> []
		   end,

    {ok, #state{nodes=queue:from_list(InitialNodes)}}.


handle_call(nodes, _From, State) ->
    {reply, State#state.nodes, State};

handle_call(next_node, _From, State) ->

    {Next, Rest} = queue:out(State#state.nodes),
    case Next of 
	{value, NextNode} ->
	    {reply, NextNode, State#state{nodes = queue:in(NextNode, Rest)}};
	
	empty ->
	    {reply, undefined, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodeup, Node}, State) ->

    Nodes = case should_be_added(Node) of
		false -> State#state.nodes;
	        _ -> queue:in(Node, State#state.nodes)
	    end,

    {noreply, State#state{nodes = Nodes}};

handle_info({nodedown, Node}, State) ->
    Nodes = queue:filter(fun(N) -> N /= Node end, State#state.nodes),
    {noreply, State#state{nodes = Nodes}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% helper functions
should_be_added(Node) ->
    case rpc:call(Node, application, get_env, 
		  [dawn, available_for_processing]) of

	{ok, Bool} ->
	    Bool;

	_ -> false

    end.


next_node(undefined) ->
    undefined;
next_node(Pid) ->
    gen_server:call(Pid, next_node).

nodes(undefined) ->
    [];
nodes(Pid) ->
    queue:to_list(gen_server:call(Pid, nodes)).

