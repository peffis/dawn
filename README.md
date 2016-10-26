# dawn
A simple library for making rpc calls to nodes in a cluster. A
controlling node monitors nodes as they join the cluster and checks if 
they are configured to be used as "compute nodes". When you later make
function calls, either synchronous (with the "call" function) or
asynchronous (with the "cast" function) the call will be evaluated on
the "next" node, where the "next" node is chosen in a simple
round-robin fashion among the nodes that have been connected to the
cluster and are configured to take part in the computations (by
setting available_for_processing to "true" in sys.config).

This, rather naive, approach is obviously not suitable for all types
of function calls as the cost for calling a function is relatively
high, but when you want to kick off larger jobs that will take time
and you want to spread those jobs out over the entire cluster this
could be useful, for instance when the function you are calling has a
lot of side effects (such as calling scripts in the underlying os). 

## Installing
1. Add dawn dependency to your erlang.mk Makefile (if you use rebar
you will need to figure it out on your own)
   ```
   DEPS = dawn
   dep_dawn = git https://github.com/peffis/dawn master
   ```

   **Note:** You will need this dependency on all nodes that will take
   part in the compute cluster. The application dawn does however only have to
   *run* on at least one of the nodes in the cluster. The rest, that
   just are slaves and will run functions, will just have to have the
   code. 

2. If you are using a .app.src you should add it to the applications list
   ```
   {applications, [
			...,
			dawn,
			...

   ]},
   ```

3. For nodes in your cluster that you want to be available for
processing you add the following to sys.config (nodes in the cluster
that do not have this setting will not do any processing). 
   ```
[
...

 {dawn, [
         {available_for_processing, true}
        ]}
...
]
   ```




## Usage
1. Making a function call (will wait for the call to complete)
   ```
   Fun = ...
   Ret = dawn:call(Fun, Args)
   
   Example:
   > Ret = dawn:call(fun os:cmd/1, ["shutdown -r now"]).
   > Ret.
   "The system is going down for system reboot NOW!"
   >
   
   ```
   The call will be evaluated on the "next" available node, where "next"
   is chosen in a round-robin fashion. The call is blocking and the 
   value of the function evaluated is returned. 


2. Making a function cast (a non-blocking, asynchronous call)
   ```
   Fun = ...
   dawn:cast(Fun, Args)
   
   Example:
   > dawn:cast(fun os:cmd/1, ["shutdown -r now"]).
   ok
   >
   ```
   The call will be evaluated on the "next" available node, where "next"
   is chosen in a round-robin fashion. The call is non-blocking and no 
   return value is available. 

