# dawn
A framework for making rpc calls to nodes in a cluster

## Installing
1. Add dawn dependency to your Makefile
   ```
   DEPS = dawn
   dep_dawn = git https://github.com/peffis/dawn master
   ```

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
   Ret = dawn:call(fun os:cmd/1, ["shutdown -r now"]),
   ...
   
   ```
   The call will be evaluated on the "next" available node, where "next"
   is chosen in a round-robin fashion. The call is blocking and the 
   value of the function evaluated is returned. 


2. Making a function cast (a non-blocking, asynchronous call)
   ```
   Fun = ...
   dawn:cast(Fun, Args)
   
   Example:
   dawn:call(fun os:cmd/1, ["shutdown -r now"]),
   ...
   ```
   The call will be evaluated on the "next" available node, where "next"
   is chosen in a round-robin fashion. The call is non-blocking and no 
   return value is available. 

