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
   ```

...
]


## Usage
