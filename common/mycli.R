## FHE 02 Mar 2016

# Define a variable which, when accessed, calls a given function.
# Useful for command-line shortcuts.

functionAsVariable=function(varName,func) {
  helperName=paste0(".",varName,".helper");

  # calling helperName assigns a delayed-assign function to varName
  assign(helperName,
         envir=.GlobalEnv,
         function() {
           delayedAssign(varName,
                         assign.env=.GlobalEnv,
                         {
                           # first re-assign ourselves
                           do.call(helperName,list());
                           # then call the function
                           func();
#                           invisible(NULL)
                         })
         }
         );
  do.call(helperName,list());
}

# Example: bind 'bt' to traceback()
functionAsVariable("bt",function(){traceback(max.lines=1)})


