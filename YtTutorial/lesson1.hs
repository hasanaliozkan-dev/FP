-- [1] ++ [2,3,4,5] == 1: [2,3,4,5] 
-- GHC stands for Glasgow Haskell Compiler
qSort [] = []
qSort(x:xs) = qSort ys ++ [x] ++ qSort(zs)
              where
                ys = [a | a<-xs , a <= x]
                zs = [b | b<-xs , b > x]

--head [1,2,3,4,5] returns 1
--tail [1,2,3,4,5] returns [2,3,4,5]
--[1,2,3,4,5]!!2 returns 3
--take 3 [1,2,3,4,5] returns [1,2,3]
--take 3 [1,2,3,4,5] returns [4,5]
--length [1,2,3,4,5] returns 5
--sum [1,2,3,4,5] returns 15
--product [1,2,3,4,5] returns 120
--reverse [1,2,3,4,5] returns [5,4,3,2,1]
--In math -> f(a,b) + cd | In haskell f a b + c*d
--In haskell f a + b means that f(a) + b

double x = x + x
quadruple x = double(double x) 

--take (double 2) [1,2,3,4,5,6,7] returns [1,2,3,4]

factorial n = product [1..n]

average ns = sum ns  `div` length ns


{-
 <statement>                 evaluate/run <statement>
   :                           repeat last command
   :{\n ..lines.. \n:}\n       multiline command
   :add [*]<module> ...        add module(s) to the current target set
   :browse[!] [[*]<mod>]       display the names defined by module <mod>
                               (!: more details; *: all top-level names)
   :cd <dir>                   change directory to <dir>
   :cmd <expr>                 run the commands returned by <expr>::IO String
   :complete <dom> [<rng>] <s> list completions for partial input string
   :ctags[!] [<file>]          create tags file <file> for Vi (default: "tags")
                               (!: use regex instead of line number)
   :def[!] <cmd> <expr>        define command :<cmd> (later defined command has
                               precedence, ::<cmd> is always a builtin command)
                               (!: redefine an existing command name)
   :doc <name>                 display docs for the given name (experimental)
   :edit <file>                edit file
   :edit                       edit last module
   :etags [<file>]             create tags file <file> for Emacs (default: "TAGS")
   :help, :?                   display this list of commands
   :info[!] [<name> ...]       display information about the given names
                               (!: do not filter instances)
   :instances <type>           display the class instances available for <type>
   :issafe [<mod>]             display safe haskell information of module <mod>
   :kind[!] <type>             show the kind of <type>
                               (!: also print the normalised type)
   :load[!] [*]<module> ...    load module(s) and their dependents
                               (!: defer type errors)
   :main [<arguments> ...]     run the main function with the given arguments
   :module [+/-] [*]<mod> ...  set the context for expression evaluation
   :quit                       exit GHCi
   :reload[!]                  reload the current module set
                               (!: defer type errors)
   :run function [<arguments> ...] run the function with the given arguments
   :script <file>              run the script <file>
   :type <expr>                show the type of <expr>
   :type +d <expr>             show the type of <expr>, defaulting type variables
   :type +v <expr>             show the type of <expr>, with its specified tyvars
   :unadd <module> ...         remove module(s) from the current target set
   :undef <cmd>                undefine user-defined command :<cmd>
   ::<cmd>                     run the builtin command
   :!<command>                 run the shell command <command>

 -- Commands for debugging:

   :abandon                    at a breakpoint, abandon current computation
   :back [<n>]                 go back in the history N steps (after :trace)
   :break [<mod>] <l> [<col>]  set a breakpoint at the specified location
   :break <name>               set a breakpoint on the specified function
   :continue [<count>]         resume after a breakpoint [and set break ignore count]
   :delete <number> ...        delete the specified breakpoints
   :delete *                   delete all breakpoints
   :disable <number> ...       disable the specified breakpoints
   :disable *                  disable all breakpoints
   :enable <number> ...        enable the specified breakpoints
   :enable *                   enable all breakpoints
   :force <expr>               print <expr>, forcing unevaluated parts
   :forward [<n>]              go forward in the history N step s(after :back)
   :history [<n>]              after :trace, show the execution history
   :ignore <breaknum> <count>  for break <breaknum> set break ignore <count>
   :list                       show the source code around current breakpoint
   :list <identifier>          show the source code for <identifier>
   :list [<module>] <line>     show the source code around line number <line>
   :print [<name> ...]         show a value without forcing its computation
   :sprint [<name> ...]        simplified version of :print
   :step                       single-step after stopping at a breakpoint
   :step <expr>                single-step into <expr>
   :steplocal                  single-step within the current top-level binding
   :stepmodule                 single-step restricted to the current module
   :trace                      trace after stopping at a breakpoint
   :trace <expr>               evaluate <expr> with tracing on (see :history)

 -- Commands for changing settings:

   :set <option> ...           set options
   :seti <option> ...          set options for interactive evaluation only
   :set local-config { source | ignore }
                               set whether to source .ghci in current dir
                               (loading untrusted config is a security issue)
   :set args <arg> ...         set the arguments returned by System.getArgs
   :set prog <progname>        set the value returned by System.getProgName
   :set prompt <prompt>        set the prompt used in GHCi
   :set prompt-cont <prompt>   set the continuation prompt used in GHCi
   :set prompt-function <expr> set the function to handle the prompt
   :set prompt-cont-function <expr>
                               set the function to handle the continuation prompt
   :set editor <cmd>           set the command used for :edit
   :set stop [<n>] <cmd>       set the command to run when a breakpoint is hit
   :unset <option> ...         unset options

  Options for ':set' and ':unset':

    +m            allow multiline commands
    +r            revert top-level expressions after each evaluation
    +s            print timing/memory stats after each evaluation
    +t            print type after evaluation
    +c            collect type/location info after loading modules
    -<flags>      most GHC command line flags can also be set here
                         (eg. -v2, -XFlexibleInstances, etc.)
                    for GHCi-specific flags, see User's Guide,
                    Flag reference, Interactive-mode options

 -- Commands for displaying information:

   :show bindings              show the current bindings made at the prompt
   :show breaks                show the active breakpoints
   :show context               show the breakpoint context
   :show imports               show the current imports
   :show linker                show current linker state
   :show modules               show the currently loaded modules
   :show packages              show the currently active package flags
   :show paths                 show the currently active search paths
   :show language              show the currently active language flags
   :show targets               show the current set of targets
   :show <setting>             show value of <setting>, which is one of
                                  [args, prog, editor, stop]
   :showi language             show language flags for interactive evaluation


-}


--Exercises
myLastWL xs = xs !! (length xs - 1)
myLastWR xs = head(reverse xs)