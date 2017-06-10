module Interaction where 

import InterBasics
import Prelude hiding (null)
import System.IO(hSetBuffering,stdout,BufferMode(LineBuffering))


{-                                    
    Interact.m                            
                                    
    A model of Interactions based on the type Interact.        
                                    
    Version 1.0        18 September 1989            
                                    
    Uses     inter_basics.m                        
                                    
    Simon Thompson, Computing Laboratory, Univ. of Kent, U.K.    
    sjt@ukc.ac.uk
-}

{- 
    For details of types such as Input, Output etc., see InterBasics.m                            
 -}

type Interact a b = (Input,a) -> (Input,b,Output)

{-                                    
    A general Interaction will consume some Input, and produce    
    some Output. The remainder of the Input is returned too.    
                                    
    What about a b? These represent information about the state    
    of the machine before and after the Interaction has taken    
    place. In a previous version                    
    the state-info type was taken to be the same before and     
    after the Interaction - this results in some inelegancies:    
    Often an Interaction can be used to ``pick up'' information    
    of a particular type - if this is to be modelled by an        
    Interaction of the old kind, a dummy value has to be passed    
    inwards. We replace this with the unique value, (), of the one-    
    element type ().
-}

write :: String -> Interact a a

write outstring (input,st) 
      = (input,st,[outstring])

writeln :: String -> Interact a a

writeln outstring (input,st) 
      = (input,st,[outstring++"\n"])

{-
    write and writeln push a string onto the Output stream,        
    without modifying Input or state                
-}

readin :: Interact () String

readin (input,())
      = (tail input, head input,[])

writeout :: (a -> String) -> Interact a ()

writeout f (input,st)
    = (input,(),[f st ++ "\n"])   
    
writekeep :: (a -> String) -> Interact a a

writekeep f (input,st)
    = (input,st,[f st ++ "\n"])     

{- 
defaults for writeout and writekeep using show         
{show behaves "badly" on String, as it adds quotes
-}

showout :: Show a => Interact a ()

showout = writeout show

showkeep :: Show a => Interact a a

showkeep = writekeep show



{-
    readin extracts the first string from the Input stream,        
    and returns it using the state place in its Output.        
-}

readI :: (String -> a -> b) -> Interact a b

readI f (line:rest,st) = (rest,f line st,[])

{-                                    
    make_Output is the function which puts its first        
    argument, a piece of Output, at the front of the Output        
    stream.                                
                
    It is used in the rept function so that the piece of        
    Output will appear on the Output stream BEFORE the Condition    
            ~ cond next                    
    is evaluated                            
-}                                    


make_Output :: Output -> (Input,a,Output) -> (Input,a,Output)

make_Output piece ~(input,st,out) = (input,st,piece++out)



{-                                    
    Some traditional control structures:                
                                    
    A Condition is seen as a boolean function taking TWO         
    arguments, the current Input stream together with        
    the state. Hence the following type definition:            
-}                                    


type Condition a = (Input,a) -> Bool

{- 
    alt is an alternative command, which preforms the first of the    
    second of its Interaction arguments depending upon the        
    (boolean) value of the Condition.                
 -}

alt :: Condition a -> Interact a b -> Interact a b -> Interact a b

alt cond inter1 inter2 x
    | cond x     = inter1 x
    | otherwise  = inter2 x 

 {-
    The following sequencing functions perform a number of inter-    
    actions in sequence. sq takes two, seq3 and seq4 take three     
    four respectively. seqlist will combine a list, but note that    
    as lists are homogeneous, all the Interactions in the list need    
    to be of the same type, i.e. have the same state type         
    on initiation and termination.                    
    Pascal's infix operator `;' is given by the infix $sq.        
 -}

sq :: Interact a b -> Interact b c -> Interact a c

sq inter1 inter2 x
      = make_Output out1 (inter2 (rest,st))
    where ~(rest,st,out1) = inter1 x

sqNew :: Interact a b -> Interact b c -> Interact a c

sqNew inter1 inter2 x
      = (make_Output out1 (inter2 (rest,st)))
    where (rest,st,out1) = inter1 x
          (rest2,st2,out2) = inter2 (rest,st)

seq3 :: Interact a b -> Interact b c -> Interact c d -> Interact a d

seq3 inter1 inter2 inter3
      = sq inter1
        (sq inter2 inter3)

seq4 :: Interact a b -> Interact b c
    -> Interact c d -> Interact d e
    -> Interact a e

seq4 inter1 inter2 inter3 inter4
      = sq inter1
        (seq3 inter2 inter3 inter4)

seqlist :: [Interact a a] -> Interact a a

seqlist = foldr sq null

 {-
    while and repeat take an Interaction and iterate it in the     
    usual way:                            
    with while, the Condition is first evaluated, and if true the    
    Interaction is performed, and the loop re-invoked. For repeat    
    the Interaction is performed, and only if the Condition fails    
    is the loop re-entered.                        
                                    
    Note how higher-order definitions can be given: given alt and    
    sq we simply need to use recursion to give the iterators.    
 -}

while ::  Condition a -> Interact a a -> Interact a a

while cond inter 
      = whi
    where
    whi = alt cond (inter `sq` whi) null

rept  ::  Condition a -> Interact a a -> Interact a a

rept cond inter
      = inter `sq` (while (not.cond) inter)


{-                                    
    We can build a state by the parallel application of         
    a number of Interactions.                    
    We have parallelism in the sense that the second (and        
    subsequent) Interactions                    
    are performed in the initial state.                
-}                                    


par :: Interact a b -> Interact a c -> Interact a (b,c)

par inter1 inter2 (input,st)
      = (rest2,(st1,st2),out1++out2)
    where
    (rest1,st1,out1) = inter1 (input,st)
    (rest2,st2,out2) = inter2 (rest1,st)

par3 :: Interact a b -> Interact a c ->
    Interact a d -> Interact a (b,c,d)

par3 inter1 inter2 inter3 (input,st)
      = (rest3,(st1,st2,st3),out1++out2++out3)
    where
    (rest1,st1,out1) = inter1 (input,st)
    (rest2,st2,out2) = inter2 (rest1,st)
    (rest3,st3,out3) = inter3 (rest2,st)


{-                                    
    Function application as an Interaction                
-}                                    


apply :: (a -> b) -> Interact a b

apply f (input,st) = (input, f st , [])


{-                                    
    The null Interaction is not simply the identity transformation,    
    since we must register the fact that it produces no Output;    
    that is done by the next definition.                
-}                                    


null :: Interact a a

null (input,st) = (input,st,[])


{-                                    
    Almost the same as the null Interaction, except that we        
    forget the state information.                    
-}                                    


forget :: Interact a ()

forget (input,st)
      = (input,(),[])


{-                                    
    The ``inverse'' of the forget operation: gives the state a    
    starting value.                            
-}                                    


start :: a -> Interact () a

start v (input,()) = (input,v,[])


{-                                    
    changing :- putting forgetting and starting together:        
-}                                    


change :: b -> Interact a b

change n = sq forget (start n)


{-                                    
    This Interaction checks its Input state, so that it forces    
    evaluation of what precedes it. In the situation of        
        seq3 write read (write "Thanks")            
    we want the second write                    
    to act only AFTER the read has been performed - this can be    
    achieved by prefacing it by a wait.                
    Thanks are due to Steve Hill for the example above        
     which stimulated this definition.                
-}                                    


wait :: Eq a => Interact a a
 
wait (input,x)
    | x==x   =  (input,x,[])


{-                                    
    adding values to the state                    
-}                                    

add_val_right :: a -> Interact b (b,a)

add_val_right v = add_val_r v null

add_val_r :: a -> Interact b c -> Interact b (c,a)

add_val_r v inter 
      = (add_r v) . inter
    where
    add_r v (a,b,c) = (a,(b,v),c)

add_val_left :: a -> Interact b (a,b)

add_val_left v = add_val_l v null

add_val_l :: a -> Interact b c -> Interact b (a,c)

add_val_l v inter 
      = (add_l v) . inter
    where
    add_l v (a,b,c) = (a,(v,b),c)


{-                                    
    functions which will pass state information around        
-}                                    
 

pass_on :: Interact a b -> Interact (a,c) (b,c)
pass_on_l :: Interact a b -> Interact (c,a) (c,b)
pass_on_r :: Interact a b -> Interact (a,c) (b,c)


pass_on = pass_on_r

pass_on_l inter (input,(st3,st1))
      = (rest,(st3,st2),out)
    where
    (rest,st2,out) = inter (input,st1)

pass_on_r inter (input,(st1,st3))
      = (rest,(st2,st3),out)
    where
    (rest,st2,out) = inter (input,st1)



{-                                    
    The first Interaction gathers a value, st1, of type b.     
    We then    perform the Interaction delivered by applying the    
    second argument, a function, to st1.                
-}                                    


pass_param :: Interact a b ->
          ( b -> Interact () d ) ->
          Interact a d

pass_param int f (input,st)
      = (rest,final,out1++out)
    where
    ~(inter1,st1,out1) = int (input,st)
    ~(rest,final,out) = (f st1) (inter1,())

{- variant in which the parameter also feeds into the interaction -}

pass_param_keep :: Interact a b ->
          ( b -> Interact b d ) ->
          Interact a d

pass_param_keep int f (input,st)
      = (rest,final,out1++out)
    where
    ~(inter1,st1,out1) = int (input,st)
    ~(rest,final,out) = (f st1) (inter1,st1)


{-                                    
    In order to get these Interactions to run on the Miranda    
    system, we have to evaluate an expression which gives        
    rise to the stream of Output required.                 
                                    
    run will run an Interaction from a starting state to         
    termination, if that happens, printing the final state        
    if termination occurs.                         
    run accepts Input from stdin and must be supplied with a    
    `show' function for the final state. This is its third        
    parameter.                            
-}                                    

{- This is the Miranda-style version

run :: Interact a b -> a -> (b -> RawOutput) -> RawOutput

run inter st g
      = join out ++ g final
    where
    (_,final,out) = inter (split (read stdin),st)
-}

run :: Interact a b -> a -> IO ()

run inter st 
      = interact (\chs -> 
          case inter (split chs,st) of
            (_,_,out) -> join out ++ "\n")

runShow :: Show b => Interact a b -> a -> IO ()

runShow inter st 
      = interact (\chs -> 
          case inter (split chs,st) of
            (_,final,out) -> join out ++ show final ++ "\n")            

runL :: Interact a b -> a -> IO ()

runL inter st 
      = do hSetBuffering stdout LineBuffering
           run inter st

runShowL :: Show b => Interact a b -> a -> IO ()

runShowL inter st 
      = do hSetBuffering stdout LineBuffering
           runShow inter st

 {-}
    dumprun causes the final state to be dumped in the file        
    which is passed as the third parameter.                
-} 

{-     ====== TO FIX ==============
dumprun :: Interact a b -> a -> [char] -> (b -> RawOutput) -> [sys_message]

dumprun inter st fil g
      = [ stdout (join out) , tofile fil (g final) ]
    where
    (rest,final,out) = inter (split (read stdin),st)
-}

{- 
    SOME EXAMPLES                            
 -}


{-                                    
    Interactions to try to get positive integers or integers.    
    The user is prompted once. If a correct Input is produced,    
    it is returned, together with a flag value True. If not        
    the flag is set to False, (and a dummy value of 0 is returned)    
-}                                    


getposint :: Interact () (Int,Bool)

getposint 
      = seq3 (write "Please enter a positive integer: ")
         readin
         (alt (numeric_String . snd)
            (sq (apply string_posint)
             (add_val_right True))
            (seq3 (write "Not a positive integer, try again.\n")
              forget
              (start (0,False))))

getint :: Interact () (Int,Bool)

getint 
      = seq3 (write "Please enter an integer: ")
         readin
         (alt (integer_String.snd)
            (sq (apply string_int)
             (add_val_right True))
            (seq3 (write "Not an integer, try again.\n")
              forget
              (start (0,False))))


{-                                    
    newgetint is a new version of getint, which is parametrised    
    on its prompt, error message and checking function. It is    
    defined much as getstring, except that it caters for two     
    kinds of error                            
        not typing (a string representing an) integer        
    and                                
        not typing an integer in the appropriate set        
                                    
    newgetposint acts similarly                    
-}                                    


newgetint :: String -> String -> (Int -> Bool) -> Interact () (Int,Bool)

newgetint prompt err_mess checkfun (input,())
      = make_Output [prompt] aux
    where
    aux = (rest,outst,out)
    (outst,out) 
       | not (integer_String a)     = ((0,False),["Not integer string; try again\n"])
       | not (checkfun num_a)        = ((0,False),[err_mess++"\n"])
       | otherwise                  = ((num_a ,True),[]) 
    a = head input
    rest = tail input
    num_a = string_int a

newgetposint :: String -> String -> (Int -> Bool) -> Interact () (Int,Bool)

newgetposint prompt err_mess checkfun (input,())
      = make_Output [prompt] aux
    where
    aux = (rest,outst,out)
    (outst,out) 
       | not (numeric_String a)     = ((0,False),["Not positive integer string; try again\n"])
       | not (checkfun num_a)       = ((0,False),[err_mess++"\n"])
       | otherwise                  = ((num_a ,True),[])
    a = head input
    rest = tail input
    num_a = string_posint a


{-                                    
    getstring tries once to get a string. It prompts with its    
    first string argument, and Outputs an error message if the    
    string doesn't meet the requirement of checkfun. It returns    
    the value of checkfun on the Input, together with the Input    
    itself.                                
-}                                    


getstring :: String -> String -> (String -> Bool) -> Interact () (String,Bool)

getstring prompt error_mess checkfun (x,())
      = make_Output [prompt] aux
    where
    aux = (y,(a,ok),out)
    ok  = checkfun a
    out 
      | ok            = []
      | otherwise     = [ error_mess++"\n" ]
    a   = head x            -- Delayed pattern matching
    y   = tail x


{-                                    
    Often want to try repeatedly to get an integer - iterate     
    a trial until it is successful - this can be performed using    
    control structures given in the file control.m            
                                    
    Inputposint,Inputint apply the functions getposint and getint    
    repeatedly until a valid Input is found.            
                                    
    in_posint in_int do the same, but also perform a validity    
    check on their Input accordint to the boolean function        
            checkfun                    
    the prompt and the error message corresponding to checkfun    
    are also passed as parameters.                    
-}                                    


inputposint :: Interact () Int

inputposint
      = seq3 (start (0,False)) 
         (rept is_ok fetchposint) 
         (apply num_part)
    where
    is_ok (_,(_,b))     = b
    fetchposint         = sq forget getposint
    num_part (a,_)      = a

{- end of file -}

eof :: Condition a

eof (input,_) = input==[]