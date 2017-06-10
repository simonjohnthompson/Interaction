module InterBasics where

import Data.Char

type Input = [String]
type Output = [String]
type RawInput = [Char]
type RawOutput = [Char]


{-    
    Contains various auxilliary functions and definitions for    
    Input/output                            
                                    
    Version 1.0        18 September 1989            
    Version 2.0        25 May 2017
                                    
    Simon Thompson, Computing Lab., Univ. of Kent.            
    sjt@ukc.ac.uk                            
-}

{-
    The fundamental type definitions                
                                    
    Note that we have chosen to consider Input and Output as    
    consisting of streams of Strings rather than streams of        
    Char. Conversion to and from the ``raw'' versions is trivial:    
    RawInput is split at newlines, and RawOutput is the result    
    of concatenating the String forming the output stream. (Note    
    that this means that we are explicit about the placing of    
    newlines etc.                            
-}

newline,space,tab :: Char

newline = '\n'
space   = ' '
tab    = '\t'

stdin :: [Char]
stdin = "/dev/tty"    

{-
    Some sensible names. Note that stdin names standard Input    
    as a file.                            
-}

split     :: RawInput -> Input
gen_split :: Char -> RawInput -> Input


{-                                    
    split splits RawInput into lines. Defined in terms of the    
    more general gen_split which splits Input at occurrences of     
    its first argument, a Character.                
                                    
    Following these definitions we have two more general ones,     
    developed in the course of writing a text processing        
    system. They are more general in that they allow         
        1. splitting on more than one Character            
        2. multiple occurrences of the splitting Characters    
        to be treated in a similar way to single occurrences.    
-}

split = gen_split newline    

gen_split ch l
      = aux_split ch [] l
    where
    aux_split ch sofar (a:rest)
       | a == ch        = rever sofar : gen_split ch rest 
       | otherwise      = aux_split ch (a:sofar) rest      
    aux_split ch sofar []
                        = [ rever sofar ]

{-
    Splitting lists into lists of lists according to membership    
    of a `split_set'                        
    Lists can be split into sublists in two slightly different    
    ways, depending on how we treat repeated occurrences of        
    members of the split_set. We can either treat a repetition    
    as delimiting an empty list, as we do in `cut', or we         
    can treat repetitions as single instances, which we do in    
    `simple_cut'.                            
    Both flavours have their uses.                    
-}

cut :: Eq a => [a] -> [a] -> [[a]]

cut splitSet [] = []
cut splitSet (a:x) 
      = cut_aux [] (a:x)
    where
    cut_aux l [] 
       | l /= []    = [l]  
       | otherwise    = []   
    cut_aux l (a:x) 
       | not (elem a splitSet) = cut_aux (l++[a]) x  
       | otherwise             = l : (cut_aux [] x)
              
              
simple_cut :: Eq a => [a] -> [a] -> [[a]]

simple_cut splitSet [] = []
simple_cut splitSet (a:x) 
      = cut_aux [] (a:x)
    where
    cut_aux l []    
       | l/=[]       = [l]
       | otherwise   = []
    cut_aux l (a:x) 
       | not (elem a splitSet)   = cut_aux (l++[a]) x 
       | l /= []                 = l : (cut_aux [] x) 
       | otherwise               = cut_aux [] x 
              


{-                                    
    An auxilliary function : an efficient reversing function    
    based on shunting                        
-}                                    

rever :: [a] -> [a]
rever l = shunt l []

shunt :: [a] -> [a] -> [a]
shunt []    m = m    
shunt (a:x) m = shunt x (a:m)    


join      :: Output -> RawOutput

{-
    join joins lines, and is an alias for concat.            
-}

join = concat                    -- from the standard envt.

 
{-                                    
    Dealing with basic types, of numbers and Characters.        
-}                                    
 

numeric      :: Char -> Bool            -- To test for particular
alpha        :: Char -> Bool            -- kinds of Character.
alphanumeric :: Char -> Bool

numeric = Data.Char.isDigit                            -- from the standard envt.
alpha   = Data.Char.isLetter                        -- ditto.
alphanumeric ch = alpha ch || numeric ch 


{-                                    
    Testing for Strings consisting of particular kinds of        
    Character                            
    Note that the empty String is classed as being in the         
    respective classes.                        
-}                                    


numeric_String      :: String -> Bool
alpha_String        :: String -> Bool
alphanumeric_String :: String -> Bool
integer_String      :: String -> Bool

numeric_String      = foldr (&&) True . map numeric
alpha_String        = foldr (&&) True . map alpha
alphanumeric_String = foldr (&&) True . map alphanumeric
integer_String x    = numeric_String x || ( head x == '-' && numeric_String (tail x) )


{-                                    
    Converting Strings to numbers and vice versa.            
    The empty numeric String is translated as zero.            
-}                                    


string_posint :: String -> Int
string_int    :: String -> Int

string_posint
      = conv_aux . rever
    where
    conv_aux (a:x) 
       | numeric a    = (ord a - ord '0') + 10 * conv_aux x
       | otherwise    = error "String_posint found non-numeric Character"
    conv_aux []    = 0

string_int (a:x)
      | numeric a       = string_posint (a:x)
      | a == '-'        = - (string_posint x)
      | otherwise       = error "unexpected first Character to String_int"
string_int [] = 0

num_String :: Integer -> String

num_String = show           --  A standard function

