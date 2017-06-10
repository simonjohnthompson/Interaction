module Experiments where

import InterBasics(join)
import Control.Monad(replicateM)
import System.IO(hSetBuffering,stdout,BufferMode(LineBuffering))


{- getting n lines - still consumes the whole of the input :-( -}

getNlines :: Int -> IO String

getNlines 0 =
    return []

getNlines n = 
    do l <-  getLine
       ls <- getNlines (n-1) 
       return (l++ls) 

{- using getNlines in an interaction -}       

interactN :: Int -> (String -> String) -> IO ()

interactN n f =
    do  input <- replicateM n getLine
        putStr (f (join input))

{- setting buffering before doing an interaction -}

interactL :: (String -> String) -> IO ()

interactL f =
    do hSetBuffering stdout LineBuffering
       interact f

{- "bare" interactions -}

necho :: String -> String

necho ~(x:xs) = "Prompt: " ++ [x] ++ "\n" ++ necho xs
