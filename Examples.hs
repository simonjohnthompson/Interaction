module Examples where 

import Interaction  

readWrite :: Interact () ()

readWrite = readin `sq` writeout id

copy :: Interact () ()

copy = readin `sq` writeout id `sq` copy

test1 :: IO ()

test1 = run readWrite ()

test2 :: IO ()

test2 = runL readWrite ()

readInt :: String -> Int

readInt = read

getInt :: Interact () Int

getInt = readin `sq` apply read

addNum :: Interact Int Int

addNum = readI ((+).readInt) `sq` showkeep

addNums :: Interact Int Int

addNums 
  = while (not.eof)
          addNum     

addNumsToZero :: Interact Int Int

addNumsToZero 
  = while (not.isZero)
          addNum     
    where isZero (_,st) = st==0          

collectNums :: Interact Int Int

collectNums
  = addNum `pass_param` (\n -> start 0 `sq` 
                               seqlist (replicate n addNum) `sq` 
                               write "finished") 

collector :: Interact () (Int,Int)

collector
  =  getInt `sq`                                     -- state is counter
     add_val_right 0 `sq`                            -- now is   (counter,sum)
     while ((>(0::Int)).fst.snd)                     -- still is (counter,sum)
         (add_val_left () `sq`                       -- now  ((),(counter,sum))  
          pass_on getInt `sq`                        -- now (Int,(counter,sum))
          apply (\(p,(m,s))->(m-1,s+p)) `sq`         -- is again (counter,sum)
          wait `sq`                                  -- see what happens when this is commented out :-)
          showkeep)
 
{-
Getting the irrefutable patterns right - messy
-}  

oneWord :: Interact () ()

oneWord = writeln "word"          

manyWords :: Interact () ()

manyWords = oneWord `sq` manyWords     

moreWords :: Interact () ()

moreWords x
      = make_Output ["word"] (moreWords x)

evenmoreWords :: Interact () ()

evenmoreWords x
      = (y,z,["word"]++out)
        where
            (y,z,out) = evenmoreWords x


-- Flavours of echo

necho ys 
  = "Prompt: " ++ [head ys] ++ "\n" ++ necho (tail ys)

echon (x:xs) 
  = "Prompt: " ++ [x] ++ "\n" ++ echon xs