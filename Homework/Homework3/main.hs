module Main where
import System.IO
import Hanoi_check

kappa1::[(String,String)]
kappa1= [("a","b"),("a","c") ,("a","d"),("c","d"),("b","d")]

kappa2::[(String,String)]
kappa2= [("a","b"),("a","c") ,("a","d"),("b","d"),("c","d")]

kappa3::[(String,String)]
kappa3= [("a","b"),("a","c") ,("d","b"),("b","d"),("c","d")]

main:: IO ()
main = do  y <- getLine
           let z= Prelude.read y :: Int
           if z == 4 then do print (check kappa1 (0, [[1..z],[],[],[]])); 
                             print(check (hanoi 4 "a" "b" "c" "d") (0, [[1..z],[],[],[]]))
                     else if z== 10 then do print (check kappa2 (0, [[1..z],[],[],[]])); 
                                            print (check (hanoi z "a" "b" "c" "d") (0, [[1..z], [],[],[]]))
                                    else do print (check kappa3 (0, [[1..z],[],[],[]])); 
                                            print (check (hanoi z "a" "b" "c" "d") (0, [[1..z], [],[],[]]))