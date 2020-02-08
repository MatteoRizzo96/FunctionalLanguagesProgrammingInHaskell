import System.IO
import System.Directory
import System.FilePath
import Parser
import CoreLanguage
import PrettyPrinter

-- Read the input from file
readF :: IO String
readF = do currDir <- getCurrentDirectory
           inh <- openFile (joinPath [currDir, "..", "tests", "test6.txt"]) ReadMode
           prog <- readloop inh
           hClose inh
           return prog
           where readloop inh = do ineof <- hIsEOF inh
                                   if ineof then return [] else do x  <- hGetLine inh
                                                                   xs <- readloop inh
                                                                   return (x ++ " " ++ xs)

-- Check the output of the parsing process 
comp :: [(Program Name, Name)] -> String
comp [] = error "no parse"
comp [(e, [])] = pprint e
comp [(x, a)] = error ("does not use all input \n" ++ show x ++ "\n ------- \n" ++ a)

-- Read, parse and pretty print the input file
main :: IO ()
main = do inp <- readF
          let out = (comp (parse parseProg inp))
          putStrLn out
