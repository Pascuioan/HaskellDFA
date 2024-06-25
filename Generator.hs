import System.IO
import System.Directory.Internal.Prelude (getArgs)
import Data.ByteString (isInfixOf)
import Data.Text (pack, count)
import Control.Exception (Exception, throw)
import GHC.Base (thenIO)

newtype MyException = MyException String deriving (Show)
instance Exception MyException
-- generic datatype to encapsulate each component of a DFA
data Component = States [String] | Alphabet [String] | Transitions [(String, String, String)] deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  if Prelude.null args then do
    print "Usage: ./Generator.exe <dfa_config_file>"
    return ()
  else do
    fileH <- openFile "dfa.txt" ReadMode
    content <- hGetContents fileH
    hClose fileH
    let valid = validate content
    if valid then do
      (c1, c2, c3) <- parse content
      
      return ()
    else do
      print ""
      return ()

-- functions to validate the input
validate :: String -> Bool
validate s = "states" `appearsOnce` s && "alphabet" `appearsOnce` s && "transitions" `appearsOnce` s && appearances "#" s == 2

appearsOnce :: String -> String -> Bool
appearsOnce sub str = appearances sub str == 1

appearances :: String -> String -> Int
appearances sub str = sum $ map (toInt . isPrefixOf sub) $ tails str
  where toInt True = 1
        toInt False = 0

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (subH : subT) (strH : strT)
  |subH == strH = isPrefixOf subT strT
  |otherwise = False

tails :: String -> [String]
tails "" = [""]
tails (h:t) = (h:t) : tails t

--functions to parse the input

parse :: String -> IO (Component, Component, Component)
parse s = do
  let sections = split '#' s
  if length sections /= 3 then do
    throw $ MyException "Incorrect file format"
  else do
    let splitSections = map (filter (/= "") . split '\n') sections
    -- build a list of functions ([String] -> Component) based on each section
    let functions = map matchFunction splitSections
    -- apply the corresponding function to each section (after removing the head)
    return $ tuplify3 $ zipWith ($) functions (map (\(h:t) -> t) splitSections)

parseStates :: [String] -> Component
parseStates = States

parseAlphabet :: [String] -> Component
parseAlphabet = Alphabet

parseTransitions :: [String] -> Component
parseTransitions ls = Transitions $ parseTransitionsInner ls
  -- take each row with shape "state1 symbol state2" and turn it into ("state1", "symbol", "state2")
  -- and then pack them all int a Component
  where parseTransitionsInner (h:t) = tuplify3 (split ' ' h) : parseTransitionsInner t 
        parseTransitionsInner [] = []

-- return a parsing function based on the section
matchFunction :: [String] -> ([String] -> Component)
matchFunction (h:t)
  |h == "states" = parseStates
  |h == "alphabet" = parseAlphabet
  |h == "transitions" = parseTransitions
  |otherwise = throw $ MyException "A section must follow #"

tuplify3 :: [a] -> (a, a, a)
tuplify3 [a,b,c] = (a,b,c)
tuplify3 _ = throw $ MyException "Incorrect transition formatting"

split :: Char -> String -> [String]
split c = innerSplit c ""
  where innerSplit c "" [] = []
        innerSplit c acc [] = [acc]
        innerSplit c acc (h:t)
          |h == c = acc : innerSplit c "" t
          |otherwise = innerSplit c (acc ++ [h]) t

-- funtion to print a component
writeComponent :: Component -> Handle -> IO ()
writeComponent = undefined