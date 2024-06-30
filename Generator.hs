import System.IO
import System.Directory.Internal.Prelude (getArgs)
import Data.Char (toUpper)
import Control.Exception (Exception, throw)

newtype MyException = MyException String deriving (Show)
instance Exception MyException
-- generic datatype to encapsulate each component of a DFA
data Component = States String [String] [String] | Alphabet [String] | Transitions [(String, String, String)] deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  if Prelude.null args then do
    print "Usage: ./Generator.exe <dfa_config_file>"
    return ()
  else do
    fileH <- openFile (head args) ReadMode
    content <- hGetContents fileH
    let valid = validate content
    if valid then do
      (c1, c2, c3) <- parse content
      -- print c1
      -- print c2
      -- print c3
      outH <- openFile "out.hs" WriteMode
      hPutStr outH "import System.Directory.Internal.Prelude (getArgs)\n\n"
      writeComponent outH c1
      writeComponent outH c2
      writeComponent outH c3
      hPutStr outH mainBlock
      hClose outH
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
parseStates ls = States startState finalStates otherStates
  where startState = filter (/= '!') $ head rawStates
        finalStates = map (filter (/= '!')) $ filter isFinal rawStates
        otherStates = filter (not . isFinal) rawStates
        rawStates = map (map toUpper) ls

isFinal :: String -> Bool
isFinal s = '!' == last s

parseAlphabet :: [String] -> Component
parseAlphabet = Alphabet

parseTransitions :: [String] -> Component
parseTransitions ls = Transitions $ parseTransitionsInner ls
  -- take each row with shape "state1 symbol state2" and turn it into ("state1", "symbol", "state2")
  -- and then pack them all int a Component
  where parseTransitionsInner (h:t) = tuplify3 [map toUpper state1, symbol, map toUpper state2] : parseTransitionsInner t
          where [state1, symbol, state2] = split ' ' h
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
writeComponent :: Handle -> Component -> IO ()
writeComponent fileH (States startState finalStates otherStates) = do
  let states = reverse . drop 3 . reverse $ foldr (\a b -> a ++ " | " ++ b) "" (finalStates ++ otherStates)
  hPutStr fileH $ "data State = " ++ states ++ "\n\n" -- write the State datatype
  hPutStr fileH "isFinal :: State -> Bool\n" -- write the isFinal function
  let finalEvals = foldr ((\a b -> a ++ "\n" ++ b) . (\s -> "isFinal " ++ s ++ " = True")) "" finalStates
  hPutStr fileH finalEvals
  hPutStr fileH "isFinal _ = False\n\n"
  hPutStr fileH $ "start :: State = " ++ startState ++ "\n\n" -- write the start state

writeComponent fileH (Transitions ls) = do
  hPutStr fileH "step :: State -> Char -> State\n"
  let transitions = foldr ((\a b -> a ++ "\n" ++ b) . (\(s1, c, s2) -> concat ["step ", s1, " \'", c,"\' = ", s2])) "" ls
  hPutStr fileH $ transitions ++ "\n"

writeComponent _ _ = do return ()

mainBlock :: String
mainBlock = "steps :: State -> String -> State\n" ++
       "steps state \"\" = state\n" ++
       "steps state (h:t) = steps (step state h) t\n" ++
       "\n" ++
       "eval :: String -> IO Bool\n" ++
       "eval s = do\n" ++
       "  return $ isFinal $ steps start s\n" ++
       "\n" ++
       "main :: IO ()\n" ++
       "main = do\n" ++
       "  args <- getArgs\n" ++
       "  if null args then\n" ++
       "    print \"Error: no arguments received\"\n" ++
       "  else do\n" ++
       "    accepted <- eval $ head args\n" ++
       "    if accepted then\n" ++
       "      print \"Accepted\"\n" ++
       "    else\n" ++
       "      print \"Failure\""