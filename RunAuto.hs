{- 

UWAGA:
Nie do konca jest zrobiona walidacja pliku wejsciowego

-}

import Auto
import System.Environment
import System.Directory

main:: IO()
main = do
  args <- getArgs
  case args of
       [fp] -> printAnswer fp
       _ -> printUsage

printUsage:: IO()
printUsage = do
  pname <- getProgName
  print $ "Usage: " ++ pname ++ " oneFileName"

printAnswer:: String -> IO()
printAnswer fp = do
  contents <- readFile fp
  putStrLn $ validateFile . lines $ contents

validateFile:: [String] -> String
validateFile s
  | length s < 4 = "File is too short, need at least 4 lines"
  | otherwise = checkAnswer s

checkAnswer:: [String] -> String
checkAnswer linesList =
  let s = (getStates (statesCount linesList))
      iS = (parseStringToIntTable (getStartStatesString linesList))
      iA = (parseStringToIntTable (getAcceptStatesString linesList))
      t = makeAllTransitions (extractTableWithTransitions linesList)
  in boolToString (accepts (fromLists s iS iA t) (getWord linesList))

boolToString:: Bool -> String
boolToString True = "True"
boolToString False = "False"

makeAllTransitions:: [String] -> [(Int, Char, [Int])]
makeAllTransitions [] = []
makeAllTransitions (x:xs) = (splitIntoTransitions (makeOneTransition x)) ++ (makeAllTransitions xs)

splitIntoTransitions:: (Int, String, [Int]) -> [(Int, Char, [Int])]
splitIntoTransitions (_, [], _) = []
splitIntoTransitions (q, a:as, x) = [(q, a, x)] ++ splitIntoTransitions (q, as, x)

makeOneTransition:: String -> (Int, String, [Int])
makeOneTransition s =
  let q = readInt (head (words s))
      a = getElem 1 (words s)
      x = map readInt (drop 2 (words s))
  in (q, a, x)

extractTableWithTransitions:: [String] -> [String]
--remove first 3 lines and then remove last line
extractTableWithTransitions linesList = init (drop 3 linesList)

statesCount:: [String] -> Int
statesCount linesList = readInt (head linesList)

getStates:: Int -> [Int]
getStates i = [1..i]

getStartStatesString:: [String] -> String
--in second line is given list of start states
getStartStatesString linesList = getElem 1 linesList

getAcceptStatesString:: [String] -> String
--in third line is given list of accepting states
getAcceptStatesString linesList = getElem 2 linesList

getWord:: [String] -> String
-- in last line is given
getWord linesList = last linesList

parseStringToIntTable:: String -> [Int]
parseStringToIntTable s = map readInt (map (removeChar ']') (map (removeChar '[') (split ',' s)))

--helper functions 

getElem:: Int -> [a] -> a
getElem _ [] = error "empty list"
getElem y (x:xs)
  | y <= 0 = x
  | otherwise = getElem (y-1) xs

split:: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s

removeChar:: Char -> String -> String
removeChar _ [] = []
removeChar ch (c:cs)
    | c == ch   = removeChar ch cs
    | otherwise = c:(removeChar ch cs)

readInt:: String -> Int
readInt = read