-- Build me with: cabal build TP3.hs
-- Execute me with: cabal run -v0 TP3.hs
-- Load me in the REPL with: cabal repl TP3.hs, then use :r to reload the code upon changing

{- HLINT ignore -}

module Main where

import Debug.Trace
import GHC.Generics
import Generic.Random
import Test.QuickCheck
import Data.Char (isAscii, isSpace)
import Data.List.Split (splitOn)
import Data.Graph (path)

main :: IO ()
main = do
  putStrLn "TP3 is running"
  let url = "https://github.com/dmjio/miso/pulls"
  putStrLn (show (parseUrl url))

-- 1/ Define a type representing URLs you can enter in a browser address bar,
--    i.e. strings of the form:
--    - http://www.google.fr
--    - https://github.com/dmjio/miso/pulls
--    - http://reddit.com/r/haskell
-- 2/ Write a parser from String to your URL type. Its return type must be
--    Either String MyType. The 'Left' case is the error message. Write tests.
-- 3/ We now want to do URL filtering, to grant/forbid access to an URL
--    based on a policy. A filter is a string of the form:
--    - lemonde.fr/emploi
--    - reddit.com/**
--    - reddit.com/*/haskell/**
--    The second filter rules out all URLs of the form reddit.com/suffix, for any 'suffix'
--    The third filter rules out all URLs of the form reddit.com/whatever/haskell/suffix,
--    for any 'whatever' and any 'suffix'. In other words, '*' matches any single
--    segment of the URL while '**' matches all possibles suffixes.
--
--    Write a type for filters
-- 4/ Write a function that taskes an URL and a filter, and returns whether
--    the URL passes the filter. Write tests.
--
-- Use https://hoogle.haskell.org/ to find the functions you need, for example splitOn:
-- https://hackage.haskell.org/package/split-0.2.5/docs/Data-List-Split.html#v:splitOn


data Protocol = Http | Https deriving Show
data Extension = Fr | Com deriving Show
type Segments = [String]


data Url = MkUrl {
  protocol :: Protocol,
  domain :: String,
  extension :: Extension,
  segments :: Segments
} deriving Show


data Err = ParsingErr | BadProtocol | NonAsciiCharInDomain | BadExtension | NonAsciiOrSpaceCharInAccessPath deriving Show


splitUrl :: String -> Either Err (String, String, String, String)
splitUrl input = 
  case splitOn "://" input of
    [protocolPart, rest] -> 
      case splitOn "/" rest of
        [] -> Left ParsingErr
        (domainAndExt:pathParts) ->
          case splitOn "." domainAndExt of
            [domain, ext] -> Right (protocolPart, domain, ext, concat $ map ("/" ++) pathParts)
            _ -> Left ParsingErr
    _ -> Left ParsingErr


checkProtocol :: String -> Either Err Protocol
checkProtocol "http" = Right Http
checkProtocol "https" = Right Https
checkProtocol _ = Left BadProtocol


checkDomain :: String -> Either Err String
checkDomain domain | all isAscii domain = Right domain
                   | otherwise = Left NonAsciiCharInDomain


checkExtension :: String -> Either Err Extension
checkExtension "fr" = Right Fr
checkExtension "com" = Right Com
checkExtension _ = Left BadExtension


checkIndividualSegments :: Segments -> Either Err Segments
checkIndividualSegments [] = Right []
checkIndividualSegments (seg:segments) 
  | all (\c -> isAscii c && not (isSpace c)) seg =
    case checkIndividualSegments segments of
      Right segments -> Right (seg:segments)
      Left err -> Left err
  | otherwise = Left NonAsciiOrSpaceCharInAccessPath


checkSegments :: String -> Either Err Segments
checkSegments "" = Right []
checkSegments "/" = Right []
checkSegments path = do 
  segments <- Right $ filter (not . null) $ splitOn "/" path
  checkIndividualSegments segments


parseUrl :: String -> Either Err Url
parseUrl url = do
  (prot, dom, ext, path) <- splitUrl url
  protocol <- checkProtocol prot
  domain <- checkDomain dom
  extension <- checkExtension ext
  segments <- checkSegments path
  Right (MkUrl protocol domain extension segments)
