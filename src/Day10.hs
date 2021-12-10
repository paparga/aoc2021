module Day10 (day10Main) where

import Conduit
import qualified Data.ByteString as B
import Data.ByteString.Internal (w2c)
import Protolude hiding (sourceFile)
import Protolude.Error (error)

-- Conduits

conduitCharsLine :: Monad m => ConduitT B.ByteString [Char] m ()
conduitCharsLine =
    linesUnboundedAsciiC
        .| mapC (map w2c . B.unpack)

-- Pure

getLineScore :: [Char] -> Int
getLineScore xs = case foldl' checkLine (Right []) xs of
    Left n -> n
    Right _ -> 0

checkLine :: Either Int [Char] -> Char -> Either Int [Char]
checkLine (Left i) curr = Left i
checkLine (Right []) curr = return [curr]
checkLine (Right (lastP : xs)) curr = case (lastP, curr) of
    ('[', ']') -> return xs
    ('(', ')') -> return xs
    ('{', '}') -> return xs
    ('<', '>') -> return xs
    (_, '[') -> return (curr : lastP : xs)
    (_, '(') -> return (curr : lastP : xs)
    (_, '{') -> return (curr : lastP : xs)
    (_, '<') -> return (curr : lastP : xs)
    (_, ']') -> Left 57
    (_, ')') -> Left 3
    (_, '}') -> Left 1197
    (_, '>') -> Left 25137
    _ -> error "Imposible"

-- Debug

debugCond :: Monad m => ConduitT [Char] Char m ()
debugCond = awaitForever yieldMany

-- Main

day10Main :: IO ()
day10Main = do
    score <-
        runConduitRes $
            sourceFile "day10.txt"
                .| conduitCharsLine
                .| mapC getLineScore
                .| foldlC (+) 0

    print score