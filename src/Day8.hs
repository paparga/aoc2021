module Day8 (
    day8Main,
) where

import Conduit
import qualified Data.ByteString as B
import Data.ByteString.Internal (w2c)
import Data.Char (digitToInt)
import Protolude hiding (sourceFile)
import Protolude.Error (error)

type ThreeRows =
    ( Maybe [Int] -- Prev
    , [Int] -- Curr
    , Maybe [Int] -- Next
    )

-- Conduits

conduitThreeRows :: Monad m => Maybe ThreeRows -> ConduitT [Int] ThreeRows m ()
conduitThreeRows prevData = case prevData of
    Nothing -> initialPhase
    Just (_, last, Just curr) -> normalPhase last curr
    _ -> error "Imposible"
  where
    initialPhase = do
        mCurr <- await
        case mCurr of
            Nothing -> return () -- Empty file
            Just curr -> do
                next <- await
                yield (Nothing, curr, next)
                case next of
                    Nothing -> return () -- Special case 1 line
                    Just _ -> conduitThreeRows $ Just (Nothing, curr, next) -- Continues
    normalPhase last curr = do
        next <- await
        yield (Just last, curr, next)
        case next of
            Nothing -> return () -- Last line
            Just _ -> conduitThreeRows $ Just (Just last, curr, next) -- Middle line

conduitDigits :: Monad m => ConduitT B.ByteString [Int] m ()
conduitDigits =
    linesUnboundedAsciiC
        .| mapC B.unpack
        .| mapC (map (digitToInt . w2c))

-- Pure

getLineScore :: ThreeRows -> Int
getLineScore (last, curr, next) = foldr foldFn 0 indexed
  where
    indexed = zip [0 ..] curr
    isLowerM i x ys = (x <) <$> atMay ys i
    isLowerCurr i x = fromMaybe True $ isLowerM i x curr
    lastRow i x = fromMaybe True $ last >>= isLowerM i x
    nextRow i x = fromMaybe True $ next >>= isLowerM i x
    foldFn (i, x) prev =
        if lastRow i x && nextRow i x && isLowerCurr (i - 1) x && isLowerCurr (i + 1) x
            then prev + x + 1
            else prev

-- Main

day8Main :: IO ()
day8Main = do
    score <-
        runConduitRes $
            sourceFile "day8.txt"
                .| conduitDigits
                .| conduitThreeRows Nothing
                .| mapC getLineScore
                .| foldlC (+) 0

    print score
