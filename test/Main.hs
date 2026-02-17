module Main where

import Control.Monad (forM_, replicateM, replicateM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, evalStateT, state)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.List (intersperse)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import Data.Word (Word64, Word8)
import GHC.IO.Exception (ExitCode (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (Handle, hClose, hFlush, hPutStrLn, stderr, stdout)
import System.Process
  ( CreateProcess (std_err, std_in, std_out)
  , StdStream (CreatePipe)
  , createProcess
  , proc
  , waitForProcess
  )
import System.Random.SplitMix (SMGen, mkSMGen, nextInteger)

----- core types -----

newtype WordCount = WordCount Integer
  deriving (Eq, Ord)

instance Show WordCount where
  show (WordCount n) = show n

type Rand = StateT SMGen IO

data TestCase = TestCase
  { testName :: String
  , writeInput :: Handle -> Rand WordCount
  }

data RandomSetup = RandomSetup
  { setupName :: String
  , setupCount :: WordCount
  , setupLenGen :: Rand Integer
  }

data Surround = Surround
  { surroundName :: String
  , surroundPrefix :: Bool
  , surroundSuffix :: Bool
  }

----- randomness and alphabets -----

fixedSeed :: Word64
fixedSeed = 0xDEADBEEF12345678

allAsciiBytes :: [Word8]
allAsciiBytes = [0 .. 127]

whitespaceBytes :: [Word8]
whitespaceBytes = [9, 10, 11, 12, 13, 32]

nonWhitespaceBytes :: [Word8]
nonWhitespaceBytes = filter (`notElem` whitespaceBytes) allAsciiBytes

whitespaceVec :: Vector Word8
whitespaceVec = Vector.fromList whitespaceBytes

nonWhitespaceVec :: Vector Word8
nonWhitespaceVec = Vector.fromList nonWhitespaceBytes

randInteger :: (Integer, Integer) -> Rand Integer
randInteger (lo, hi) = state (nextInteger lo hi)

randInt :: (Int, Int) -> Rand Int
randInt (lo, hi) = fromIntegral <$> randInteger (toInteger lo, toInteger hi)

randElem :: Vector a -> Rand a
randElem v = (v !) <$> randInt (0, Vector.length v - 1)

randBuilder :: Int -> Vector Word8 -> Rand BB.Builder
randBuilder size alphabet = mconcat . map BB.word8 <$> replicateM size (randElem alphabet)

----- input generation -----

writeBuilder :: Handle -> BB.Builder -> IO ()
writeBuilder h = LBS.hPutStr h . BB.toLazyByteString

writeRandomBytes :: Handle -> Integer -> Vector Word8 -> Rand ()
writeRandomBytes h total alphabet = do
  bytes <- randBuilder (fromInteger total) alphabet
  liftIO $ writeBuilder h bytes

writeWhitespaces :: Handle -> Int -> Rand ()
writeWhitespaces h maxCount = do
  count <- randInt (1, maxCount)
  spaces <- randBuilder count whitespaceVec
  liftIO $ writeBuilder h spaces

randomWordLen :: Rand Integer
randomWordLen = randInteger (1, 50)

writeRandomBody :: Handle -> WordCount -> Rand Integer -> Rand ()
writeRandomBody h (WordCount n) lenGen
  | n <= 0 = pure ()
  | n <= smallInputLimit = do
      writeWord
      replicateM_ (fromInteger (n - 1)) writeGapAndWord
  | otherwise = do
      let (copies, rest) = divMod n largeChunkWords
      bigChunk <- makeLargeChunk
      replicateM_ (fromInteger copies) $ liftIO $ LBS.hPutStr h bigChunk
      replicateM_ (fromInteger rest) writeGapAndWord
  where
    smallInputLimit = 100000 :: Integer
    largeChunkWords = 10000 :: Integer

    writeWord = do
      len <- lenGen
      writeRandomBytes h len nonWhitespaceVec

    writeGapAndWord = writeWhitespaces h 50 >> writeWord

    makeLargeChunk = do
      lead <- randBuilder 1 whitespaceVec
      bytes <- replicateM (fromInteger largeChunkWords) (randElem nonWhitespaceVec)
      let wordsOnly = mconcat (map BB.word8 (intersperse 32 bytes))
      pure $ BB.toLazyByteString (lead <> wordsOnly)

writeRandomCase :: Handle -> RandomSetup -> Surround -> Rand WordCount
writeRandomCase h (RandomSetup _ count lenGen) (Surround _ hasPrefixWs hasSuffixWs) = do
  when hasPrefixWs $ writeWhitespaces h 10
  writeRandomBody h count lenGen
  when hasSuffixWs $ writeWhitespaces h 10
  pure count

----- output formatting -----

parseStrictCountLine :: BS.ByteString -> Either String WordCount
parseStrictCountLine raw
  | BS.null raw = Left "stdout is empty"
  | otherwise =
      case BS.unsnoc raw of
        Just (digits, 10) ->
          case BS8.readInteger digits of
            Just (n, rest) | BS.null rest && n >= 0 -> Right (WordCount n)
            _ -> Left "stdout must contain only decimal digits followed by newline"
        _ -> Left "stdout must end with a single newline"

color :: String -> String -> String
color code s = "\x1b[" ++ code ++ "m" ++ s ++ "\x1b[0m"

cyan, green, red :: String -> String
cyan = color "36"
green = color "32"
red = color "31"

failCase :: String -> [String] -> IO a
failCase reason details = do
  putStrLn (red "Failed")
  putStrLn (red ("reason: " ++ reason))
  mapM_ (hPutStrLn stderr . red) details
  exitFailure

----- test handler -----

runCase :: FilePath -> TestCase -> IO ()
runCase binaryPath (TestCase name writeInputCase) = do
  putStr (cyan name ++ " ... ")
  hFlush stdout
  runAndAssert
  putStrLn (green "OK")
  where
    runAndAssert = do
      (Just hIn, Just hOut, Just hErr, ph) <-
        createProcess
          (proc binaryPath [])
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }

      expected <- evalStateT (writeInputCase hIn) (mkSMGen fixedSeed)
      hClose hIn
      out <- BS.hGetContents hOut
      err <- BS.hGetContents hErr
      exitCode <- waitForProcess ph

      let failWith reason details =
            failCase reason (details ++ ["stdout: " ++ show out, "stderr: " ++ show err])

      when (exitCode /= ExitSuccess) $
        failWith "process exited with non-zero status" ["exit code: " ++ show exitCode]

      actual <- case parseStrictCountLine out of
        Left parseErr -> failWith ("invalid stdout format: " ++ parseErr) []
        Right parsed -> pure parsed

      unless (actual == expected) $
        failWith "word count mismatch" ["expected: " ++ show expected, "actual: " ++ show actual]

----- test setups -----

surrounds :: [Surround]
surrounds =
  [ Surround "with leading and trailing whitespace" True True
  , Surround "with leading whitespace only" True False
  , Surround "with trailing whitespace only" False True
  , Surround "without surrounding whitespace" False False
  ]

deterministicCases :: [TestCase]
deterministicCases =
  [ det "empty input" 0 BS.empty
  , det "only whitespace" 0 $ BS.pack $ concat $ replicate 16 whitespaceBytes
  , det "one word without whitespace" 1 $ BS8.pack "hello"
  , det "one word surrounded by mixed whitespace" 1 $ BS.pack [32, 9, 10, 97, 11, 12, 13, 32]
  , det "multiple words separated by all whitespace kinds" 7 mixedSeparators
  , det "control bytes are treated as non-whitespace word characters" 1 controlsAsWord
  , det "control bytes become separate words when split by whitespace" 7 controlsSeparated
  , det "word ends exactly at the 8192-byte buffer boundary" 2 boundaryWordEndsAtBuffer
  , det "whitespace falls exactly at the 8192-byte buffer boundary" 2 boundarySeparatorAtBuffer
  , det "one word crosses the 8192-byte buffer boundary" 2 boundaryWordCrossesBuffer
  , det "one huge word" 1 hugeWord
  , det "huge alternating one-letter words and spaces" 20000 hugeAlternatingWords
  ]
  where
    det name count input =
      TestCase ("Deterministic: " ++ name) $ \h -> do
        liftIO $ LBS.hPutStr h (LBS.fromStrict input)
        pure (WordCount count)

    mixedSeparators = BS8.pack "a\tb\nc\vd\fe\rf g"
    controlsAsWord = BS8.pack "A\NULB\SOHC\BSD\SOE\USF\DELG"
    controlsSeparated = BS.pack [0, 32, 1, 9, 2, 10, 3, 11, 4, 12, 5, 13, 6]
    boundaryWordEndsAtBuffer = BS.replicate 8192 65 <> BS.pack [32, 66]
    boundarySeparatorAtBuffer = BS.replicate 8191 65 <> BS.pack [32, 66]
    boundaryWordCrossesBuffer = BS.replicate 8191 65 <> BS.pack [66, 32, 67]
    hugeWord = BS8.replicate 50000 'X'
    hugeAlternatingWords = BS8.intersperse ' ' (BS8.replicate 20000 'Z')

randomSetups :: [RandomSetup]
randomSetups =
  variableLen 0 : fixed ++ medium ++ huge
  where
    fixed = fixedLen 1 <$> [pow2 p + d | p <- [8 .. 16], d <- [-1, 0, 1]]
    medium = variableLen <$> [100, 200 .. 5000]
    huge = variableLen <$> ([pow10 p + 13 | p <- [6, 7, 8]] ++ [pow2 32 + 42])

    pow2 :: Integer -> Integer
    pow2 p = 2 ^ p

    pow10 :: Integer -> Integer
    pow10 p = 10 ^ p

    fixedLen :: Integer -> Integer -> RandomSetup
    fixedLen wordsCount len =
      RandomSetup
        (show wordsCount ++ " word(s), fixed word length " ++ show len)
        (WordCount wordsCount)
        (pure len)

    variableLen :: Integer -> RandomSetup
    variableLen wordsCount =
      RandomSetup
        (show wordsCount ++ " word(s), variable word lengths")
        (WordCount wordsCount)
        randomWordLen

testCases :: [TestCase]
testCases = deterministicCases ++ [mkRandomCase setup s | setup <- randomSetups, s <- surrounds]
  where
    mkRandomCase setup surround =
      TestCase ("Randomized: " ++ setupName setup ++ " [" ++ surroundName surround ++ "]") $ \h ->
        writeRandomCase h setup surround

----- entry point -----

main :: IO ()
main = do
  args <- getArgs
  let binaryPath = case args of
        path : _ -> path
        [] -> error "missing binary path"
  forM_ testCases $ runCase binaryPath
