module BFInterp (
  runBF, runBFWithInput, runBFFileWithInput
)

where

import System.IO
import System.IO.Temp
import System.Process

bfProgramPath = "./bf-interp"

runBF :: String -> IO ()
runBF code = runBFWithInput "" code

runBFWithInput :: String -> String -> IO ()
runBFWithInput input code = 
  withTempFile "." ".tmp-XXXXXX" $ \path h -> do
    hPutStr h code
    hFlush h
    callProcess bfProgramPath [path, input]

runBFFileWithInput :: String -> String -> IO ()
runBFFileWithInput path input = do
  code <- readFile path
  runBFWithInput input code

test1 = runBF code
  where code = (replicate 34 '+') ++ "."

test2 = runBF code
  where code = "+++++ +++++ // set first cell to 10"
                 ++ "  [ // enter multiplication loop"
                 ++ "    > +++++ + // add 6 to the next cell"
                 ++ "      < - // go back and decrement the cell"
                 ++ "      ] // memory layout: (0|60)"
                 ++ "      > +++++ // add 5 to the next cell; memory layout: (0|65)"
                 ++ "      . // print out character"

