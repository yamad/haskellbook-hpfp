module Main where

import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import System.Environment (getArgs)
import System.IO (hGetContents, hPutStrLn, hWaitForInput, stderr, stdin, stdout)

import Cipher

main :: IO ()
main = do
  cipher <- getArgs >>= parseArgs
  didNotTimeout <- hWaitForInput stdin (5*1000)
  if didNotTimeout
    then cipher <$> hGetContents stdin >>= hPutStrLn stdout
    else hPutStrLn stderr "Timeout (5 seconds)" >> die

parseArgs :: [String] -> IO (String -> String)
parseArgs ["-h"] = usage >> exit
parseArgs [key, "-e"] = return $ vigenere key   -- encrypt
parseArgs [key, "-d"] = return $ unVigenere key -- decrypt
parseArgs _ = usage >> die

usage :: IO ()
usage = do
  putStrLn "Usage: vigenere KEY [-e|-d]"
  putStrLn "  (e)ncrypt/(d)ecrypt input from stdin using vigenere cipher with KEY"

exit :: IO a
exit = exitWith ExitSuccess

die :: IO a
die = exitWith (ExitFailure 1)
