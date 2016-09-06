module Main where

import Data.Time
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

main :: IO ()
main = do
  [procname] <- getArgs
  time <- map (\c -> if c == ':' then '_' else c) . show <$> getCurrentTime
  let dir = "logs" </> time
  createDirectoryIfMissing True dir
  putStrLn $ "Logs being written to " ++ dir
  go procname dir 1

go :: String -> FilePath -> Int -> IO ()
go procname dir count = let
  stdoutpath = dir </> "stdout-" ++ show count
  stderrpath = dir </> "stderr-" ++ show count in
  if count > 200 then putStrLn "200 attempts without failure, exiting." else
  withFile stdoutpath WriteMode $ \out ->
    withFile stderrpath WriteMode $ \err -> do
      (_, _, _, ph) <- createProcess $ (proc procname [])
        {std_out = UseHandle out, std_err = UseHandle err}
      exit <- waitForProcess ph
      let continue = do
            putStrLn $ "No failure, attempt " ++ show count
            go procname dir (count + 1)
      case exit of
        ExitSuccess -> continue
        ExitFailure x | x < 0 -> do
            putStrLn $ "Attempt " ++ show count ++ " failed!"
            putStrLn $ "Logs in " ++ stdoutpath ++ " and " ++ stderrpath
                      | otherwise -> continue
