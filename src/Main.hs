module Main where

import Data.Time
import Path
import Path.IO
import System.Environment
import System.Exit
import System.IO
import System.Process

main :: IO ()
main = do
  [procname] <- getArgs
  time <- map (\c -> if c == ':' then '_' else c) . show <$> getCurrentTime
  timeDir <- parseRelDir time
  let dir = $(mkRelDir "logs") </> timeDir
  createDirIfMissing True dir
  putStrLn $ "Logs being written to " ++ show dir
  go procname dir 1

go :: String -> Path b Dir -> Int -> IO ()
go procname dir count = do
  stdOutRel <- parseRelFile $ "stdout-" ++ show count
  stdErrRel <- parseRelFile $ "stderr-" ++ show count
  let stdoutpath = dir </> stdOutRel
      stderrpath = dir </> stdErrRel
  if count > 200 then putStrLn "200 attempts without failure, exiting." else
    withFile (toFilePath stdoutpath) WriteMode $ \out ->
      withFile (toFilePath stderrpath) WriteMode $ \err -> do
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
            putStrLn $ "Logs in " ++ show stdoutpath ++ " and " ++ show stderrpath
                      | otherwise -> continue
