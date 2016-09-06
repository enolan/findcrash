module Main(main) where

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

runLogged :: String -> [String] -> Path b Dir -> IO ExitCode
runLogged cmd params dir = do
  createDirIfMissing False dir
  stdOutRel <- parseRelFile "stdout"
  stdErrRel <- parseRelFile "stderr"
  let stdoutpath = dir </> stdOutRel
      stderrpath = dir </> stdErrRel
  withFile (toFilePath stdoutpath) WriteMode $ \out ->
      withFile (toFilePath stderrpath) WriteMode $ \err -> do
        (_, _, _, ph) <- createProcess $ (proc cmd params)
          {std_out = UseHandle out, std_err = UseHandle err}
        waitForProcess ph

go :: String -> Path b Dir -> Int -> IO ()
go procname dir count =
  if count > 200
  then putStrLn "200 attempts without failure, exiting."
  else do
    dirEx <- parseRelDir $ show count
    exit <- runLogged procname [] (dir </> dirEx)
    let continue = do
              putStrLn $ "No failure, attempt " ++ show count
              go procname dir (count + 1)
    case exit of
      ExitSuccess -> continue
      ExitFailure x | x < 0 ->
                        putStrLn $ "Attempt " ++ show count ++ " failed!"
                    | otherwise -> continue
