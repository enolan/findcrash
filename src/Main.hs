module Main(main) where

import Data.Word
import Path
import Path.IO
import System.Exit
import System.IO
import System.Process
import System.Random
import System.Random.Shuffle

main :: IO ()
main = do
  let cmd = ".stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/regression-and-feature-tests/regression-and-feature-tests"
  opts <- shuffleM $ replicate 50 False ++ replicate 50 True
  let logs = $(mkRelDir "logs")
      parDir = logs </> $(mkRelDir "par")
      serDir = logs </> $(mkRelDir "ser")
  mapM_ (createDirIfMissing True) [parDir, serDir]
  mapM_ (\ser -> do
            dir <- mkRandDir (if ser then serDir else parDir)
            let params = "--node" : if ser then ["+RTS", "-N1"] else []
            runLogged cmd params dir) opts

mkRandDir :: Path b Dir -> IO (Path b Dir)
mkRandDir base = do
  randomW :: Word64 <- randomIO
  randomDir <- parseRelDir $ show randomW
  let randomDir' = base </> randomDir
  createDir randomDir'
  pure randomDir'

runLogged :: String -> [String] -> Path b Dir -> IO ExitCode
runLogged cmd params dir = do
  createDirIfMissing False dir
  stdOutRel <- parseRelFile "stdout"
  stdErrRel <- parseRelFile "stderr"
  let stdoutpath = dir </> stdOutRel
      stderrpath = dir </> stdErrRel
  writeFile (toFilePath $ dir </> $(mkRelFile "cmd")) $ unwords $ cmd : params
  withFile (toFilePath stdoutpath) WriteMode $ \out ->
      withFile (toFilePath stderrpath) WriteMode $ \err -> do
        (_, _, _, ph) <- createProcess $ (proc cmd params)
          {std_out = UseHandle out, std_err = UseHandle err}
        waitForProcess ph
