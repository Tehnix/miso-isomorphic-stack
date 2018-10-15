module Main where

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Wai
import qualified Network.Wai.Middleware.Gzip as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified System.IO as IO

import Backend.App (app)

main :: IO ()
main = do
  IO.hPutStrLn IO.stderr "Running on port http://127.0.0.1:3003 ..."
  Wai.run 3003 $ Wai.logStdout $ compress app
 where
  compress :: Wai.Middleware
  compress = Wai.gzip Wai.def { Wai.gzipFiles = Wai.GzipCompress }
