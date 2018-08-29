module Main where

import qualified Miso
import Miso (App(..))
import Language.Javascript.JSaddle.Warp as JSaddle

import qualified Common.Model as Common
import qualified Common.View as Common
import qualified Frontend.Update as Frontend

main :: IO ()
main =
  JSaddle.run 8080 $ do
    currentURI <- Miso.getCurrentURI
    Miso.startApp App { initialAction = Common.NoOp
                      , model = Common.initialModel currentURI
                      , update = Miso.fromTransition . Frontend.updateModel
                      , view = Common.viewModel
                      , events = Miso.defaultEvents
                      , subs = [Miso.uriSub Common.HandleURI]
                      , mountPoint = Nothing
                      }
