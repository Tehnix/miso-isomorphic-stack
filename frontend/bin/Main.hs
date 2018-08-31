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
    -- FIXME: Once https://github.com/Tehnix/miso-isomorphic-stack/issues/6 is fixed, switch back to Miso.startApp.
    -- currentURI <- Miso.getCurrentURI
    Miso.miso $ \currentURI -> App
      { initialAction = Common.NoOp
      , model = Common.initialModel currentURI
      , update = Miso.fromTransition . Frontend.updateModel
      , view = Common.viewModel
      , events = Miso.defaultEvents
      , subs = [Miso.uriSub Common.HandleURI]
      , mountPoint = Nothing
      }
