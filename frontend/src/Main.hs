module Main where

import Control.Lens ((+=), (-=), (.=), (^.), makeLenses)
import Data.Proxy (Proxy(..))
import qualified Miso
import Miso (App(..), View)
import qualified Miso.String as Miso
import qualified Servant.API as Servant
import Servant.API ((:<|>)(..))
import qualified Servant.Utils.Links as Servant

import qualified Common.Model as Common
import qualified Common.View as Common

main :: IO ()
main =
  Miso.miso $ \currentURI ->
    App
    { initialAction = Common.NoOp
    , model = Common.initialModel currentURI
    , update = Miso.fromTransition . updateModel
    , view = Common.viewModel
    , events = Miso.defaultEvents
    , subs = [Miso.uriSub Common.HandleURI]
    , mountPoint = Nothing
    }

updateModel :: Common.Action -> Miso.Transition Common.Action Common.Model ()
updateModel action =
  case action of
    Common.NoOp -> pure ()
    -- Handle History/Routing.
    Common.ChangeURI uri ->
      Miso.scheduleIO $ do
        Miso.pushURI uri
        pure Common.NoOp
    Common.HandleURI uri -> Common.uri .= uri
    -- Custom actions from our inc/decrementor.
    Common.AddOne -> Common.counterValue += 1
    Common.SubtractOne -> Common.counterValue -= 1
