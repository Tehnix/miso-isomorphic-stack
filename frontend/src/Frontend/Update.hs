module Frontend.Update where

import Control.Lens ((+=), (-=), (.=))
import qualified Miso

import qualified Common.Model as Common

updateModel :: Common.Action -> Miso.Transition Common.Action Common.Model ()
updateModel action = case action of
  Common.NoOp          -> pure ()
  -- Handle History/Routing.
  Common.ChangeURI uri -> Miso.scheduleIO $ do
    Miso.pushURI uri
    pure Common.NoOp
  Common.HandleURI uri -> Common.uri .= uri
  -- Custom actions from our inc/decrementor.
  Common.AddOne        -> Common.counterValue += 1
  Common.SubtractOne   -> Common.counterValue -= 1
