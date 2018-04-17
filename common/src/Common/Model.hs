{-# LANGUAGE TemplateHaskell #-}
module Common.Model where

import Control.Lens
import qualified Servant.API as Servant

data Model = Model
  { _uri :: !Servant.URI
  -- ^ Current URI of the application.
  , _counterValue :: !Int
  -- ^ The counter value.
  } deriving (Eq, Show)

makeLenses ''Model

-- | Set up the initial model/state.
initialModel :: Servant.URI -> Model
initialModel initialUri = Model {_uri = initialUri, _counterValue = 0}

data Action
  = NoOp -- ^ Empty/No operation.
  | ChangeURI !Servant.URI -- ^ Push a new URI on the History stack.
  | HandleURI !Servant.URI -- ^ Handle a URI change (e.g. popstate events).
  | AddOne
  | SubtractOne
  deriving (Show, Eq)
