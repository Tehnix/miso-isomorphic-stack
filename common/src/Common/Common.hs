{-# LANGUAGE CPP #-}
module Common.Common where

import Control.Lens
import Data.Proxy ( Proxy(..) )
import qualified Servant.API as Servant
import Servant.API ( (:<|>)(..), (:>) )
#if MIN_VERSION_servant(0,10,0)
import qualified Servant.Utils.Links as Servant
#endif
import qualified Miso
import Miso ( View )
import Miso.Html
import qualified Miso.String as Miso
-- import qualified Network.URI as Network


data Model = Model
  { _uri          :: !Servant.URI
  -- ^ current URI of the application
  , _counterValue :: !Int
  -- ^ the counter value
  }
  deriving (Eq, Show)

-- | Set up the initial model/state.
initialModel :: Servant.URI -> Model
initialModel uri =
  Model
  { _uri = uri
  , _counterValue = 0
  }

data Action
  = NoOp
  | AddOne
  | SubtractOne
  | ChangeURI !Servant.URI
  | HandleURI !Servant.URI
  deriving (Show, Eq)

-- Holds a servant route tree of `View action`
type ViewRoutes = Home :<|> Flipped
-- Home route, contains two buttons and a field
type Home = View Action
-- Flipped route, same as Home, but with the buttons flipped
type Flipped = "flipped" :> View Action

makeLenses ''Model

-- Checks which URI is open and shows the appropriate view
viewModel :: Model -> View Action
viewModel model = view
  where
    view =
        either (const page404View) id
          $ Miso.runRoute (Proxy :: Proxy ViewRoutes) handlers _uri model
    handlers = homeView :<|> flippedView

-- View function of the Home route
homeView :: Model -> View Action
homeView m =
  div_ []
    [ div_
      []
      [ button_ [ onClick SubtractOne ] [ text "-" ]
      , text $ Miso.ms $ show $ _counterValue m
      , button_ [ onClick AddOne ] [ text "+" ]
      ]
    , button_ [ onClick $ ChangeURI flippedLink ] [ text "Go to /flipped" ]
    ]

-- View function of the Home route
flippedView :: Model -> View Action
flippedView m =
  div_ []
    [ div_
      []
      [ button_ [ onClick AddOne ] [ text "+" ]
      , text $ Miso.ms $ show $ _counterValue m
      , button_ [ onClick SubtractOne ] [ text "-" ]
      ]
    , button_ [ onClick $ ChangeURI homeLink ] [ text "Go to /" ]
    ]

page404View :: View Action
page404View =
  text "Yo, 404, page unknown. Go to / or /flipped. Shoo!"

-- Servant.URI that points to the home route
homeLink :: Servant.URI
homeLink =
#if MIN_VERSION_servant(0,10,0)
  Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Home)
#else
  Servant.safeLink (Proxy @ViewRoutes) (Proxy @Home)
#endif

-- Servant.URI that points to the flipped route
flippedLink :: Servant.URI
flippedLink =
#if MIN_VERSION_servant(0,10,0)
  Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Flipped)
#else
  Servant.safeLink (Proxy @ViewRoutes) (Proxy @Flipped)
#endif
