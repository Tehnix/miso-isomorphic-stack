module Common.Routes where

import Data.Proxy (Proxy(..))
import Miso (View)
import qualified Servant.API as Servant
import Servant.API ((:<|>)(..), (:>))
import qualified Servant.Utils.Links as Servant

import Common.Model

-- Holds a servant route tree of `View action`
type ViewRoutes = Home :<|> Flipped

-- Home route, contains two buttons and a field
type Home = View Action

-- Flipped route, same as Home, but with the buttons flipped
type Flipped = "flipped" :> View Action

-- Servant.URI that points to the home route
homeLink :: Servant.URI
homeLink = Servant.linkURI
  $ Servant.safeLink (Proxy :: Proxy ViewRoutes) (Proxy :: Proxy Home)

-- Servant.URI that points to the flipped route
flippedLink :: Servant.URI
flippedLink = Servant.linkURI
  $ Servant.safeLink (Proxy :: Proxy ViewRoutes) (Proxy :: Proxy Flipped)
