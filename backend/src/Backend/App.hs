module Backend.App
  ( app
  )
where

import Data.Proxy
import qualified Lucid as L
import qualified Lucid.Base as L
import qualified Miso
import Miso (View)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Servant
import Servant ((:<|>)(..), (:>))

import qualified Common.Model as Common
import qualified Common.Routes as Common
import qualified Common.View as Common

app :: Wai.Application
app = Servant.serve
  (Proxy :: Proxy ServerAPI)
  (static :<|> serverHandlers :<|> Servant.Tagged page404)
 where
  static :: Servant.Server StaticAPI
  static = Servant.serveDirectoryFileServer "static"
  serverHandlers :: Servant.Server ServerRoutes
  serverHandlers = homeServer :<|> flippedServer
-- Alternative type:
-- Servant.Server (ToServerRoutes Common.Home HtmlPage Common.Action)
-- Handles the route for the home page, rendering Common.homeView.
  homeServer :: Servant.Handler (HtmlPage (View Common.Action))
  homeServer =
    pure $ HtmlPage $ Common.viewModel $ Common.initialModel Common.homeLink
-- Alternative type:
-- Servant.Server (ToServerRoutes Common.Flipped HtmlPage Common.Action)
-- Renders the /flipped page.
  flippedServer :: Servant.Handler (HtmlPage (View Common.Action))
  flippedServer =
    pure $ HtmlPage $ Common.viewModel $ Common.initialModel Common.flippedLink
-- The 404 page is a Wai application because the endpoint is Raw.
-- It just renders the page404View and sends it to the client.
  page404 :: Wai.Application
  page404 _ respond =
    respond
      $ Wai.responseLBS HTTP.status404 [("Content-Type", "text/html")]
      $ L.renderBS
      $ L.toHtml Common.page404View

-- | Represents the top level Html code. Its value represents the body of the
-- page.
newtype HtmlPage a =
  HtmlPage a
  deriving (Show, Eq)

instance L.ToHtml a => L.ToHtml (HtmlPage a) where
  toHtmlRaw = L.toHtml
  toHtml (HtmlPage x) =
    L.doctypehtml_ $ do
      L.head_ $ do
        L.title_ "Miso isomorphic example"
        L.meta_ [L.charset_ "utf-8"]
        L.with
          (L.script_ mempty)
          [ L.makeAttribute "src" "/static/all.js"
          , L.makeAttribute "async" mempty
          , L.makeAttribute "defer" mempty
          ]
      L.body_ (L.toHtml x)

-- Converts the ClientRoutes (which are a servant tree of routes leading to
-- some `View action`) to lead to `Get '[Html] (HtmlPage (View Common.Action))`
type ServerRoutes = Miso.ToServerRoutes Common.ViewRoutes HtmlPage Common.Action

-- The server serves static files besides the ServerRoutes, among which is the
-- javascript file of the client.
type ServerAPI
   = StaticAPI :<|> (ServerRoutes :<|> Servant.Raw -- This will show the 404 page for any unknown route
                     )

type StaticAPI = "static" :> Servant.Raw
