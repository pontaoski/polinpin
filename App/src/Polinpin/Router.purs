module Polinpin.Router where

import Prelude

import Halogen as H
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (hush)
import Halogen.Store.Monad (class MonadStore)
import Polinpin.Store as Store
import Effect.Aff.Class (class MonadAff)
import Routing.Duplex as RD
import Halogen (liftEffect)
import Routing.Hash (getHash)
import Polinpin.Interfaces (Route(..), navigate, routeCodec, class Navigation)
import Halogen.HTML as HH
import Element
import Polinpin.DemoPage as DemoPage

data Query a
    = Navigate Route a

data Action
    = Initialize
    | Receive Unit

type State =
    { route :: Maybe Route
    }

type ChildSlots :: forall k. Row k
type ChildSlots =
    (
    )

component
    :: forall m
     . MonadAff m
    => Navigation m
    => MonadStore Store.Action Store.Store m
    => H.Component Query Unit Void m
component =
    H.mkComponent
        { initialState: \_ -> { route: Nothing }
        , render
        , eval: H.mkEval $ H.defaultEval
            { handleQuery = handleQuery
            , handleAction = handleAction
            , initialize = Just Initialize
            }
        }
    where

    handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
    handleAction = case _ of
        Initialize -> do
            initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
            navigate $ fromMaybe Home initialRoute
        Receive _ ->
            pure unit

    handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
    handleQuery = case _ of
        Navigate dest a -> do
            { route } <- H.get
            when (route /= Just dest) do
                H.modify_ _ { route = Just dest }
            pure (Just a)

    render :: State -> H.ComponentHTML Action ChildSlots m
    render { route: _route } =
        DemoPage.main
        -- layoutWith { options: [] } [] $
        --     row [] [text "test", text "test", column [] [text "column", text "column"]]
        -- HH.div_ [ HH.text "Oh no! That page wasn't found." ]
