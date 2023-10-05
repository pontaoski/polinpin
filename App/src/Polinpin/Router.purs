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
import Polinpin.SharedUI as SharedUI
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Select (selectAll)

data Query a
    = Navigate Route a

data Action
    = Initialize
    | Receive (Connected Store.Store Unit)

type State =
    { route :: Maybe Route
    , store :: Store.Store
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
    connect selectAll $ H.mkComponent
        { initialState: \{ context } -> { route: Nothing, store: context }
        , render
        , eval: H.mkEval $ H.defaultEval
            { handleQuery = handleQuery
            , handleAction = handleAction
            , initialize = Just Initialize
            , receive = Just <<< Receive
            }
        }
    where

    handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
    handleAction = case _ of
        Initialize -> do
            initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
            navigate $ fromMaybe Home initialRoute
        Receive { context } ->
            H.modify_ _ { store = context }

    handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
    handleQuery = case _ of
        Navigate dest a -> do
            { route } <- H.get
            when (route /= Just dest) do
                H.modify_ _ { route = Just dest }
            pure (Just a)

    render :: State -> H.ComponentHTML Action ChildSlots m
    render { store, route } =
        case route of
            Just Home ->
                SharedUI.toHTML $
                    SharedUI.sharedFrame store
                        { body: (text "owo")
                        , over: Nothing
                        , title: "Homepage"
                        }
            Just _ ->
                layout [] (text "not implemented")
            Nothing ->
                layout [] (text "not found")
        

        -- DemoPage.main
        -- layoutWith { options: [] } [] $
        --     row [] [text "test", text "test", column [] [text "column", text "column"]]
        -- HH.div_ [ HH.text "Oh no! That page wasn't found." ]
