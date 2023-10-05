module Polinpin.SharedUI where

import Prelude
import Polinpin.Store as Polinpin.Store
import Element
import Element.Font as Font
import Polinpin.UI as UI
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Halogen.HTML as HH
import Polinpin.Interfaces as Interfaces
import Polinpin.Interfaces (Route(..), AuthenticatedRoute(..), UnauthenticatedRoute(..))

type View p i =
    { title :: String
    , body :: Element p i
    , over :: Maybe (Element p i)
    }

toHTML :: forall p i. View p i -> HH.HTML p i
toHTML view =
    layout
        (Font.family
            [ Font.typeface "Fuzzy Bubbles"
            ]
            `Array.cons` (case view.over of
                    Nothing ->
                        []

                    Just elm ->
                        [ inFront (UI.withScrim elm) ]
               )
        )
        view.body

sharedFrame :: forall p i. Polinpin.Store.Store -> View p i -> View p i
sharedFrame store view =
    { title: view.title
    , body:
        column [ spacing 10, width fill, height fill ]
            [ header store view.title, view.body ]
    , over: view.over
    }

shadedRow :: forall p i. Array (Attribute p i) -> Array (Element p i) -> Element p i
shadedRow attrs =
    row $
        [ padding 10, behindContent $ UI.grayBox [ width fill, height fill ] none ] <> attrs

header :: forall p i. Polinpin.Store.Store -> String -> Element p i
header store title =
    row [ padding 10, width fill ]
        [ shadedRow [ alignLeft ]
            [ UI.link [] { url: "/", label: text "Polinpin" }
            ]
        , el [ centerX ] (text title)
        , shadedRow [ alignRight, spacing 20 ]
            case store.user of
                Just _ ->
                    [ UI.link [] { url: Interfaces.printRoute (Authenticated MyStudies), label: text "My Studies" }
                    ]
                Nothing ->
                    [ UI.link [] { url: Interfaces.printRoute (Unauthenticated Login), label: text "Login" }
                    , UI.link [] { url: Interfaces.printRoute (Unauthenticated Register), label: text "Register" }
                    ]
        ]
