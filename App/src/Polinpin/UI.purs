module Polinpin.UI where

import Prelude
import Element as Element
import Element.Font as Font
import Data.Number.Format as Number.Format
import Data.Maybe (Maybe(..))
import Halogen.HTML.Properties as HP
import Halogen.HTML as HH
import Web.HTML.Common (PropName(..))
import Data.Array as Array
import Element.Background as Background

color2string :: Element.Color -> String
color2string color =
    let
        decons =
            Element.toRgb color

        decstr =
            { red: Number.Format.toString (decons.red * 255.0)
            , green: Number.Format.toString (decons.green * 255.0)
            , blue: Number.Format.toString (decons.blue * 255.0)
            , alpha: Number.Format.toString decons.alpha
            }

        str =
            "rgba(" <> decstr.red <> ", " <> decstr.green <> ", " <> decstr.blue <> ", " <> decstr.alpha <> ")"
    in
    str

type Palette =
    { background :: Element.Color
    , stroke :: Element.Color
    , hoverBackground :: Maybe Element.Color
    , hoverStroke :: Maybe Element.Color
    , downBackground :: Maybe Element.Color
    , downStroke :: Maybe Element.Color
    }

palette' background stroke hoverBackground hoverStroke downBackground downStroke =
    { background
    , stroke
    , hoverBackground
    , hoverStroke
    , downBackground
    , downStroke
    }

maybeToArray :: forall a. Maybe a -> Array a
maybeToArray x =
    case x of
        Just y ->
            [y]
        Nothing ->
            []


paletteToAttribute :: forall r i. Palette -> Array (HH.IProp r i)
paletteToAttribute pal =
    let
        c =
            [ HP.prop (PropName "color") (color2string pal.background) ]

        sc =
            [ HP.prop (PropName "stroke-color") (color2string pal.stroke) ]

        hc =
            map (HP.prop (PropName "hover-color") <<< color2string) pal.hoverBackground
                # maybeToArray

        hsc =
            map (HP.prop (PropName "hover-stroke-color") <<< color2string) pal.hoverStroke
                # maybeToArray

        dc =
            map (HP.prop (PropName "down-color") <<< color2string) pal.downBackground
                # maybeToArray

        dsc =
            map (HP.prop (PropName "down-stroke-color") <<< color2string) pal.downStroke
                # maybeToArray
    in
    c <> sc <> hc <> hsc <> dc <> dsc

box :: forall p i. Palette -> Array (Element.Attribute p i) -> Element.Element p i -> Element.Element p i
box palette attrs child =
    let
        htmlEl =
            HH.element (HH.ElemName "rough-rectangle")
                (paletteToAttribute palette)
                []
    in
    Element.el (Element.behindContent (Element.html htmlEl) `Array.cons` attrs) child

solidBox :: forall p i. Element.Color -> Array (Element.Attribute p i) -> Element.Element p i -> Element.Element p i
solidBox color attrs child =
    let
        htmlEl =
            HH.element (HH.ElemName "rough-rectangle")
                [ HH.prop (PropName "color") (color2string color) ]
                []
    in
    Element.el ([ Element.behindContent (Element.html htmlEl), Background.color color ] <> attrs) child

line :: forall p i. Palette -> Array (Element.Attribute p i) -> Element.Element p i
line palette attrs =
    let
        htmlEl =
            HH.element (HH.ElemName "rough-horizontal-line")
                (paletteToAttribute palette)
                []
    in
    Element.el ([ Element.behindContent (Element.html htmlEl), Element.height (Element.px 1) ] <> attrs) Element.none

grayBox :: forall p i. Array (Element.Attribute p i) -> Element.Element p i -> Element.Element p i
grayBox =
    box (palette' (Element.rgb255 225 221 210) (Element.rgb255 50 50 50) Nothing Nothing Nothing Nothing)


blackLine :: forall p i. Array (Element.Attribute p i) -> Element.Element p i
blackLine =
    line (palette' (Element.rgb255 0 0 0) (Element.rgb255 50 50 50) Nothing Nothing Nothing Nothing)

solidRoughBox :: forall p i. Element.Color -> Array (Element.Attribute p i) -> Element.Element p i -> Element.Element p i
solidRoughBox color attrs child =
    let
        htmlEl =
            HH.element (HH.ElemName "rougher-rectangle")
                [ HH.prop (PropName "color") (color2string color) ]
                []
    in
    Element.el (Element.behindContent (Element.html htmlEl) `Array.cons` attrs) child

link :: forall p i. Array (Element.Attribute p i) -> { url :: String, label :: Element.Element p i } -> Element.Element p i
link attrs =
    Element.link $
        [ Font.color (Element.rgb255 0x32 0x7F 0xA2)
        , Font.underline
        ] <> attrs

withScrim :: forall p i. Element.Element p i -> Element.Element p i
withScrim child =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color $ Element.rgba255 0x00 0x00 0x00 0.4
        ]
        child
