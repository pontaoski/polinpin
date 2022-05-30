module UI exposing (Palette, blackLine, box, currentPasswordInput, darkTextButton, emailInput, grayBox, intScale, line, link, multilineInput, newPasswordInput, scale, searchInput, sizedLabel, smallTextButton, spellCheckedInput, textButton, textInput, transtext, usernameInput, withScrim)

import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (node)
import Html.Attributes exposing (attribute, style)


color2string : Color -> String
color2string color =
    let
        decons =
            toRgb color

        decstr =
            { red = String.fromFloat (decons.red * 255)
            , green = String.fromFloat (decons.green * 255)
            , blue = String.fromFloat (decons.blue * 255)
            , alpha = String.fromFloat decons.alpha
            }

        str =
            "rgba(" ++ decstr.red ++ ", " ++ decstr.green ++ ", " ++ decstr.blue ++ ", " ++ decstr.alpha ++ ")"
    in
    str


type alias Palette =
    { background : Color
    , stroke : Color
    , hoverBackground : Maybe Color
    , hoverStroke : Maybe Color
    , downBackground : Maybe Color
    , downStroke : Maybe Color
    }


maybeToList : Maybe a -> List a
maybeToList =
    Maybe.map List.singleton >> Maybe.withDefault []


paletteToAttribute : Palette -> List (Html.Attribute msg)
paletteToAttribute pal =
    let
        c =
            [ attribute "color" (color2string pal.background) ]

        sc =
            [ attribute "stroke-color" (color2string pal.stroke) ]

        hc =
            Maybe.map (attribute "hover-color" << color2string) pal.hoverBackground
                |> maybeToList

        hsc =
            Maybe.map (attribute "hover-stroke-color" << color2string) pal.hoverStroke
                |> maybeToList

        dc =
            Maybe.map (attribute "down-color" << color2string) pal.downBackground
                |> maybeToList

        dsc =
            Maybe.map (attribute "down-stroke-color" << color2string) pal.downStroke
                |> maybeToList
    in
    c ++ sc ++ hc ++ hsc ++ dc ++ dsc


box palette attrs child =
    let
        htmlEl =
            node "rough-rectangle"
                (paletteToAttribute palette)
                []
    in
    el (behindContent (html htmlEl) :: attrs) child


line palette attrs =
    let
        htmlEl =
            node "rough-horizontal-line"
                (paletteToAttribute palette)
                []
    in
    el ([ behindContent (html htmlEl), height (px 1) ] ++ attrs) none


grayBox =
    box (Palette (rgb255 225 221 210) (rgb255 50 50 50) Nothing Nothing Nothing Nothing)


blackLine =
    line (Palette (rgb255 0 0 0) (rgb255 50 50 50) Nothing Nothing Nothing Nothing)


darkGrayPalette =
    (Palette (rgb255 205 199 183)
            (rgb255 50 50 50)
            (Just (rgb255 0xE0 0xDA 0xC9))
            (Just (rgb255 50 50 50))
            (Just (rgb255 0xAE 0xA8 0x99))
            (Just (rgb255 50 50 50))
        )

darkerGrayPalette =
    (Palette (rgb255 0x8D 0x86 0x72)
        (rgb255 50 50 50)
        (Just (rgb255 0x95 0x8e 0x79))
        (Just (rgb255 50 50 50))
        (Just (rgb255 0x56 0x50 0x3D))
        (Just (rgb255 50 50 50))
    )



whiteBox =
    Palette (rgb255 0xFF 0xFF 0xFE)
        (rgb255 50 50 50)
        Nothing
        Nothing
        Nothing
        Nothing


withScrim : Element msg -> Element msg
withScrim child =
    el [ width fill, height fill, Background.color <| rgba255 0x00 0x00 0x00 0.4 ] child


transtext txt =
    el [ htmlAttribute (style "pointer-events" "none") ] (text txt)


abstractButton palette msg attrs child =
    let
        htmlEl =
            node "rough-rectangle"
                (paletteToAttribute palette)
                []
    in
    Input.button (behindContent (html htmlEl) :: attrs) { onPress = msg, label = el [ centerX ] child}

textButton : Maybe msg -> List (Attribute msg) -> String -> Element msg
textButton msg attrs txt =
    abstractButton darkGrayPalette msg ( padding 10 :: attrs) (text txt)

darkTextButton : Maybe msg -> List (Attribute msg) -> String -> Element msg
darkTextButton msg attrs txt =
    abstractButton darkerGrayPalette msg ( padding 10 :: attrs) (text txt)


smallTextButton : Maybe msg -> List (Attribute msg) -> String -> Element msg
smallTextButton msg attrs =
    textButton msg ([ padding 6, Font.size <| intScale -1 ] ++ attrs)


xInput : (List (Attribute msg) -> a) -> List (Attribute msg) -> a
xInput fn attrs =
    let
        htmlEl =
            node "rough-rectangle"
                (paletteToAttribute whiteBox)
                []
    in
    fn
        (behindContent (html htmlEl) :: attrs)


textInput :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
    -> Element msg
textInput =
    xInput Input.text


multilineInput :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        , spellcheck : Bool
        }
    -> Element msg
multilineInput =
    xInput Input.multiline


usernameInput :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
    -> Element msg
usernameInput =
    xInput Input.username


newPasswordInput :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        , show : Bool
        }
    -> Element msg
newPasswordInput =
    xInput Input.newPassword


currentPasswordInput :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        , show : Bool
        }
    -> Element msg
currentPasswordInput =
    xInput Input.currentPassword


emailInput :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
    -> Element msg
emailInput =
    xInput Input.email


searchInput :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
    -> Element msg
searchInput =
    xInput Input.search


spellCheckedInput :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
    -> Element msg
spellCheckedInput =
    xInput Input.spellChecked


link : List (Attribute msg) -> { url : String, label : Element msg } -> Element msg
link attrs =
    Element.link ([ Font.color (rgb255 0x32 0x7F 0xA2), Font.underline ] ++ attrs)


scale : Int -> Float
scale =
    Element.modular 20 1.2


intScale : Int -> Int
intScale =
    scale >> round


sizedLabel : Int -> List (Attribute msg) -> String -> Element msg
sizedLabel scaled attrs txt =
    el (Font.size (scale scaled |> round) :: attrs) (text txt)
