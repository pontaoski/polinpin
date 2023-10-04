module Element.Internal.Style where

import Prelude

import Data.Array (cons)
import Data.Array as Array
import Data.Tuple (Tuple(..))
import Halogen.HTML (ClassName(..))
import Data.String as String
import Data.Int (toStringAs, decimal)
import Data.Newtype (class Newtype, unwrap)

newtype Selector = Selector String

derive instance selectorNewtype :: Newtype Selector _
derive newtype instance selectorSemigroup :: Semigroup Selector

appendClassname :: ClassName -> ClassName -> ClassName
appendClassname a b = ClassName (unwrap a <> unwrap b)

data Class
    = Class Selector (Array Rule)

data Rule
    = Prop String String
    | Child Selector (Array Rule)
    | AllChildren Selector (Array Rule)
    | Supports (Tuple String String) (Array (Tuple String String))
    | Descriptor Selector (Array Rule)
    | Adjacent String (Array Rule)
    | Batch (Array Rule)

overrides :: String
overrides =
    """@media screen and (-ms-high-contrast: active), (-ms-high-contrast: none) {"""
        <> unwrap (dot classes.any)
        <> unwrap (dot classes.row)
        <> " > "
        <> unwrap (dot classes.any)
        <> " { flex-basis: auto !important; } "
        <> unwrap (dot classes.any)
        <> unwrap (dot classes.row)
        <> " > "
        <> unwrap (dot classes.any)
        <> unwrap (dot classes.container)
        <> " { flex-basis: auto !important; }}"
        <> inputTextReset
        <> sliderReset
        <> trackReset
        <> thumbReset
        <> explainer

inputTextReset :: String
inputTextReset =
    """
input[type="search"],
input[type="search"]::-webkit-search-decoration,
input[type="search"]::-webkit-search-cancel-button,
input[type="search"]::-webkit-search-results-button,
input[type="search"]::-webkit-search-results-decoration {
  -webkit-appearance:none;
}
"""

sliderReset :: String
sliderReset =
    """
input[type=range] {
  -webkit-appearance: none; 
  background: transparent;
  position:absolute;
  left:0;
  top:0;
  z-index:10;
  width: 100%;
  outline: dashed 1px;
  height: 100%;
  opacity: 0;
}
"""

trackReset :: String
trackReset =
    """
input[type=range]::-moz-range-track {
    background: transparent;
    cursor: pointer;
}
input[type=range]::-ms-track {
    background: transparent;
    cursor: pointer;
}
input[type=range]::-webkit-slider-runnable-track {
    background: transparent;
    cursor: pointer;
}
"""

thumbReset :: String
thumbReset =
    """
input[type=range]::-webkit-slider-thumb {
    -webkit-appearance: none;
    opacity: 0.5;
    width: 80px;
    height: 80px;
    background-color: black;
    border:none;
    border-radius: 5px;
}
input[type=range]::-moz-range-thumb {
    opacity: 0.5;
    width: 80px;
    height: 80px;
    background-color: black;
    border:none;
    border-radius: 5px;
}
input[type=range]::-ms-thumb {
    opacity: 0.5;
    width: 80px;
    height: 80px;
    background-color: black;
    border:none;
    border-radius: 5px;
}
input[type=range][orient=vertical]{
    writing-mode: bt-lr; /* IE */
    -webkit-appearance: slider-vertical;  /* WebKit */
}
"""


explainer :: String
explainer =
    """
.explain {
    border: 6px solid rgb(174, 121, 15) !important;
}
.explain > .""" <> unwrap classes.any <> """ {
    border: 4px dashed rgb(0, 151, 167) !important;
}

.ctr {
    border: none !important;
}
.explain > .ctr > .""" <> unwrap classes.any <> """ {
    border: 4px dashed rgb(0, 151, 167) !important;
}

"""

data Intermediate
    = Intermediate
        { selector :: Selector
        , props :: Array (Tuple String String)
        , closing :: String
        , others :: Array Intermediate
        }

rules :: String
rules =
    overrides
        <> renderCompact (baseSheet <> commonValues)

commonValues :: Array Class
commonValues =
    Array.concat
        [ map
            (\x ->
                Class (Selector (".border-" <> toStringAs decimal x)) [ Prop "border-width" (toStringAs decimal x <> "px") ]
            )
            (Array.range 0 6)
        , map
            (\i ->
                Class (Selector (".font-size-" <> toStringAs decimal i)) [ Prop "font-size" (toStringAs decimal i <> "px") ]
            )
            (Array.range 8 32)
        , map
            (\i ->
                Class (Selector (".p-" <> toStringAs decimal i)) [ Prop "padding" (toStringAs decimal i <> "px") ]
            )
            (Array.range 0 24)

        -- Common Font Variants
        , [ Class (Selector ".v-smcp")
                [ Prop "font-variant" "small-caps"
                ]
          , Class (Selector ".v-smcp-off")
                [ Prop "font-variant" "normal"
                ]
          ]

        -- , fontVariant "smcp"
        , fontVariant "zero"
        , fontVariant "onum"
        , fontVariant "liga"
        , fontVariant "dlig"
        , fontVariant "ordn"
        , fontVariant "tnum"
        , fontVariant "afrc"
        , fontVariant "frac"
        ]

fontVariant :: String -> Array Class
fontVariant var =
    [ Class (Selector (".v-" <> var))
        [ Prop "font-feature-settings" ("\"" <> var <> "\"")
        ]
    , Class (Selector (".v-" <> var <> "-off"))
        [ Prop "font-feature-settings" ("\"" <> var <> "\" 0")
        ]
    ]

emptyIntermediate :: Selector -> String -> Intermediate
emptyIntermediate selector closing =
    Intermediate
        { selector: selector
        , props: []
        , closing: closing
        , others: []
        }

renderRules :: Intermediate -> Array Rule -> Intermediate
renderRules (Intermediate parent) rulesToRender =
    let
        generateIntermediates rule rendered =
            case rule of
                Prop name val ->
                    rendered { props = (Tuple name val) `cons` rendered.props }

                Supports (Tuple prop value) props ->
                    rendered {
                        others =
                            Intermediate
                                { selector: Selector $ "@supports (" <> prop <> ":" <> value <> ") {" <> unwrap parent.selector
                                , props: props
                                , closing: "\n}"
                                , others: []
                                }
                                `cons` rendered.others
                    }

                Adjacent selector adjRules ->
                    rendered {
                        others =
                            renderRules
                                (emptyIntermediate (Selector (unwrap parent.selector <> " + " <> selector)) "")
                                adjRules
                                `cons` rendered.others
                    }

                Child child childRules ->
                    rendered {
                        others =
                            renderRules
                                (emptyIntermediate (Selector (unwrap parent.selector <> " > " <> unwrap child)) "")
                                childRules
                                `cons` rendered.others
                    }

                AllChildren child childRules ->
                    rendered { 
                        others =
                            renderRules
                                (emptyIntermediate (parent.selector <> Selector " " <> child) "")
                                childRules
                                `cons` rendered.others
                    }

                Descriptor descriptor descriptorRules ->
                    rendered {
                        others =
                            renderRules
                                (emptyIntermediate (Selector (unwrap parent.selector <> unwrap descriptor)) "")
                                descriptorRules
                                `cons` rendered.others
                    }

                Batch batched ->
                    rendered {
                        others =
                            renderRules (emptyIntermediate parent.selector "") batched
                                `cons` rendered.others
                    }
    in
    Intermediate $ Array.foldr generateIntermediates parent rulesToRender

renderCompact :: Array Class -> String
renderCompact styleClasses =
    let
        renderValues :: Array (Tuple String String) -> String
        renderValues values =
            values
                # map (\(Tuple x y) -> x <> ":" <> y <> ";")
                # String.joinWith ""

        renderClass :: Intermediate -> String
        renderClass (Intermediate { selector, props, closing }) =
            case props of
                [] ->
                    ""

                _ ->
                    unwrap selector <> "{" <> renderValues props <> closing <> "}"

        renderIntermediate :: Intermediate -> String
        renderIntermediate (Intermediate rule) =
            renderClass (Intermediate rule)
                <> String.joinWith "" (map renderIntermediate rule.others)
    in
    styleClasses
        # Array.foldr
            (\(Class name styleRules) existing ->
                renderRules (emptyIntermediate name "") styleRules `cons` existing
            )
            []
        # map renderIntermediate
        # String.joinWith ""

classes =
    { root: ClassName "ui"
    , any: ClassName "s"
    , single: ClassName "e"
    , row: ClassName "r"
    , column: ClassName "c"
    , page: ClassName "pg"
    , paragraph: ClassName "p"
    , text: ClassName "t"
    , grid: ClassName "g"
    , imageContainer: ClassName "ic"
    , wrapped: ClassName "wrp"

    -- widhts/heights
    , widthFill: ClassName "wf"
    , widthContent: ClassName "wc"
    , widthExact: ClassName "we"
    , widthFillPortion: ClassName "wfp"
    , heightFill: ClassName "hf"
    , heightContent: ClassName "hc"
    , heightExact: ClassName "he"
    , heightFillPortion: ClassName "hfp"
    , seButton: ClassName "sbt"

    -- nearby elements
    , nearby: ClassName "nb"
    , above: ClassName "a"
    , below: ClassName "b"
    , onRight: ClassName "or"
    , onLeft: ClassName "ol"
    , inFront: ClassName "fr"
    , behind: ClassName "bh"
    , hasBehind: ClassName "hbh"

    -- alignments
    , alignTop: ClassName "at"
    , alignBottom: ClassName "ab"
    , alignRight: ClassName "ar"
    , alignLeft: ClassName "al"
    , alignCenterX: ClassName "cx"
    , alignCenterY: ClassName "cy"
    , alignedHorizontally: ClassName "ah"
    , alignedVertically: ClassName "av"

    -- space evenly
    , spaceEvenly: ClassName "sev"
    , container: ClassName "ctr"
    , alignContainerRight: ClassName "acr"
    , alignContainerBottom: ClassName "acb"
    , alignContainerCenterX: ClassName "accx"
    , alignContainerCenterY: ClassName "accy"

    -- content alignments
    , contentTop: ClassName "ct"
    , contentBottom: ClassName "cb"
    , contentRight: ClassName "cr"
    , contentLeft: ClassName "cl"
    , contentCenterX: ClassName "ccx"
    , contentCenterY: ClassName "ccy"

    -- selection
    , noTextSelection: ClassName "notxt"
    , cursorPointer: ClassName "cptr"
    , cursorText: ClassName "ctxt"

    -- pointer events
    , passPointerEvents: ClassName "ppe"
    , capturePointerEvents: ClassName "cpe"
    , transparent: ClassName "clr"
    , opaque: ClassName "oq"
    , overflowHidden: ClassName "oh"

    -- special state classes
    , hover: ClassName "hv"

    -- , hoverOpaque: ClassName "hover-opaque"
    , focus: ClassName "fcs"
    , focusedWithin: ClassName "focus-within"

    -- , focusOpaque: ClassName "focus-opaque"
    , active: ClassName "atv"

    -- , activeOpaque: ClassName "active-opaque"
    --scrollbars
    , scrollbars: ClassName "sb"
    , scrollbarsX: ClassName "sbx"
    , scrollbarsY: ClassName "sby"
    , clip: ClassName "cp"
    , clipX: ClassName "cpx"
    , clipY: ClassName "cpy"

    -- borders
    , borderNone: ClassName "bn"
    , borderDashed: ClassName "bd"
    , borderDotted: ClassName "bdt"
    , borderSolid: ClassName "bs"

    -- text weight
    , sizeByCapital: ClassName "cap"
    , fullSize: ClassName "fs"
    , textThin: ClassName "w1"
    , textExtraLight: ClassName "w2"
    , textLight: ClassName "w3"
    , textNormalWeight: ClassName "w4"
    , textMedium: ClassName "w5"
    , textSemiBold: ClassName "w6"
    , bold: ClassName "w7"
    , textExtraBold: ClassName "w8"
    , textHeavy: ClassName "w9"
    , italic: ClassName "i"
    , strike: ClassName "sk"
    , underline: ClassName "u"
    , textUnitalicized: ClassName "tun"

    -- text alignment
    , textJustify: ClassName "tj"
    , textJustifyAll: ClassName "tja"
    , textCenter: ClassName "tc"
    , textRight: ClassName "tr"
    , textLeft: ClassName "tl"
    , transition: ClassName "ts"

    -- inputText
    , inputText: ClassName "it"
    , inputMultiline: ClassName "iml"
    , inputMultilineParent: ClassName "imlp"
    , inputMultilineFiller: ClassName "imlf"
    , inputMultilineWrapper: ClassName "implw"
    , inputLabel: ClassName "lbl"

    -- link
    , link: ClassName "lnk"
    }

dot :: ClassName -> Selector
dot (ClassName c) =
    Selector $ "." <> c

data Alignment
    = Top
    | Bottom
    | Right
    | Left
    | CenterX
    | CenterY

alignments :: Array Alignment
alignments =
    [ Top
    , Bottom
    , Right
    , Left
    , CenterX
    , CenterY
    ]

data Location
    = Above
    | Below
    | OnRight
    | OnLeft
    | Within
    | Behind

locations :: Array Location
locations =
    let
        loc =
            Above

        _ =
            case loc of
                Above ->
                    unit

                Below ->
                    unit

                OnRight ->
                    unit

                OnLeft ->
                    unit

                Within ->
                    unit

                Behind ->
                    unit
    in
    [ Above
    , Below
    , OnRight
    , OnLeft
    , Within
    , Behind
    ]

data ContentDescriptor
    = Content Alignment

contentName :: ContentDescriptor -> Selector
contentName desc =
    case desc of
        Content Top ->
            dot classes.contentTop

        Content Bottom ->
            dot classes.contentBottom

        Content Right ->
            dot classes.contentRight

        Content Left ->
            dot classes.contentLeft

        Content CenterX ->
            dot classes.contentCenterX

        Content CenterY ->
            dot classes.contentCenterY

describeAlignment :: (Alignment -> (Tuple (Array Rule) (Array Rule))) -> Rule
describeAlignment values =
    let
        createDescription alignment =
            let
                (Tuple content indiv) =
                    values alignment
            in
            [ Descriptor (contentName (Content alignment)) $
                content
            , Child (dot classes.any)
                [ Descriptor (selfName $ Self alignment) indiv
                ]
            ]
    in
    Batch $
        Array.concatMap createDescription alignments

data SelfDescriptor
    = Self Alignment

selfName :: SelfDescriptor -> Selector
selfName desc =
    case desc of
        Self Top ->
            dot classes.alignTop

        Self Bottom ->
            dot classes.alignBottom

        Self Right ->
            dot classes.alignRight

        Self Left ->
            dot classes.alignLeft

        Self CenterX ->
            dot classes.alignCenterX

        Self CenterY ->
            dot classes.alignCenterY

gridAlignments :: (Alignment -> Array Rule) -> Rule
gridAlignments values =
    let
        createDescription alignment =
            [ Child (dot classes.any)
                [ Descriptor (selfName $ Self alignment) (values alignment)
                ]
            ]
    in
    Batch $
        Array.concatMap createDescription alignments

baseSheet :: Array Class
baseSheet =
    [ Class (Selector "html,body")
        [ Prop "height" "100%"
        , Prop "padding" "0"
        , Prop "margin" "0"
        ]
    , Class (Selector (unwrap (dot classes.any) <> unwrap (dot classes.single) <> unwrap (dot classes.imageContainer)))
        [ Prop "display" "block"
        , Descriptor (dot classes.heightFill)
            [ Child (Selector "img")
                [ Prop "max-height" "100%"
                , Prop "object-fit" "cover"
                ]
            ]
        , Descriptor (dot classes.widthFill)
            [ Child (Selector "img")
                [ Prop "max-width" "100%"
                , Prop "object-fit" "cover"
                ]
            ]
        ]
    , Class (Selector (unwrap (dot classes.any) <> ":focus"))
        [ Prop "outline" "none"
        ]
    , Class (dot classes.root)
        [ Prop "width" "100%"
        , Prop "height" "auto"
        , Prop "min-height" "100%"
        , Prop "z-index" "0"
        , Descriptor
            (dot classes.any
                -- <> dot classes.single
                <> dot classes.heightFill
            )
            [ Prop "height" "100%"
            , Child (dot classes.heightFill)
                [ Prop "height" "100%"
                ]
            ]
        , Child (dot classes.inFront)
            [ Descriptor (dot classes.nearby)
                [ Prop "position" "fixed"
                , Prop "z-index" "20"
                ]
            ]
        ]
    , Class (dot classes.nearby)
        [ Prop "position" "relative"
        , Prop "border" "none"
        , Prop "display" "flex"
        , Prop "flex-direction" "row"
        , Prop "flex-basis" "auto"
        , Descriptor (dot classes.single)
            elDescription
        , Batch $
            (\fn -> map fn locations) $
                \loc ->
                    case loc of
                        Above ->
                            Descriptor (dot classes.above)
                                [ Prop "position" "absolute"
                                , Prop "bottom" "100%"
                                , Prop "left" "0"
                                , Prop "width" "100%"
                                , Prop "z-index" "20"
                                , Prop "margin" "0 !important"
                                , Child (dot classes.heightFill)
                                    [ Prop "height" "auto"
                                    ]
                                , Child (dot classes.widthFill)
                                    [ Prop "width" "100%"
                                    ]
                                , Prop "pointer-events" "none"
                                , Child (Selector "*")
                                    [ Prop "pointer-events" "auto"
                                    ]
                                ]

                        Below ->
                            Descriptor (dot classes.below)
                                [ Prop "position" "absolute"
                                , Prop "bottom" "0"
                                , Prop "left" "0"
                                , Prop "height" "0"
                                , Prop "width" "100%"
                                , Prop "z-index" "20"
                                , Prop "margin" "0 !important"
                                , Prop "pointer-events" "none"
                                , Child (Selector "*")
                                    [ Prop "pointer-events" "auto"
                                    ]
                                , Child (dot classes.heightFill)
                                    [ Prop "height" "auto"
                                    ]
                                ]

                        OnRight ->
                            Descriptor (dot classes.onRight)
                                [ Prop "position" "absolute"
                                , Prop "left" "100%"
                                , Prop "top" "0"
                                , Prop "height" "100%"
                                , Prop "margin" "0 !important"
                                , Prop "z-index" "20"
                                , Prop "pointer-events" "none"
                                , Child (Selector "*")
                                    [ Prop "pointer-events" "auto"
                                    ]
                                ]

                        OnLeft ->
                            Descriptor (dot classes.onLeft)
                                [ Prop "position" "absolute"
                                , Prop "right" "100%"
                                , Prop "top" "0"
                                , Prop "height" "100%"
                                , Prop "margin" "0 !important"
                                , Prop "z-index" "20"
                                , Prop "pointer-events" "none"
                                , Child (Selector "*")
                                    [ Prop "pointer-events" "auto"
                                    ]
                                ]

                        Within ->
                            Descriptor (dot classes.inFront)
                                [ Prop "position" "absolute"
                                , Prop "width" "100%"
                                , Prop "height" "100%"
                                , Prop "left" "0"
                                , Prop "top" "0"
                                , Prop "margin" "0 !important"
                                , Prop "pointer-events" "none"
                                , Child (Selector "*")
                                    [ Prop "pointer-events" "auto"
                                    ]
                                ]

                        Behind ->
                            Descriptor (dot classes.behind)
                                [ Prop "position" "absolute"
                                , Prop "width" "100%"
                                , Prop "height" "100%"
                                , Prop "left" "0"
                                , Prop "top" "0"
                                , Prop "margin" "0 !important"
                                , Prop "z-index" "0"
                                , Prop "pointer-events" "none"
                                , Child (Selector "*")
                                    [ Prop "pointer-events" "auto"
                                    ]
                                ]
        ]
    , Class (dot classes.any)
        [ Prop "position" "relative"
        , Prop "border" "none"
        , Prop "flex-shrink" "0"
        , Prop "display" "flex"
        , Prop "flex-direction" "row"
        , Prop "flex-basis" "auto"
        , Prop "resize" "none"
        , Prop "font-feature-settings" "inherit"

        -- , Prop "flex-basis" "0%"
        , Prop "box-sizing" "border-box"
        , Prop "margin" "0"
        , Prop "padding" "0"
        , Prop "border-width" "0"
        , Prop "border-style" "solid"

        -- inheritable font properties
        , Prop "font-size" "inherit"
        , Prop "color" "inherit"
        , Prop "font-family" "inherit"
        , Prop "line-height" "1"
        , Prop "font-weight" "inherit"

        -- Text decoration is *mandatorily inherited* in the css spec.
        -- There's no way to change this.  How crazy is that?
        , Prop "text-decoration" "none"
        , Prop "font-style" "inherit"
        , Descriptor (dot classes.wrapped)
            [ Prop "flex-wrap" "wrap"
            ]
        , Descriptor (dot classes.noTextSelection)
            [ Prop "-moz-user-select" "none"
            , Prop "-webkit-user-select" "none"
            , Prop "-ms-user-select" "none"
            , Prop "user-select" "none"
            ]
        , Descriptor (dot classes.cursorPointer)
            [ Prop "cursor" "pointer"
            ]
        , Descriptor (dot classes.cursorText)
            [ Prop "cursor" "text"
            ]
        , Descriptor (dot classes.passPointerEvents)
            [ Prop "pointer-events" "none !important"
            ]
        , Descriptor (dot classes.capturePointerEvents)
            [ Prop "pointer-events" "auto !important"
            ]
        , Descriptor (dot classes.transparent)
            [ Prop "opacity" "0"
            ]
        , Descriptor (dot classes.opaque)
            [ Prop "opacity" "1"
            ]
        , Descriptor (dot (classes.hover `appendClassname` classes.transparent) <> Selector ":hover")
            [ Prop "opacity" "0"
            ]
        , Descriptor (dot (classes.hover `appendClassname` classes.opaque) <> Selector ":hover")
            [ Prop "opacity" "1"
            ]
        , Descriptor (dot (classes.focus `appendClassname` classes.transparent) <> Selector ":focus")
            [ Prop "opacity" "0"
            ]
        , Descriptor (dot (classes.focus `appendClassname` classes.opaque) <> Selector ":focus")
            [ Prop "opacity" "1"
            ]
        , Descriptor (dot (classes.active `appendClassname` classes.transparent) <> Selector ":active")
            [ Prop "opacity" "0"
            ]
        , Descriptor (dot (classes.active `appendClassname` classes.opaque) <> Selector ":active")
            [ Prop "opacity" "1"
            ]
        , Descriptor (dot classes.transition)
            [ Prop "transition"
                (String.joinWith ", " $
                    map (\x -> x <> " 160ms")
                        [ "transform"
                        , "opacity"
                        , "filter"
                        , "background-color"
                        , "color"
                        , "font-size"
                        ]
                )
            ]
        , Descriptor (dot classes.scrollbars)
            [ Prop "overflow" "auto"
            , Prop "flex-shrink" "1"
            ]
        , Descriptor (dot classes.scrollbarsX)
            [ Prop "overflow-x" "auto"
            , Descriptor (dot classes.row)
                [ Prop "flex-shrink" "1"
                ]
            ]
        , Descriptor (dot classes.scrollbarsY)
            [ Prop "overflow-y" "auto"
            , Descriptor (dot classes.column)
                [ Prop "flex-shrink" "1"
                ]
            , Descriptor (dot classes.single)
                [ Prop "flex-shrink" "1"
                ]
            ]
        , Descriptor (dot classes.clip)
            [ Prop "overflow" "hidden"
            ]
        , Descriptor (dot classes.clipX)
            [ Prop "overflow-x" "hidden"
            ]
        , Descriptor (dot classes.clipY)
            [ Prop "overflow-y" "hidden"
            ]
        , Descriptor (dot classes.widthContent)
            [ Prop "width" "auto"
            ]
        , Descriptor (dot classes.borderNone)
            [ Prop "border-width" "0"
            ]
        , Descriptor (dot classes.borderDashed)
            [ Prop "border-style" "dashed"
            ]
        , Descriptor (dot classes.borderDotted)
            [ Prop "border-style" "dotted"
            ]
        , Descriptor (dot classes.borderSolid)
            [ Prop "border-style" "solid"
            ]
        , Descriptor (dot classes.text)
            [ Prop "white-space" "pre"
            , Prop "display" "inline-block"
            ]
        , Descriptor (dot classes.inputText)
            -- chrome and safari have a minimum recognized line height for text input of 1.05
            -- If it's 1, it bumps up to something like 1.2
            [ Prop "line-height" "1.05"
            , Prop "background" "transparent"
            , Prop "text-align" "inherit"
            ]
        , Descriptor (dot classes.single)
            elDescription
        , Descriptor (dot classes.row)
            [ Prop "display" "flex"
            , Prop "flex-direction" "row"
            , Child (dot classes.any)
                [ Prop "flex-basis" "0%"
                , Descriptor (dot classes.widthExact)
                    [ Prop "flex-basis" "auto"
                    ]
                , Descriptor (dot classes.link)
                    [ Prop "flex-basis" "auto"
                    ]
                ]
            , Child (dot classes.heightFill)
                [ -- alignTop, centerY, and alignBottom need to be disabled
                  Prop "align-self" "stretch !important"
                ]
            , Child (dot classes.heightFillPortion)
                [ -- alignTop, centerY, and alignBottom need to be disabled
                  Prop "align-self" "stretch !important"
                ]

            -- TODO:: This may be necessary..should it move to classes.heightFIll?
            -- , Child (dot classes.heightFillBetween)
            --     [ Prop "align-self" "stretch"
            --     , Descriptor ".aligned-vertically"
            --         [ Prop "height" "100%"
            --         ]
            --     ]
            , Child (dot classes.widthFill)
                [ Prop "flex-grow" "100000"
                ]
            , Child (dot classes.container)
                [ Prop "flex-grow" "0"
                , Prop "flex-basis" "auto"
                , Prop "align-self" "stretch"
                ]

            -- , Child "alignLeft:last-of-type.align-container-left"
            --     [ Prop "flex-grow" "1"
            --     ]
            -- alignRight -> <u>
            --centerX -> <s>
            , Child (Selector "u:first-of-type." <> Selector (unwrap classes.alignContainerRight))
                [ Prop "flex-grow" "1"
                ]

            -- first center y
            , Child (Selector "s:first-of-type." <> Selector (unwrap classes.alignContainerCenterX))
                [ Prop "flex-grow" "1"
                , Child (dot classes.alignCenterX)
                    [ Prop "margin-left" "auto !important"
                    ]
                ]
            , Child (Selector "s:last-of-type." <> Selector (unwrap classes.alignContainerCenterX))
                [ Prop "flex-grow" "1"
                , Child (dot classes.alignCenterX)
                    [ Prop "margin-right" "auto !important"
                    ]
                ]

            -- lonley centerX
            , Child (Selector "s:only-of-type." <> Selector (unwrap classes.alignContainerCenterX))
                [ Prop "flex-grow" "1"
                , Child (dot classes.alignCenterY)
                    [ Prop "margin-top" "auto !important"
                    , Prop "margin-bottom" "auto !important"
                    ]
                ]

            -- alignBottom's after a centerX should not grow
            , Child
                (Selector "s:last-of-type." <> Selector (unwrap classes.alignContainerCenterX) <> Selector " ~ u")
                [ Prop "flex-grow" "0"
                ]

            -- centerX's after an alignBottom should be ignored
            , Child (Selector "u:first-of-type." <> Selector (unwrap classes.alignContainerRight) <> Selector " ~ s." <> Selector (unwrap classes.alignContainerCenterX))
                -- Bottom alignment always overrides center alignment
                [ Prop "flex-grow" "0"
                ]
            , describeAlignment $
                \alignment ->
                    case alignment of
                        Top ->
                            (Tuple [ Prop "align-items" "flex-start" ]
                              [ Prop "align-self" "flex-start"
                              ]
                            )

                        Bottom ->
                            (Tuple [ Prop "align-items" "flex-end" ]
                              [ Prop "align-self" "flex-end"
                              ]
                            )

                        Right ->
                            (Tuple [ Prop "justify-content" "flex-end"
                              ]
                              []
                            )

                        Left ->
                            (Tuple [ Prop "justify-content" "flex-start"
                              ]
                              []
                            )

                        CenterX ->
                            (Tuple [ Prop "justify-content" "center"
                              ]
                              []
                            )

                        CenterY ->
                            (Tuple [ Prop "align-items" "center" ]
                              [ Prop "align-self" "center"
                              ]
                            )

            -- Must be below the alignment rules or else it interferes
            , Descriptor (dot classes.spaceEvenly)
                [ Prop "justify-content" "space-between"
                ]
            , Descriptor (dot classes.inputLabel)
                [ Prop "align-items" "baseline"
                ]
            ]
        , Descriptor (dot classes.column)
            [ Prop "display" "flex"
            , Prop "flex-direction" "column"
            , Child (dot classes.any)
                -- *Note* - While rows have flex-basis 0%,
                -- which allows for the children of a row to default to their content size
                -- This apparently is a different story for columns.
                -- Safari has an issue if this is flex-basis: 0%, as it goes entirely to 0,
                -- instead of the expected content size.
                -- So we add `min-height: min-content`, which isn't supported by IE, but works for all other browsers!
                -- Separately, 0% is different than 0px, but only for columns
                -- In columns, 0% will actually be calculated as `auto` for columns
                -- So, 0px is the one we want.
                [ Prop "flex-basis" "0px"
                , Prop "min-height" "min-content"
                , Descriptor (dot classes.heightExact)
                    [ Prop "flex-basis" "auto"
                    ]
                ]
            , Child (dot classes.heightFill)
                [ Prop "flex-grow" "100000"
                ]
            , Child (dot classes.widthFill)
                [ -- alignLeft, alignRight, centerX need to be disabled
                  --   Prop "align-self" "stretch !important"
                  Prop "width" "100%"
                ]
            , Child (dot classes.widthFillPortion)
                [ -- alignLeft, alignRight, centerX need to be disabled
                  --   Prop "align-self" "stretch !important"
                  Prop "width" "100%"
                ]

            -- TODO:: This might be necessary, maybe it should move to widthFill?
            -- , Child (dot classes.widthFill)
            --     [ Prop "align-self" "stretch"
            --     , Descriptor (dot classes.alignedHorizontally)
            --         [ Prop "width" "100%"
            --         ]
            --     ]
            , Child (dot classes.widthContent)
                [ Prop "align-self" "flex-start"
                ]

            -- , Child "alignTop:last-of-type.align-container-top"
            --     [ Prop "flex-grow" "1"
            --     ]
            , Child (Selector "u:first-of-type." <> Selector (unwrap classes.alignContainerBottom))
                [ Prop "flex-grow" "1"
                ]

            -- centerY -> <s>
            -- alignBottom -> <u>
            -- first center y
            , Child (Selector "s:first-of-type." <> Selector (unwrap classes.alignContainerCenterY))
                [ Prop "flex-grow" "1"
                , Child (dot classes.alignCenterY)
                    [ Prop "margin-top" "auto !important"
                    , Prop "margin-bottom" "0 !important"
                    ]
                ]
            , Child (Selector "s:last-of-type." <> Selector (unwrap classes.alignContainerCenterY))
                [ Prop "flex-grow" "1"
                , Child (dot classes.alignCenterY)
                    [ Prop "margin-bottom" "auto !important"
                    , Prop "margin-top" "0 !important"
                    ]
                ]

            -- lonley centerY
            , Child (Selector "s:only-of-type." <> Selector (unwrap classes.alignContainerCenterY))
                [ Prop "flex-grow" "1"
                , Child (dot classes.alignCenterY)
                    [ Prop "margin-top" "auto !important"
                    , Prop "margin-bottom" "auto !important"
                    ]
                ]

            -- alignBottom's after a centerY should not grow
            , Child (Selector "s:last-of-type." <> Selector (unwrap classes.alignContainerCenterY) <> Selector" ~ u")
                [ Prop "flex-grow" "0"
                ]

            -- centerY's after an alignBottom should be ignored
            , Child (Selector "u:first-of-type." <> Selector (unwrap classes.alignContainerBottom) <> Selector " ~ s." <> Selector (unwrap classes.alignContainerCenterY))
                -- Bottom alignment always overrides center alignment
                [ Prop "flex-grow" "0"
                ]
            , describeAlignment $
                \alignment ->
                    case alignment of
                        Top ->
                            (Tuple [ Prop "justify-content" "flex-start" ]
                              [ Prop "margin-bottom" "auto" ]
                            )

                        Bottom ->
                            (Tuple [ Prop "justify-content" "flex-end" ]
                              [ Prop "margin-top" "auto" ]
                            )

                        Right ->
                            (Tuple [ Prop "align-items" "flex-end" ]
                              [ Prop "align-self" "flex-end" ]
                            )

                        Left ->
                            (Tuple [ Prop "align-items" "flex-start" ]
                              [ Prop "align-self" "flex-start" ]
                            )

                        CenterX ->
                            (Tuple [ Prop "align-items" "center" ]
                              [ Prop "align-self" "center"
                              ]
                            )

                        CenterY ->
                            (Tuple [ Prop "justify-content" "center" ]
                              []
                            )
            , Child (dot classes.container)
                [ Prop "flex-grow" "0"
                , Prop "flex-basis" "auto"
                , Prop "width" "100%"
                , Prop "align-self" "stretch !important"
                ]
            , Descriptor (dot classes.spaceEvenly)
                [ Prop "justify-content" "space-between"
                ]
            ]
        , Descriptor (dot classes.grid)
            [ Prop "display" "-ms-grid"
            , Child (Selector ".gp")
                [ Child (dot classes.any)
                    [ Prop "width" "100%"
                    ]
                ]
            , Supports (Tuple "display" "grid")
                [ (Tuple "display" "grid")
                ]
            , gridAlignments $
                \alignment ->
                    case alignment of
                        Top ->
                            [ Prop "justify-content" "flex-start" ]

                        Bottom ->
                            [ Prop "justify-content" "flex-end" ]

                        Right ->
                            [ Prop "align-items" "flex-end" ]

                        Left ->
                            [ Prop "align-items" "flex-start" ]

                        CenterX ->
                            [ Prop "align-items" "center" ]

                        CenterY ->
                            [ Prop "justify-content" "center" ]
            ]
        , Descriptor (dot classes.page)
            [ Prop "display" "block"
            , Child (dot classes.any <> Selector ":first-child")
                [ Prop "margin" "0 !important"
                ]

            -- clear spacing of any subsequent element if an element is float-left
            , Child (dot classes.any <> selfName (Self Left) <> Selector ":first-child + ." <> Selector (unwrap classes.any))
                [ Prop "margin" "0 !important"
                ]
            , Child (dot classes.any <> selfName (Self Right) <> Selector ":first-child + ." <> Selector (unwrap classes.any))
                [ Prop "margin" "0 !important"
                ]
            , describeAlignment $
                \alignment ->
                    case alignment of
                        Top ->
                            (Tuple []
                              []
                            )

                        Bottom ->
                            (Tuple []
                              []
                            )

                        Right ->
                            (Tuple []
                              [ Prop "float" "right"
                              , Descriptor (Selector "::after")
                                    [ Prop "content" "\"\""
                                    , Prop "display" "table"
                                    , Prop "clear" "both"
                                    ]
                              ]
                            )

                        Left ->
                            (Tuple []
                              [ Prop "float" "left"
                              , Descriptor (Selector "::after")
                                    [ Prop "content" "\"\""
                                    , Prop "display" "table"
                                    , Prop "clear" "both"
                                    ]
                              ]
                            )

                        CenterX ->
                            (Tuple []
                              []
                            )

                        CenterY ->
                            (Tuple []
                              []
                            )
            ]
        , Descriptor (dot classes.inputMultiline)
            [ Prop "white-space" "pre-wrap !important"
            , Prop "height" "100%"
            , Prop "width" "100%"
            , Prop "background-color" "transparent"
            ]
        , Descriptor (dot classes.inputMultilineWrapper)
            -- Get this.
            -- This allows multiline input to anchor scrolling to the bottom of the node
            -- when in a scrolling viewport, and the user is adding content.
            -- however, it only works in chrome.  In firefox, it prevents scrolling.
            --
            -- But how crazy is this solution?
            -- [ Prop "display" "flex"
            -- , Prop "flex-direction" "column-reverse"
            -- ]
            [ -- to increase specificity to beat another rule
              Descriptor (dot classes.single)
                [ Prop "flex-basis" "auto" ]
            ]
        , Descriptor (dot classes.inputMultilineParent)
            [ Prop "white-space" "pre-wrap !important"
            , Prop "cursor" "text"
            , Child (dot classes.inputMultilineFiller)
                [ Prop "white-space" "pre-wrap !important"
                , Prop "color" "transparent"
                ]
            ]
        , Descriptor (dot classes.paragraph)
            [ Prop "display" "block"
            , Prop "white-space" "normal"
            , Prop "overflow-wrap" "break-word"
            , Descriptor (dot classes.hasBehind)
                [ Prop "z-index" "0"
                , Child (dot classes.behind)
                    [ Prop "z-index" "-1"
                    ]
                ]
            , AllChildren (dot classes.text)
                [ Prop "display" "inline"
                , Prop "white-space" "normal"
                ]
            , AllChildren (dot classes.paragraph)
                [ Prop "display" "inline"
                , Descriptor (Selector "::after")
                    [ Prop "content" "none"
                    ]
                , Descriptor (Selector "::before")
                    [ Prop "content" "none"
                    ]
                ]
            , AllChildren (dot classes.single)
                [ Prop "display" "inline"
                , Prop "white-space" "normal"

                -- Inline block allows the width of the item to be set
                -- but DOES NOT like wrapping text in a standard, normal, sane way.
                -- We're sorta counting that if an exact width has been set,
                -- people aren't expecting proper text wrapping for this element
                , Descriptor (dot classes.widthExact)
                    [ Prop "display" "inline-block"
                    ]
                , Descriptor (dot classes.inFront)
                    [ Prop "display" "flex"
                    ]
                , Descriptor (dot classes.behind)
                    [ Prop "display" "flex"
                    ]
                , Descriptor (dot classes.above)
                    [ Prop "display" "flex"
                    ]
                , Descriptor (dot classes.below)
                    [ Prop "display" "flex"
                    ]
                , Descriptor (dot classes.onRight)
                    [ Prop "display" "flex"
                    ]
                , Descriptor (dot classes.onLeft)
                    [ Prop "display" "flex"
                    ]
                , Child (dot classes.text)
                    [ Prop "display" "inline"
                    , Prop "white-space" "normal"
                    ]
                ]
            , Child (dot classes.row)
                [ Prop "display" "inline"
                ]
            , Child (dot classes.column)
                [ Prop "display" "inline-flex"
                ]
            , Child (dot classes.grid)
                [ Prop "display" "inline-grid"
                ]
            , describeAlignment $
                \alignment ->
                    case alignment of
                        Top ->
                            (Tuple []
                              []
                            )

                        Bottom ->
                            (Tuple []
                              []
                            )

                        Right ->
                            (Tuple []
                              [ Prop "float" "right" ]
                            )

                        Left ->
                            (Tuple []
                              [ Prop "float" "left" ]
                            )

                        CenterX ->
                            (Tuple []
                              []
                            )

                        CenterY ->
                            (Tuple []
                              []
                            )
            ]
        , Descriptor (Selector ".hidden")
            [ Prop "display" "none"
            ]
        , Descriptor (dot classes.textThin)
            [ Prop "font-weight" "100"
            ]
        , Descriptor (dot classes.textExtraLight)
            [ Prop "font-weight" "200"
            ]
        , Descriptor (dot classes.textLight)
            [ Prop "font-weight" "300"
            ]
        , Descriptor (dot classes.textNormalWeight)
            [ Prop "font-weight" "400"
            ]
        , Descriptor (dot classes.textMedium)
            [ Prop "font-weight" "500"
            ]
        , Descriptor (dot classes.textSemiBold)
            [ Prop "font-weight" "600"
            ]
        , Descriptor (dot classes.bold)
            [ Prop "font-weight" "700"
            ]
        , Descriptor (dot classes.textExtraBold)
            [ Prop "font-weight" "800"
            ]
        , Descriptor (dot classes.textHeavy)
            [ Prop "font-weight" "900"
            ]
        , Descriptor (dot classes.italic)
            [ Prop "font-style" "italic"
            ]
        , Descriptor (dot classes.strike)
            [ Prop "text-decoration" "line-through"
            ]
        , Descriptor (dot classes.underline)
            [ Prop "text-decoration" "underline"
            , Prop "text-decoration-skip-ink" "auto"
            , Prop "text-decoration-skip" "ink"
            ]
        , Descriptor (dot classes.underline <> dot classes.strike)
            [ Prop "text-decoration" "line-through underline"
            , Prop "text-decoration-skip-ink" "auto"
            , Prop "text-decoration-skip" "ink"
            ]
        , Descriptor (dot classes.textUnitalicized)
            [ Prop "font-style" "normal"
            ]
        , Descriptor (dot classes.textJustify)
            [ Prop "text-align" "justify"
            ]
        , Descriptor (dot classes.textJustifyAll)
            [ Prop "text-align" "justify-all"
            ]
        , Descriptor (dot classes.textCenter)
            [ Prop "text-align" "center"
            ]
        , Descriptor (dot classes.textRight)
            [ Prop "text-align" "right"
            ]
        , Descriptor (dot classes.textLeft)
            [ Prop "text-align" "left"
            ]
        , Descriptor (Selector ".modal")
            [ Prop "position" "fixed"
            , Prop "left" "0"
            , Prop "top" "0"
            , Prop "width" "100%"
            , Prop "height" "100%"
            , Prop "pointer-events" "none"
            ]
        ]
    ]

elDescription :: Array Rule
elDescription =
    [ Prop "display" "flex"
    , Prop "flex-direction" "column"
    , Prop "white-space" "pre"
    , Descriptor (dot classes.hasBehind)
        [ Prop "z-index" "0"
        , Child (dot classes.behind)
            [ Prop "z-index" "-1"
            ]
        ]
    , Descriptor (dot classes.seButton)
        -- Special default for text in a button.
        -- This is overridden is they put the text inside an `el`
        [ Child (dot classes.text)
            [ Descriptor (dot classes.heightFill)
                [ Prop "flex-grow" "0"
                ]
            , Descriptor (dot classes.widthFill)
                [ Prop "align-self" "auto !important"
                ]
            ]
        ]
    , Child (dot classes.heightContent)
        [ Prop "height" "auto"
        ]
    , Child (dot classes.heightFill)
        [ Prop "flex-grow" "100000"
        ]
    , Child (dot classes.widthFill)
        [ -- alignLeft, alignRight, centerX are overridden by width.
          --   Prop "align-self" "stretch !important"
          Prop "width" "100%"
        ]
    , Child (dot classes.widthFillPortion)
        [ Prop "width" "100%"
        ]
    , Child (dot classes.widthContent)
        [ Prop "align-self" "flex-start"
        ]

    -- , Child (dot classes.widthFill)
    --     [ Prop "align-self" "stretch"
    --     , Descriptor (dot classes.alignedHorizontally)
    --         [ Prop "width" "100%"
    --         ]
    --     ]
    , describeAlignment $
        \alignment ->
            case alignment of
                Top ->
                    (Tuple [ Prop "justify-content" "flex-start" ]
                      [ Prop "margin-bottom" "auto !important"
                      , Prop "margin-top" "0 !important"
                      ]
                    )

                Bottom ->
                    (Tuple [ Prop "justify-content" "flex-end" ]
                      [ Prop "margin-top" "auto !important"
                      , Prop "margin-bottom" "0 !important"
                      ]
                    )

                Right ->
                    (Tuple [ Prop "align-items" "flex-end" ]
                      [ Prop "align-self" "flex-end" ]
                    )

                Left ->
                    (Tuple [ Prop "align-items" "flex-start" ]
                      [ Prop "align-self" "flex-start" ]
                    )

                CenterX ->
                    (Tuple [ Prop "align-items" "center" ]
                      [ Prop "align-self" "center"
                      ]
                    )

                CenterY ->
                    (Tuple [ -- Prop "justify-content" "center"
                        Child (dot classes.any)
                            [ Prop "margin-top" "auto"
                            , Prop "margin-bottom" "auto"
                            ]
                      ]
                      [ Prop "margin-top" "auto !important"
                      , Prop "margin-bottom" "auto !important"
                      ]
                    )
    ]
