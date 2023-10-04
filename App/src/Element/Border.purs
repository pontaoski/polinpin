module Element.Border
    ( color
    , width, widthXY, widthEach
    , solid, dashed, dotted
    , rounded, roundEach
    , glow, innerGlow, shadow, innerShadow
    )
where

{-|

@docs color


## Border Widths

@docs width, widthXY, widthEach


## Border Styles

@docs solid, dashed, dotted


## Rounded Corners

@docs rounded, roundEach


## Shadows

@docs glow, innerGlow, shadow, innerShadow

-}

import Prelude

import Element (Attr, Attribute, Color)
import Element.Internal.Flag as Flag
import Element.Internal.Model as Internal
import Element.Internal.Style as Style
import Element.Internal.Style (classes)
import Data.Tuple (Tuple(..))
import Data.Int as Int
import Data.Newtype (unwrap)

{-| -}
color :: forall decorative r p i. Color -> Attr decorative r p i
color clr =
    Internal.StyleClass
        Flag.borderColor
        (Internal.Colored
            ("bc-" <> Internal.formatColorClass clr)
            "border-color"
            clr
        )


{-| -}
width :: forall r p i. Int -> Attribute r p i
width v =
    Internal.StyleClass
        Flag.borderWidth
        (Internal.BorderWidth
            ("b-" <> Int.toStringAs Int.decimal v)
            v
            v
            v
            v
        )


{-| Set horizontal and vertical borders.
-}
widthXY :: forall r p i. Int -> Int -> Attribute r p i
widthXY x y =
    Internal.StyleClass
        Flag.borderWidth
        (Internal.BorderWidth
            ("b-"
                <> Int.toStringAs Int.decimal x
                <> "-"
                <> Int.toStringAs Int.decimal y
            )
            y
            x
            y
            x
        )


{-| -}
widthEach :: forall r p i.
    { bottom :: Int
    , left :: Int
    , right :: Int
    , top :: Int
    }
    -> Attribute r p i
widthEach { bottom, top, left, right } =
    if top == bottom && left == right then
        if top == right then
            width top

        else
            widthXY left top

    else
        Internal.StyleClass Flag.borderWidth
            (Internal.BorderWidth
                ("b-"
                    <> Int.toStringAs Int.decimal top
                    <> "-"
                    <> Int.toStringAs Int.decimal right
                    <> "-"
                    <> Int.toStringAs Int.decimal bottom
                    <> "-"
                    <> Int.toStringAs Int.decimal left
                )
                top
                right
                bottom
                left
            )



-- {-| No Borders
-- -}
-- none :: Attribute r p i
-- none =
--     Class "border" "border-none"


{-| -}
solid :: forall r p i. Attribute r p i
solid =
    Internal.Class Flag.borderStyle (unwrap classes.borderSolid)


{-| -}
dashed :: forall r p i. Attribute r p i
dashed =
    Internal.Class Flag.borderStyle (unwrap classes.borderDashed)


{-| -}
dotted :: forall r p i. Attribute r p i
dotted =
    Internal.Class Flag.borderStyle (unwrap classes.borderDotted)


{-| Round all corners.
-}
rounded :: forall r p i. Int -> Attribute r p i
rounded radius =
    Internal.StyleClass
        Flag.borderRound
        (Internal.Single
            ("br-" <> Int.toStringAs Int.decimal radius)
            "border-radius"
            (Int.toStringAs Int.decimal radius <> "px")
        )


{-| -}
roundEach :: forall r p i.
    { topLeft :: Int
    , topRight :: Int
    , bottomLeft :: Int
    , bottomRight :: Int
    }
    -> Attribute r p i
roundEach { topLeft, topRight, bottomLeft, bottomRight } =
    Internal.StyleClass Flag.borderRound
        (Internal.Single
            ("br-"
                <> Int.toStringAs Int.decimal topLeft
                <> "-"
                <> Int.toStringAs Int.decimal topRight
                <> Int.toStringAs Int.decimal bottomLeft
                <> "-"
                <> Int.toStringAs Int.decimal bottomRight
            )
            "border-radius"
            (Int.toStringAs Int.decimal topLeft
                <> "px "
                <> Int.toStringAs Int.decimal topRight
                <> "px "
                <> Int.toStringAs Int.decimal bottomRight
                <> "px "
                <> Int.toStringAs Int.decimal bottomLeft
                <> "px"
            )
        )


{-| A simple glow by specifying the color and size.
-}
glow :: forall decorative r p i. Color -> Number -> Attr decorative r p i
glow clr size =
    shadow
        { offset: (Tuple 0.0 0.0)
        , size: size
        , blur: size * 2.0
        , color: clr
        }


{-| -}
innerGlow :: forall decorative r p i. Color -> Number -> Attr decorative r p i
innerGlow clr size =
    innerShadow
        { offset: (Tuple 0.0 0.0)
        , size: size
        , blur: size * 2.0
        , color: clr
        }


{-| -}
shadow :: forall decorative r p i.
    { offset :: (Tuple Number Number)
    , size :: Number
    , blur :: Number
    , color :: Color
    }
    -> Attr decorative r p i
shadow almostShade =
    let
        shade =
            { inset: false
            , offset: almostShade.offset
            , size: almostShade.size
            , blur: almostShade.blur
            , color: almostShade.color
            }
    in
    Internal.StyleClass Flag.shadows $
        Internal.Single
            (Internal.boxShadowClass shade)
            "box-shadow"
            (Internal.formatBoxShadow shade)


{-| -}
innerShadow :: forall decorative r p i.
    { offset :: (Tuple Number Number)
    , size :: Number
    , blur :: Number
    , color :: Color
    }
    -> Attr decorative r p i
innerShadow almostShade =
    let
        shade =
            { inset: true
            , offset: almostShade.offset
            , size: almostShade.size
            , blur: almostShade.blur
            , color: almostShade.color
            }
    in
    Internal.StyleClass Flag.shadows $
        Internal.Single
            (Internal.boxShadowClass shade)
            "box-shadow"
            (Internal.formatBoxShadow shade)



-- {-| -}
-- shadow :
--     { offset :: ( Number, Number )
--     , blur :: Number
--     , size :: Number
--     , color :: Color
--     }
--     -> Attr decorative r p i
-- shadow shade =
--     Internal.BoxShadow
--         { inset = False
--         , offset = shade.offset
--         , size = shade.size
--         , blur = shade.blur
--         , color = shade.color
--         }
