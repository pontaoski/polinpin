module Element.Background
    ( color, gradient
    , image, uncropped, tiled, tiledX, tiledY
    )
where

{-|

@docs color, gradient


# Images

@docs image, uncropped, tiled, tiledX, tiledY

**Note** if you want more control over a background image than is provided here, you should try just using a normal `Element.image` with something like `Element.behindContent`.

-}

import Prelude

import Element (Attr, Attribute, Color)
import Element.Internal.Flag as Flag
import Element.Internal.Model as Internal
import Element.Internal.Style (Selector(..))
import Halogen.HTML (IProp(..))
import Halogen.HTML.Properties as HP
import Data.List as List
import Data.List (List(..), (:))
import Data.String as String
import Data.Newtype (unwrap, wrap)
import Data.Number.Format as NumberFormat
import Data.Array as Array

{-| -}
color :: forall decorative p i. Color -> Attr decorative p i
color clr =
    Internal.StyleClass Flag.bgColor (Internal.Colored ("bg-" <> Internal.formatColorClass clr) "background-color" clr)


{-| Resize the image to fit the containing element while maintaining proportions and cropping the overflow.
-}
image :: forall p i. String -> Attribute p i
image src =
    let style = "background: url(\"" <> src <> "\") center / cover no-repeat" in
    Internal.iprop' (HP.style style)


{-| A centered background image that keeps its natural proportions, but scales to fit the space.
-}
uncropped :: forall p i. String -> Attribute p i
uncropped src =
    let style = "background: url(\"" <> src <> "\") center / contain no-repeat" in
    Internal.iprop' (HP.style style)


{-| Tile an image in the x and y axes.
-}
tiled :: forall p i. String -> Attribute p i
tiled src =
    let style = "background: url(\"" <> src <> "\") repeat" in
    Internal.iprop' (HP.style style)


{-| Tile an image in the x axis.
-}
tiledX :: forall p i. String -> Attribute p i
tiledX src =
    let style = "background: url(\"" <> src <> "\") repeat-x" in
    Internal.iprop' (HP.style style)


{-| Tile an image in the y axis.
-}
tiledY :: forall p i. String -> Attribute p i
tiledY src =
    let style = "background: url(\"" <> src <> "\") repeat-y" in
    Internal.iprop' (HP.style style)


data Direction
    = ToUp
    | ToDown
    | ToRight
    | ToTopRight
    | ToBottomRight
    | ToLeft
    | ToTopLeft
    | ToBottomLeft
    | ToAngle Number


data Step
    = ColorStep Color
    | PercentStep Number Color
    | PxStep Int Color


{-| -}
step :: Color -> Step
step =
    ColorStep


{-| -}
percent :: Number -> Color -> Step
percent =
    PercentStep


{-| -}
px :: Int -> Color -> Step
px =
    PxStep


{-| A linear gradient.

First you need to specify what direction the gradient is going by providing an angle in radians. `0` is up and `pi` is down.

The colors will be evenly spaced.

-}
gradient ::
    forall decorative p i.
    { angle :: Number
    , steps :: Array Color
    }
    -> Attr decorative p i
gradient { angle, steps: asteps } =
    let steps = List.fromFoldable asteps
    in
    case steps of
        Nil ->
            Internal.NoAttribute

        (Cons clr Nil) ->
            Internal.StyleClass Flag.bgColor
                (Internal.Colored ("bg-" <> Internal.formatColorClass clr) "background-color" clr)

        _ ->
            Internal.StyleClass Flag.bgGradient $
                Internal.Single (Selector $ "bg-grad-" <> (String.joinWith "-" $ Array.fromFoldable $ Internal.floatClass angle : map Internal.formatColorClass steps))
                    "background-image"
                    ("linear-gradient(" <> (String.joinWith ", " $ Array.fromFoldable $ (NumberFormat.toString angle <> "rad") : map Internal.formatColor steps) <> ")")



-- {-| -}
-- gradientWith :: { direction :: Direction, steps :: List Step } -> Attribute msg
-- gradientWith { direction, steps } =
--     StyleClass $
--         Single ("bg-gradient-" <> (String.join "-" $ renderDirectionClass direction :: List.map renderStepClass steps))
--             "background"
--             ("linear-gradient(" <> (String.join ", " $ renderDirection direction :: List.map renderStep steps) <> ")")
-- {-| -}
-- renderStep :: Step -> String
-- renderStep step =
--     case step of
--         ColorStep color ->
--             formatColor color
--         PercentStep percent color ->
--             formatColor color <> " " <> toString percent <> "%"
--         PxStep px color ->
--             formatColor color <> " " <> toString px <> "px"
-- {-| -}
-- renderStepClass :: Step -> String
-- renderStepClass step =
--     case step of
--         ColorStep color ->
--             formatColorClass color
--         PercentStep percent color ->
--             formatColorClass color <> "-" <> NumberClass percent <> "p"
--         PxStep px color ->
--             formatColorClass color <> "-" <> toString px <> "px"
-- toUp :: Direction
-- toUp =
--     ToUp
-- toDown :: Direction
-- toDown =
--     ToDown
-- toRight :: Direction
-- toRight =
--     ToRight
-- toTopRight :: Direction
-- toTopRight =
--     ToTopRight
-- toBottomRight :: Direction
-- toBottomRight =
--     ToBottomRight
-- toLeft :: Direction
-- toLeft =
--     ToLeft
-- toTopLeft :: Direction
-- toTopLeft =
--     ToTopLeft
-- toBottomLeft :: Direction
-- toBottomLeft =
--     ToBottomLeft
-- angle :: Number -> Direction
-- angle rad =
--     ToAngle rad
-- renderDirection :: Direction -> String
-- renderDirection dir =
--     case dir of
--         ToUp ->
--             "to top"
--         ToDown ->
--             "to bottom"
--         ToRight ->
--             "to right"
--         ToTopRight ->
--             "to top right"
--         ToBottomRight ->
--             "to bottom right"
--         ToLeft ->
--             "to left"
--         ToTopLeft ->
--             "to top left"
--         ToBottomLeft ->
--             "to bottom left"
--         ToAngle angle ->
--             toString angle <> "rad"
-- renderDirectionClass :: Direction -> String
-- renderDirectionClass dir =
--     case dir of
--         ToUp ->
--             "to-top"
--         ToDown ->
--             "to-bottom"
--         ToRight ->
--             "to-right"
--         ToTopRight ->
--             "to-top-right"
--         ToBottomRight ->
--             "to-bottom-right"
--         ToLeft ->
--             "to-left"
--         ToTopLeft ->
--             "to-top-left"
--         ToBottomLeft ->
--             "to-bottom-left"
--         ToAngle angle ->
--             NumberClass angle <> "rad"

