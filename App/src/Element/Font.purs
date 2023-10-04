module Element.Font
    ( color, size
    , family, Font, typeface, serif, sansSerif, monospace
    , external
    , alignLeft, alignRight, center, justify, letterSpacing, wordSpacing
    , underline, strike, italic, unitalicized
    , heavy, extraBold, bold, semiBold, medium, regular, light, extraLight, hairline
    , Variant, variant, variantList, smallCaps, slashedZero, ligatures, ordinal, tabularNumbers, stackedFractions, diagonalFractions, swash, feature, indexed
    , glow, shadow
    )
where

{-|

    import Element
    import Element.Font as Font

    view =
        Element.el
            [ Font.color (Element.rgb 0 0 1)
            , Font.size 18
            , Font.family
                [ Font.typeface "Open Sans"
                , Font.sansSerif
                ]
            ]
            (Element.text "Woohoo, I'm stylish text")

**Note:** `Font.color`, `Font.size`, and `Font.family` are inherited, meaning you can set them at the top of your view and all subsequent nodes will have that value.

**Other Note:** If you're looking for something like `line-height`, it's handled by `Element.spacing` on a `paragraph`.

@docs color, size


## Typefaces

@docs family, Font, typeface, serif, sansSerif, monospace

@docs external


## Alignment and Spacing

@docs alignLeft, alignRight, center, justify, letterSpacing, wordSpacing


## Font Styles

@docs underline, strike, italic, unitalicized


## Font Weight

@docs heavy, extraBold, bold, semiBold, medium, regular, light, extraLight, hairline


## Variants

@docs Variant, variant, variantList, smallCaps, slashedZero, ligatures, ordinal, tabularNumbers, stackedFractions, diagonalFractions, swash, feature, indexed


## Shadows

@docs glow, shadow

-}

import Prelude
import Element (Attr, Attribute, Color)
import Element.Internal.Flag as Flag
import Element.Internal.Model as Internal
import Element.Internal.Style (classes, Selector(..))
import Data.Array as Array
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe)
import Data.Number.Format as NumberFormat
import Data.Int as Int
import Data.String as String
import Data.Newtype (unwrap)
import Halogen.HTML (ClassName(..))

{-| -}
type Font =
    Internal.Font


{-| -}
color :: forall decorative r p i. Color -> Attr decorative r p i
color fontColor =
    Internal.StyleClass
        Flag.fontColor
        (Internal.Colored
            ("fc-" <> Internal.formatColorClass fontColor)
            "color"
            fontColor
        )


{-|

    import Element
    import Element.Font as Font

    myElement =
        Element.el
            [ Font.family
                [ Font.typeface "Helvetica"
                , Font.sansSerif
                ]
            ]
            (text "")

-}
family :: forall r p i. Array Font -> Attribute r p i
family families =
    Internal.StyleClass
        Flag.fontFamily
        (Internal.FontFamily
            (Internal.elmFoldlArray Internal.renderFontClassName "ff-" families)
            families
        )


{-| -}
serif :: Font
serif =
    Internal.Serif


{-| -}
sansSerif :: Font
sansSerif =
    Internal.SansSerif


{-| -}
monospace :: Font
monospace =
    Internal.Monospace


{-| -}
typeface :: String -> Font
typeface =
    Internal.Typeface


{-| -}
type Adjustment =
    { capital :: Number
    , lowercase :: Number
    , baseline :: Number
    , descender :: Number
    }


{-| -}
with ::
    { name :: String
    , adjustment :: Maybe Adjustment
    , variants :: Array Variant
    }
    -> Font
with =
    Internal.FontWith


{-| -}
sizeByCapital :: forall r p i. Attribute r p i
sizeByCapital =
    Internal.htmlClass classes.sizeByCapital


{-| -}
full :: forall r p i. Attribute r p i
full =
    Internal.htmlClass classes.fullSize


{-| **Note** it's likely that `Font.external` will cause a flash on your page on loading.

To bypass this, import your fonts using a separate stylesheet and just use `Font.typeface`.

It's likely that `Font.external` will be removed or redesigned in the future to avoid the flashing.

`Font.external` can be used to import font files. Let's say you found a neat font on <http://fonts.google.com>:

    import Element
    import Element.Font as Font

    view =
        Element.el
            [ Font.family
                [ Font.external
                    { name = "Roboto"
                    , url = "https://fonts.googleapis.com/css?family=Roboto"
                    }
                , Font.sansSerif
                ]
            ]
            (Element.text "Woohoo, I'm stylish text")

-}
external :: { url :: String, name :: String } -> Font
external { url, name } =
    Internal.ImportFont name url


{-| Font sizes are always given as `px`.
-}
size :: forall decorative r p i. Int -> Attr decorative r p i
size i =
    Internal.StyleClass Flag.fontSize (Internal.FontSize i)


{-| In `px`.
-}
letterSpacing :: forall r p i. Number -> Attribute r p i
letterSpacing offset =
    Internal.StyleClass Flag.letterSpacing $
        Internal.Single
            (Selector ("ls-" <> Internal.floatClass offset))
            "letter-spacing"
            (NumberFormat.toString offset <> "px")


{-| In `px`.
-}
wordSpacing :: forall r p i. Number -> Attribute r p i
wordSpacing offset =
    Internal.StyleClass Flag.wordSpacing $
        Internal.Single (Selector $ "ws-" <> Internal.floatClass offset) "word-spacing" (NumberFormat.toString offset <> "px")


{-| Align the font to the left.
-}
alignLeft :: forall r p i. Attribute r p i
alignLeft =
    Internal.Class Flag.fontAlignment classes.textLeft


{-| Align the font to the right.
-}
alignRight :: forall r p i. Attribute r p i
alignRight =
    Internal.Class Flag.fontAlignment classes.textRight


{-| Center align the font.
-}
center :: forall r p i. Attribute r p i
center =
    Internal.Class Flag.fontAlignment classes.textCenter


{-| -}
justify :: forall r p i. Attribute r p i
justify =
    Internal.Class Flag.fontAlignment classes.textJustify



-- {-| -}
-- justifyAll :: Attribute msg
-- justifyAll =
--     Internal.class classesTextJustifyAll


{-| -}
underline :: forall r p i. Attribute r p i
underline =
    Internal.htmlClass classes.underline


{-| -}
strike :: forall r p i. Attribute r p i
strike =
    Internal.htmlClass classes.strike


{-| -}
italic :: forall r p i. Attribute r p i
italic =
    Internal.htmlClass classes.italic


{-| -}
bold :: forall r p i. Attribute r p i
bold =
    Internal.Class Flag.fontWeight classes.bold


{-| -}
light :: forall r p i. Attribute r p i
light =
    Internal.Class Flag.fontWeight classes.textLight


{-| -}
hairline :: forall r p i. Attribute r p i
hairline =
    Internal.Class Flag.fontWeight classes.textThin


{-| -}
extraLight :: forall r p i. Attribute r p i
extraLight =
    Internal.Class Flag.fontWeight classes.textExtraLight


{-| -}
regular :: forall r p i. Attribute r p i
regular =
    Internal.Class Flag.fontWeight classes.textNormalWeight


{-| -}
semiBold :: forall r p i. Attribute r p i
semiBold =
    Internal.Class Flag.fontWeight classes.textSemiBold


{-| -}
medium :: forall r p i. Attribute r p i
medium =
    Internal.Class Flag.fontWeight classes.textMedium


{-| -}
extraBold :: forall r p i. Attribute r p i
extraBold =
    Internal.Class Flag.fontWeight classes.textExtraBold


{-| -}
heavy :: forall r p i. Attribute r p i
heavy =
    Internal.Class Flag.fontWeight classes.textHeavy


{-| This will reset bold and italic.
-}
unitalicized :: forall r p i. Attribute r p i
unitalicized =
    Internal.htmlClass classes.textUnitalicized


{-| -}
shadow ::
    forall decorative r p i.
    { offset :: (Tuple Number Number)
    , blur :: Number
    , color :: Color
    }
    -> Attr decorative r p i
shadow shade =
    Internal.StyleClass Flag.txtShadows $
        Internal.Single (Selector (Internal.textShadowClass shade)) "text-shadow" (Internal.formatTextShadow shade)


{-| A glow is just a simplified shadow.
-}
glow :: forall decorative r p i. Color -> Number -> Attr decorative r p i
glow clr i =
    let
        shade =
            { offset: (Tuple 0.0 0.0)
            , blur: i * 2.0
            , color: clr
            }
    in
    Internal.StyleClass Flag.txtShadows $
        Internal.Single (Selector (Internal.textShadowClass shade)) "text-shadow" (Internal.formatTextShadow shade)



{- Variants -}


{-| -}
type Variant =
    Internal.Variant


{-| You can use this to set a single variant on an element itself such as:

    el
        [ Font.variant Font.smallCaps
        ]
        (text "rendered with smallCaps")

**Note** These will **not** stack. If you want multiple variants, you should use `Font.variantList`.

-}
variant :: forall r p i. Variant -> Attribute r p i
variant var =
    case var of
        Internal.VariantActive name ->
            Internal.Class Flag.fontVariant (ClassName ("v-" <> name))

        Internal.VariantOff name ->
            Internal.Class Flag.fontVariant (ClassName ("v-" <> name <> "-off"))

        Internal.VariantIndexed name index ->
            Internal.StyleClass Flag.fontVariant $
                Internal.Single (Selector ("v-" <> name <> "-" <> Int.toStringAs Int.decimal index))
                    "font-feature-settings"
                    ("\"" <> name <> "\" " <> Int.toStringAs Int.decimal index)


isSmallCaps x =
    case x of
        Internal.VariantActive feat ->
            feat == "smcp"

        _ ->
            false


{-| -}
variantList :: forall r p i. Array Variant -> Attribute r p i
variantList vars =
    let
        features =
            vars
                # map Internal.renderVariant

        hasSmallCaps =
            Array.any isSmallCaps vars

        name =
            if hasSmallCaps then
                vars
                    # map Internal.variantName
                    # String.joinWith "-"
                    # (\x -> x <> "-sc")

            else
                vars
                    # map Internal.variantName
                    # String.joinWith "-"

        featureString =
            String.joinWith ", " features
    in
    Internal.StyleClass Flag.fontVariant $
        Internal.Style ("v-" <> name)
            [ Internal.Property "font-feature-settings" featureString
            , Internal.Property "font-variant"
                (if hasSmallCaps then
                    "small-caps"

                 else
                    "normal"
                )
            ]


{-| [Small caps](https://en.wikipedia.org/wiki/Small_caps) are rendered using uppercase glyphs, but at the size of lowercase glyphs.
-}
smallCaps :: Variant
smallCaps =
    Internal.VariantActive "smcp"


{-| Add a slash when rendering `0`
-}
slashedZero :: Variant
slashedZero =
    Internal.VariantActive "zero"


{-| -}
ligatures :: Variant
ligatures =
    Internal.VariantActive "liga"


{-| Oridinal markers like `1st` and `2nd` will receive special glyphs.
-}
ordinal :: Variant
ordinal =
    Internal.VariantActive "ordn"


{-| Number figures will each take up the same space, allowing them to be easily aligned, such as in tables.
-}
tabularNumbers :: Variant
tabularNumbers =
    Internal.VariantActive "tnum"


{-| Render fractions with the numerator stacked on top of the denominator.
-}
stackedFractions :: Variant
stackedFractions =
    Internal.VariantActive "afrc"


{-| Render fractions
-}
diagonalFractions :: Variant
diagonalFractions =
    Internal.VariantActive "frac"


{-| -}
swash :: Int -> Variant
swash =
    Internal.VariantIndexed "swsh"


{-| Set a feature by name and whether it should be on or off.

Feature names are four-letter names as defined in the [OpenType specification](https://docs.microsoft.com/en-us/typography/opentype/spec/featurelist).

-}
feature :: String -> Boolean -> Variant
feature name on =
    if on then
        Internal.VariantIndexed name 1

    else
        Internal.VariantIndexed name 0


{-| A font variant might have multiple versions within the font.

In these cases we need to specify the index of the version we want.

-}
indexed :: String -> Int -> Variant
indexed name on =
    Internal.VariantIndexed name on

