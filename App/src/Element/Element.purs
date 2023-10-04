module Element where

import Prelude

import Data.List (List)
import Halogen.HTML.Core (HTML)
import Halogen.HTML as HH
import Data.Int (toNumber)
import Data.Int as Int
import Data.Array as Array
import Data.Array ((:))
import Data.String as String
import Element.Internal.Model as Internal
import Element.Internal.Flag as Flag
import Element.Internal.Style (classes)
import Element.Internal.Style as Style
import Data.Newtype (unwrap)
import Debug as Debug

type Color = Internal.Color

{-| -}
rgb :: Number -> Number -> Number -> Color
rgb r g b =
    Internal.Rgba r g b 1.0

{-| Provide the red, green, and blue channels for the color.

Each channel takes a value between 0 and 1.

-}
rgba :: Number -> Number -> Number -> Number -> Color
rgba =
    Internal.Rgba

{-| Provide the red, green, and blue channels for the color.

Each channel takes a value between 0 and 255.

-}
rgb255 :: Int -> Int -> Int -> Color
rgb255 red green blue =
    Internal.Rgba
        (toNumber red / 255.0)
        (toNumber green / 255.0)
        (toNumber blue / 255.0)
        1.0


{-| -}
rgba255 :: Int -> Int -> Int -> Number -> Color
rgba255 red green blue a =
    Internal.Rgba
        (toNumber red / 255.0)
        (toNumber green / 255.0)
        (toNumber blue / 255.0)
        a


{-| Create a color from an RGB record.
-}
fromRgb ::
    { red :: Number
    , green :: Number
    , blue :: Number
    , alpha :: Number
    }
    -> Color
fromRgb clr =
    Internal.Rgba
        clr.red
        clr.green
        clr.blue
        clr.alpha


{-| -}
fromRgb255 ::
    { red :: Int
    , green :: Int
    , blue :: Int
    , alpha :: Number
    }
    -> Color
fromRgb255 clr =
    Internal.Rgba
        (toNumber clr.red / 255.0)
        (toNumber clr.green / 255.0)
        (toNumber clr.blue / 255.0)
        clr.alpha


{-| Deconstruct a `Color` into its rgb channels.
-}
toRgb ::
    Color
    ->
        { red :: Number
        , green :: Number
        , blue :: Number
        , alpha :: Number
        }
toRgb (Internal.Rgba r g b a) =
    { red: r
    , green: g
    , blue: b
    , alpha: a
    }

{- The basic building block of your layout. -}
type Element p i =
    Internal.Element p i

{-| An attribute that can be attached to an `Element`
-}
type Attribute r p i =
    Internal.Attribute Unit r p i

{-| This is a special attribute that counts as both a `Attribute msg` and a `Decoration`.
-}
type Attr decorative r p i =
    Internal.Attribute decorative r p i

{-| Only decorations
-}
type Decoration r =
    Internal.Attribute Void r Void Void

{-| -}
html :: forall p i . HTML p i -> Element p i
html =
    Internal.unstyled

{-| -}
htmlAttribute :: forall r p i . HH.IProp r i -> Attribute r p i
htmlAttribute =
    Internal.Attr

{-| -}
type Length = Internal.Length

{-| -}
px :: Int -> Length
px =
    Internal.Px


{-| Shrink an element to fit its contents. -}
shrink :: Length
shrink =
    Internal.Content


{-| Fill the available space. The available space will be split evenly between elements that have `width fill`. -}
fill :: Length
fill =
    Internal.Fill 1

{-| Similarly you can set a minimum boundary.

     el
        [ height
            (fill
                |> maximum 300
                |> minimum 30
            )

        ]
        (text "I will stop at 300px")

-}
minimum :: Int -> Length -> Length
minimum i l =
    Internal.Min i l


{-| Add a maximum to a length.

    el
        [ height
            (fill
                |> maximum 300
            )
        ]
        (text "I will stop at 300px")

-}
maximum :: Int -> Length -> Length
maximum i l =
    Internal.Max i l

{-| Sometimes you may not want to split available space evenly. In this case you can use `fillPortion` to define which elements should have what portion of the available space.

So, two elements, one with `width (fillPortion 2)` and one with `width (fillPortion 3)`. The first would get 2 portions of the available space, while the second would get 3.

**Also:** `fill == fillPortion 1`

-}
fillPortion :: Int -> Length
fillPortion =
    Internal.Fill

{-| This is your top level node where you can turn `Element` into `Html`.
-}
layout :: forall r p i . Array (Attribute r p i) -> Element p i -> HH.HTML p i
layout =
    layoutWith { options: [] }

{-| -}
type Option =
    Internal.Option

{-| -}
layoutWith :: forall r p i . { options :: Array Option } -> Array (Attribute r p i) -> Element p i -> HH.HTML p i
layoutWith { options } attrs child =
    let
        rendered =
                Internal.renderRoot options
                    (Internal.htmlClasses
                        [ classes.root
                        , classes.any
                        , classes.single
                        ]
                        `Array.cons` (Internal.rootStyle <> attrs)
                    )
                    child
    in
    Debug.trace rendered \_ -> rendered

{-| -}
width :: forall r p i. Length -> Attribute r p i
width =
    Internal.Width


{-| -}
height :: forall r p i. Length -> Attribute r p i
height =
    Internal.Height

{-| -}
row :: forall r p i. Array (Attribute r p i) -> Array (Element p i) -> Element p i
row attrs children =
    Internal.element
        Internal.asRow
        Internal.div_
        (Internal.htmlClasses
            [classes.contentLeft, classes.contentCenterY]
            : width shrink
            : height shrink
            : attrs
        )
        (Internal.Unkeyed children)

{-| -}
column :: forall r p i. Array (Attribute r p i) -> Array (Element p i) -> Element p i
column attrs children =
    Internal.element
        Internal.asColumn
        Internal.div_
        (Internal.htmlClasses
            [classes.contentTop, classes.contentLeft]
            : height shrink
            : width shrink
            : attrs
        )
        (Internal.Unkeyed children)

{-| When you want to render exactly nothing.
-}
none :: forall p i. Element p i
none =
    Internal.Empty

{-| Create some plain text.

    text "Hello, you stylish developer!"

**Note** text does not wrap by default. In order to get text to wrap, check out `paragraph`!

-}
text :: forall p i. String -> Element p i
text content =
    Internal.Text content

{-| The basic building block of your layout.

You can think of an `el` as a `div`, but it can only have one child.

If you want multiple children, you'll need to use something like `row` or `column`

    import Element exposing (Element, rgb)
    import Element.Background as Background
    import Element.Border as Border

    myElement : Element msg
    myElement =
        Element.el
            [ Background.color (rgb 0 0.5 0)
            , Border.color (rgb 0 0.7 0)
            ]
            (Element.text "You've made a stylish element!")

-}
el :: forall r p i. Array (Attribute r p i) -> Element p i -> Element p i
el attrs child =
    Internal.element
        Internal.asEl
        Internal.div_
        (width shrink
            : height shrink
            : attrs
        )
        (Internal.Unkeyed [ child ])

{-| Set the cursor to be a pointing hand when it's hovering over this element.
-}
pointer :: forall r p i. Attribute r p i
pointer =
    Internal.Class Flag.cursor (classes.cursorPointer)

{-| -}
scrollbars :: forall r p i. Attribute r p i
scrollbars =
    Internal.Class Flag.overflow (classes.scrollbars)


{-| -}
scrollbarY :: forall r p i. Attribute r p i
scrollbarY =
    Internal.Class Flag.overflow (classes.scrollbarsY)


{-| -}
scrollbarX :: forall r p i. Attribute r p i
scrollbarX =
    Internal.Class Flag.overflow (classes.scrollbarsX)

{-| -}
padding :: forall r p i. Int -> Attribute r p i
padding x =
    let
        f =
            toNumber x
    in
    Internal.StyleClass Flag.padding (Internal.PaddingStyle ("p-" <> Int.toStringAs Int.decimal x) f f f f)


{-| Set horizontal and vertical padding.
-}
paddingXY :: forall r p i. Int -> Int -> Attribute r p i
paddingXY x y =
    if x == y then
        let
            f =
                toNumber x
        in
        Internal.StyleClass Flag.padding (Internal.PaddingStyle ("p-" <> Int.toStringAs Int.decimal x) f f f f)

    else
        let
            xFloat =
                toNumber x

            yFloat =
                toNumber y
        in
        Internal.StyleClass Flag.padding
            (Internal.PaddingStyle
                ("p-" <> Int.toStringAs Int.decimal x <> "-" <> Int.toStringAs Int.decimal y)
                yFloat
                xFloat
                yFloat
                xFloat
            )


{-| If you find yourself defining unique paddings all the time, you might consider defining

    edges =
        { top = 0
        , right = 0
        , bottom = 0
        , left = 0
        }

And then just do

    paddingEach { edges | right = 5 }

-}
paddingEach :: forall r p i. { top :: Int, right :: Int, bottom :: Int, left :: Int } -> Attribute r p i
paddingEach { top, right, bottom, left } =
    if top == right && top == bottom && top == left then
        let
            topFloat =
                toNumber top
        in
        Internal.StyleClass Flag.padding
            (Internal.PaddingStyle ("p-" <> Int.toStringAs Int.decimal top)
                topFloat
                topFloat
                topFloat
                topFloat
            )

    else
        Internal.StyleClass Flag.padding
            (Internal.PaddingStyle
                (Internal.paddingName top right bottom left)
                (toNumber top)
                (toNumber right)
                (toNumber bottom)
                (toNumber left)
            )

{-| -}
centerX :: forall r p i. Attribute r p i
centerX =
    Internal.AlignX Internal.CenterX


{-| -}
centerY :: forall r p i. Attribute r p i
centerY =
    Internal.AlignY Internal.CenterY


{-| -}
alignTop :: forall r p i. Attribute r p i
alignTop =
    Internal.AlignY Internal.Top


{-| -}
alignBottom :: forall r p i. Attribute r p i
alignBottom =
    Internal.AlignY Internal.Bottom


{-| -}
alignLeft :: forall r p i. Attribute r p i
alignLeft =
    Internal.AlignX Internal.Left


{-| -}
alignRight :: forall r p i. Attribute r p i
alignRight =
    Internal.AlignX Internal.Right

{-| -}
spaceEvenly :: forall r p i. Attribute r p i
spaceEvenly =
    Internal.Class Flag.spacing (Style.classes.spaceEvenly)


{-| -}
spacing :: forall r p i. Int -> Attribute r p i
spacing x =
    Internal.StyleClass Flag.spacing (Internal.SpacingStyle (Internal.spacingName x x) x x)


{-| In the majority of cases you'll just need to use `spacing`, which will work as intended.

However for some layouts, like `textColumn`, you may want to set a different spacing for the x axis compared to the y axis.

-}
spacingXY :: forall r p i. Int -> Int -> Attribute r p i
spacingXY x y =
    Internal.StyleClass Flag.spacing (Internal.SpacingStyle (Internal.spacingName x y) x y)

{-| A paragraph will layout all children as wrapped, inline elements.

    import Element exposing (el, paragraph, text)
    import Element.Font as Font

    view =
        paragraph []
            [ text "lots of text ...."
            , el [ Font.bold ] (text "this is bold")
            , text "lots of text ...."
            ]

This is really useful when you want to markup text by having some parts be bold, or some be links, or whatever you so desire.

Also, if a child element has `alignLeft` or `alignRight`, then it will be moved to that side and the text will flow around it, (ah yes, `float` behavior).

This makes it particularly easy to do something like a [dropped capital](https://en.wikipedia.org/wiki/Initial).

    import Element exposing (alignLeft, el, padding, paragraph, text)
    import Element.Font as Font

    view =
        paragraph []
            [ el
                [ alignLeft
                , padding 5
                ]
                (text "S")
            , text "o much text ...."
            ]

Which will look something like

![A paragraph where the first letter is twice the height of the others](https://mdgriffith.gitbooks.io/style-elements/content/assets/Screen%20Shot%202017-08-25%20at%209.41.52%20PM.png)

**Note** `spacing` on a paragraph will set the pixel spacing between lines.

-}
paragraph :: forall r p i. Array (Attribute r p i) -> Array (Element p i) -> Element p i
paragraph attrs children =
    Internal.element
        Internal.asParagraph
        Internal.div_
        (Internal.Describe Internal.Paragraph
            : width fill
            : spacing 5
            : attrs
        )
        (Internal.Unkeyed children)

{-| -}
mouseOver :: forall r p i. Array (Decoration r) -> Attribute r p i
mouseOver decs =
    Internal.StyleClass Flag.hover $
        Internal.PseudoSelector Internal.Hover
            (Internal.unwrapDecorations decs)


{-| -}
mouseDown :: forall r p i. Array (Decoration r) -> Attribute r p i
mouseDown decs =
    Internal.StyleClass Flag.active $
        Internal.PseudoSelector Internal.Active
            (Internal.unwrapDecorations decs)


{-| -}
focused :: forall r p i. Array (Decoration r) -> Attribute r p i
focused decs =
    Internal.StyleClass Flag.focus $
        Internal.PseudoSelector Internal.Focus
            (Internal.unwrapDecorations decs)
