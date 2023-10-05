module Element where

import Prelude

import Data.List (List)
import Halogen.HTML.Core (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
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
type Attribute p i =
    Internal.Attribute Unit p i

{-| This is a special attribute that counts as both a `Attribute msg` and a `Decoration`.
-}
type Attr decorative p i =
    Internal.Attribute decorative p i

{-| Only decorations
-}
type Decoration =
    Internal.Attribute Void Void Void

{-| -}
html :: forall p i . HTML p i -> Element p i
html =
    Internal.unstyled

{-| -}
htmlAttribute :: forall r p i . HH.IProp r i -> Attribute p i
htmlAttribute =
    unwrap >>> Internal.Attr

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
layout :: forall p i . Array (Attribute p i) -> Element p i -> HH.HTML p i
layout =
    layoutWith { options: [] }

{-| -}
type Option =
    Internal.Option

{-| -}
layoutWith :: forall p i . { options :: Array Option } -> Array (Attribute p i) -> Element p i -> HH.HTML p i
layoutWith { options } attrs child =
    Internal.renderRoot options
        (Internal.htmlClasses
            [ classes.root
            , classes.any
            , classes.single
            ]
            `Array.cons` (Internal.rootStyle <> attrs)
        )
        child

{-| -}
width :: forall p i. Length -> Attribute p i
width =
    Internal.Width

{-| Highlight the borders of an element and it's children below. This can really help if you're running into some issue with your layout!

**Note** This attribute needs to be handed `Debug.todo` in order to work, even though it won't do anything with it. This is a safety measure so you don't accidently ship code with `explain` in it, as Elm won't compile with `--optimize` if you still have a `Debug` statement in your code.

    el
        [ Element.explain Debug.todo
        ]
        (text "Help, I'm being debugged!")

-}
explain :: forall p i. Attribute p i
explain =
    Internal.htmlClass (HH.ClassName "explain")

{-| -}
height :: forall p i. Length -> Attribute p i
height =
    Internal.Height

{-| -}
row :: forall p i. Array (Attribute p i) -> Array (Element p i) -> Element p i
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
column :: forall p i. Array (Attribute p i) -> Array (Element p i) -> Element p i
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
el :: forall p i. Array (Attribute p i) -> Element p i -> Element p i
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
pointer :: forall p i. Attribute p i
pointer =
    Internal.Class Flag.cursor (classes.cursorPointer)

{-| -}
scrollbars :: forall p i. Attribute p i
scrollbars =
    Internal.Class Flag.overflow (classes.scrollbars)


{-| -}
scrollbarY :: forall p i. Attribute p i
scrollbarY =
    Internal.Class Flag.overflow (classes.scrollbarsY)


{-| -}
scrollbarX :: forall p i. Attribute p i
scrollbarX =
    Internal.Class Flag.overflow (classes.scrollbarsX)

{-| -}
padding :: forall p i. Int -> Attribute p i
padding x =
    let
        f =
            toNumber x
    in
    Internal.StyleClass Flag.padding (Internal.PaddingStyle ("p-" <> Int.toStringAs Int.decimal x) f f f f)


{-| Set horizontal and vertical padding.
-}
paddingXY :: forall p i. Int -> Int -> Attribute p i
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
paddingEach :: forall p i. { top :: Int, right :: Int, bottom :: Int, left :: Int } -> Attribute p i
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
centerX :: forall p i. Attribute p i
centerX =
    Internal.AlignX Internal.CenterX


{-| -}
centerY :: forall p i. Attribute p i
centerY =
    Internal.AlignY Internal.CenterY


{-| -}
alignTop :: forall p i. Attribute p i
alignTop =
    Internal.AlignY Internal.Top


{-| -}
alignBottom :: forall p i. Attribute p i
alignBottom =
    Internal.AlignY Internal.Bottom


{-| -}
alignLeft :: forall p i. Attribute p i
alignLeft =
    Internal.AlignX Internal.Left


{-| -}
alignRight :: forall p i. Attribute p i
alignRight =
    Internal.AlignX Internal.Right

{-| -}
spaceEvenly :: forall p i. Attribute p i
spaceEvenly =
    Internal.Class Flag.spacing (Style.classes.spaceEvenly)


{-| -}
spacing :: forall p i. Int -> Attribute p i
spacing x =
    Internal.StyleClass Flag.spacing (Internal.SpacingStyle (Internal.spacingName x x) x x)


{-| In the majority of cases you'll just need to use `spacing`, which will work as intended.

However for some layouts, like `textColumn`, you may want to set a different spacing for the x axis compared to the y axis.

-}
spacingXY :: forall p i. Int -> Int -> Attribute p i
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
paragraph :: forall p i. Array (Attribute p i) -> Array (Element p i) -> Element p i
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
mouseOver :: forall p i. Array Decoration -> Attribute p i
mouseOver decs =
    Internal.StyleClass Flag.hover $
        Internal.PseudoSelector Internal.Hover
            (Internal.unwrapDecorations decs)


{-| -}
mouseDown :: forall p i. Array Decoration -> Attribute p i
mouseDown decs =
    Internal.StyleClass Flag.active $
        Internal.PseudoSelector Internal.Active
            (Internal.unwrapDecorations decs)


{-| -}
focused :: forall p i. Array Decoration -> Attribute p i
focused decs =
    Internal.StyleClass Flag.focus $
        Internal.PseudoSelector Internal.Focus
            (Internal.unwrapDecorations decs)

{-|

    link []
        { url = "http://fruits.com"
        , label = text "A link to my favorite fruit provider."
        }

-}
link :: forall p i.
    Array (Attribute p i)
    ->
        { url :: String
        , label :: Element p i
        }
    -> Element p i
link attrs { url, label } =
    Internal.element
        Internal.asEl
        (Internal.NodeName "a")
        (Internal.iprop' (HP.href url)
            : Internal.iprop' (HP.rel "noopener noreferrer")
            : width shrink
            : height shrink
            : Internal.htmlClasses [classes.contentCenterX, classes.contentCenterY, classes.link]
            : attrs
        )
        (Internal.Unkeyed [ label ])

{- NEARBYS -}


createNearby :: forall p i. Internal.Location -> Element p i -> Attribute p i
createNearby loc element =
    case element of
        Internal.Empty ->
            Internal.NoAttribute

        _ ->
            Internal.Nearby loc element


{-| -}
below :: forall p i. Element p i -> Attribute p i
below element =
    createNearby Internal.Below element


{-| -}
above :: forall p i. Element p i -> Attribute p i
above element =
    createNearby Internal.Above element


{-| -}
onRight :: forall p i. Element p i -> Attribute p i
onRight element =
    createNearby Internal.OnRight element


{-| -}
onLeft :: forall p i. Element p i -> Attribute p i
onLeft element =
    createNearby Internal.OnLeft element


{-| This will place an element in front of another.

**Note:** If you use this on a `layout` element, it will place the element as fixed to the viewport which can be useful for modals and overlays.

-}
inFront :: forall p i. Element p i -> Attribute p i
inFront element =
    createNearby Internal.InFront element


{-| This will place an element between the background and the content of an element.
-}
behindContent :: forall p i. Element p i -> Attribute p i
behindContent element =
    createNearby Internal.Behind element
