module Element.Internal.Model where

import Prelude

import Data.Maybe (Maybe(..))
import Data.List (List(..), (:))
import Halogen.HTML.Core (HTML)
import Halogen.HTML as HH
import Halogen.HTML (ElemName(..), PropName(..), ClassName(..), AttrName(..))
import Halogen.HTML.Properties as HP
import Halogen.VDom.DOM.Prop as VDomProp
import Halogen.Query.Input (Input)
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Int as Int
import Data.Int (toStringAs, decimal)
import Element.Internal.Style (classes, Selector(..))
import Halogen.HTML (ClassName)
import Data.Bifunctor (class Bifunctor, bimap, rmap)
import Element.Internal.Flag (Flag)
import Data.Foldable (foldl, foldr)
import Data.List as List
import Element.Internal.Flag as Flag
import Element.Internal.Style as EStyle
import Data.Number (round)
import Data.String as String
import Data.Tuple (fst, snd)
import Data.Maybe as Maybe
import Data.Set as Set
import Data.Set (Set)
import Data.Number.Format as NumberFormat
import Data.Foldable (maximum, minimum)
import Foreign (Foreign)
import Data.Newtype (wrap, unwrap, class Newtype)
import Foreign.Object as FO
import Partial.Unsafe (unsafeCrashWith)
import Element.Internal.Merginator (preProcess)
import Debug as Debug

data Element p i
    = Unstyled (LayoutContext -> HTML p i)
    | Styled
        { styles :: Array Style
        , html :: EmbedStyle -> LayoutContext -> HTML p i
        }
    | Text String
    | Empty


instance bifunctorElement :: Bifunctor Element where
    bimap f g el =
        case el of
            Styled styled ->
                Styled
                    { styles: styled.styles
                    , html: \add context -> bimap f g (styled.html add context)
                    }

            Unstyled html ->
                Unstyled (bimap f g <<< html)

            Text str ->
                Text str

            Empty ->
                Empty

instance functorElement :: Functor (Element p) where
    map = rmap

data EmbedStyle
    = NoStyleSheet
    | StaticRootAndDynamic OptionRecord (Array Style)
    | OnlyDynamic OptionRecord (Array Style)

type OptionRecord =
    { hover :: HoverSetting
    , focus :: FocusStyle
    , mode :: RenderMode
    }

data LayoutContext
    = AsRow
    | AsColumn
    | AsEl
    | AsGrid
    | AsParagraph
    | AsTextColumn

derive instance layoutContextEq :: Eq LayoutContext

data Aligned
    = Unaligned
    | Aligned (Maybe HAlign) (Maybe VAlign)


data HAlign
    = Left
    | CenterX
    | Right


data VAlign
    = Top
    | CenterY
    | Bottom

data Property
    = Property String String

type Adjustment =
    { capital :: Number
    , lowercase :: Number
    , baseline :: Number
    , descender :: Number
    }

data Font
    = Serif
    | SansSerif
    | Monospace
    | Typeface String
    | ImportFont String String
    | FontWith
        { name :: String
        , adjustment :: Maybe Adjustment
        , variants :: Array Variant
        }

data Color
    = Rgba Number Number Number Number

data Variant
    = VariantActive String
    | VariantOff String
    | VariantIndexed String Int

data Length
    = Px Int
    | Content
    | Fill Int
    | Min Int Length
    | Max Int Length

data Style
    = Style String (Array Property)
      --       class  prop   val
    | FontFamily String (Array Font)
    | FontSize Int
      -- classname, prop, value
    | Single Selector String String
    | Colored String String Color
    | SpacingStyle String Int Int
    | BorderWidth String Int Int Int Int
    | PaddingStyle String Number Number Number Number
    | GridTemplateStyle
        { spacing :: Tuple Length Length
        , columns :: Array Length
        , rows :: Array Length
        }
    | GridPosition
        { row :: Int
        , col :: Int
        , width :: Int
        , height :: Int
        }
    | Transform Transformation
    | PseudoSelector PseudoClass (Array Style)
    | Transparency String Number
    | Shadows String String

data XYZ =
    XYZ Number Number Number

newtype Angle = Angle Number

derive instance angleNewtype :: Newtype Angle _

data Transformation
    = Untransformed
    | Moved XYZ
      --              translate, scale, rotate
    | FullTransform XYZ XYZ XYZ Angle

data RenderMode
    = Layout
    | NoStaticStyleSheet
    | WithVirtualCss

data HoverSetting
    = NoHover
    | AllowHover
    | ForceHover

data PseudoClass
    = Focus
    | Hover
    | Active

type FocusStyle =
    { borderColor :: Maybe Color
    , shadow :: Maybe Shadow
    , backgroundColor :: Maybe Color
    }

type Shadow =
    { color :: Color
    , offset :: Tuple Int Int
    , blur :: Int
    , size :: Int
    }

data Description
    = Main
    | Navigation
      -- | Search
    | ContentInfo
    | Complementary
    | Heading Int
    | Label String
    | LivePolite
    | LiveAssertive
    | Button
    | Paragraph

type VDomProp' i = VDomProp.Prop (Input i)

data Attribute aligned p i
    = NoAttribute
    | Attr (VDomProp' i)
    | Describe Description
      -- invalidation key and literal class
    | Class Flag ClassName
      -- invalidation key "border-color" as opposed to "border-color-10-10-10" that will be the key for the class
    | StyleClass Flag Style
    | AlignY VAlign
    | AlignX HAlign
    | Width Length
    | Height Length
    | Nearby Location (Element p i)
    | TransformComponent Flag TransformComponent

instance bifunctorAttribute :: Bifunctor (Attribute aligned) where
    bimap fnf fng attr =
        case attr of
            NoAttribute ->
                NoAttribute

            Describe describe ->
                Describe describe

            AlignX x ->
                AlignX x

            AlignY y ->
                AlignY y

            Width x ->
                Width x

            Height x ->
                Height x

            Class x y ->
                Class x y

            StyleClass flag style ->
                StyleClass flag style

            Nearby location elem ->
                Nearby location (bimap fnf fng elem)

            Attr htmlAttr ->
                Attr (map (map fng) htmlAttr)

            TransformComponent fl trans ->
                TransformComponent fl trans

instance functorAttribute :: Functor (Attribute aligned p) where
    map = rmap

data Location
    = Above
    | Below
    | OnRight
    | OnLeft
    | InFront
    | Behind

data TransformComponent
    = MoveX Number
    | MoveY Number
    | MoveZ Number
    | MoveXYZ XYZ
    | Rotate XYZ Number
    | Scale XYZ

data Option
    = HoverOption HoverSetting
    | FocusStyleOption FocusStyle
    | RenderModeOption RenderMode

always :: forall a b . a -> b -> a
always a _ = a

unstyled :: forall p i . HTML p i -> Element p i
unstyled =
    Unstyled <<< always

textElementClasses :: Array ClassName
textElementClasses =
    [classes.any, classes.text, classes.widthContent, classes.heightContent]

textElement :: forall p i . String -> HTML p i
textElement str =
    HH.div
        [ HP.classes textElementClasses
        ]
        [ HH.text str ]

textElementFillClasses :: Array ClassName
textElementFillClasses =
    [ classes.any
    , classes.text
    , classes.widthFill
    , classes.heightFill
    ]

formatColorClass :: Color -> String
formatColorClass (Rgba red green blue alpha) =
    floatClass red
        <> "-"
        <> floatClass green
        <> "-"
        <> floatClass blue
        <> "-"
        <> floatClass alpha

rootStyle :: forall aligned p i. Array (Attribute aligned p i)
rootStyle =
    let
        families =
            [ Typeface "Open Sans"
            , Typeface "Helvetica"
            , Typeface "Verdana"
            , SansSerif
            ]
    in
    [ StyleClass Flag.bgColor (Colored ("bg-" <> formatColorClass (Rgba 1.0 1.0 1.0 0.0)) "background-color" (Rgba 1.0 1.0 1.0 0.0))
    , StyleClass Flag.fontColor (Colored ("fc-" <> formatColorClass (Rgba 0.0 0.0 0.0 1.0)) "color" (Rgba 0.0 0.0 0.0 1.0))
    , StyleClass Flag.fontSize (FontSize 20)
    , StyleClass Flag.fontFamily $
        FontFamily (elmFoldlArray renderFontClassName "font-" families)
            families
    ]

div_ = Generic

textShadowClass shadow =
    String.joinWith ""
        [ "txt"
        , floatClass (fst shadow.offset) <> "px"
        , floatClass (snd shadow.offset) <> "px"
        , floatClass shadow.blur <> "px"
        , formatColorClass shadow.color
        ]

formatTextShadow shadow =
    String.joinWith " "
        [ NumberFormat.toString (fst shadow.offset) <> "px"
        , NumberFormat.toString (snd shadow.offset) <> "px"
        , NumberFormat.toString shadow.blur <> "px"
        , formatColor shadow.color
        ]

spacingName x y =
    "spacing-" <> toStringAs decimal x <> "-" <> toStringAs decimal y


paddingName top right bottom left =
    "pad-"
        <> toStringAs decimal top
        <> "-"
        <> toStringAs decimal right
        <> "-"
        <> toStringAs decimal bottom
        <> "-"
        <> toStringAs decimal left


paddingNameFloat top right bottom left =
    "pad-"
        <> floatClass top
        <> "-"
        <> floatClass right
        <> "-"
        <> floatClass bottom
        <> "-"
        <> floatClass left

boxShadowClass shadow =
    String.joinWith "" $
        [ if shadow.inset then
            "box-inset"

          else
            "box-"
        , floatClass (fst shadow.offset) <> "px"
        , floatClass (snd shadow.offset) <> "px"
        , floatClass shadow.blur <> "px"
        , floatClass shadow.size <> "px"
        , formatColorClass shadow.color
        ]

asGrid :: LayoutContext
asGrid =
    AsGrid


asRow :: LayoutContext
asRow =
    AsRow


asColumn :: LayoutContext
asColumn =
    AsColumn


asEl :: LayoutContext
asEl =
    AsEl


asParagraph :: LayoutContext
asParagraph =
    AsParagraph


asTextColumn :: LayoutContext
asTextColumn =
    AsTextColumn

renderFontClassName :: Font -> String -> String
renderFontClassName font current =
    current
        <> (case font of
                Serif ->
                    "serif"

                SansSerif ->
                    "sans-serif"

                Monospace ->
                    "monospace"

                Typeface name ->
                    name
                        # String.toLower
                        # String.split (String.Pattern " ")
                        # String.joinWith "-"

                ImportFont name url ->
                    name
                        # String.toLower
                        # String.split (String.Pattern " ")
                        # String.joinWith "-"

                FontWith { name } ->
                    name
                        # String.toLower
                        # String.split (String.Pattern " ")
                        # String.joinWith "-"
           )

textElementFill :: forall p i . String -> HH.HTML p i
textElementFill str =
    HH.div
        [ HP.classes textElementFillClasses
        ]
        [ HH.text str ]

toHTML :: forall p i .(Array Style -> EmbedStyle) -> Element p i -> HTML p i
toHTML mode element =
    case element of
        Unstyled html ->
            html AsEl

        Styled { styles, html } ->
            html (mode styles) AsEl

        Text text ->
            textElement text

        Empty ->
            textElement ""

focusDefaultStyle :: { backgroundColor :: Maybe Color, borderColor :: Maybe Color, shadow :: Maybe Shadow }
focusDefaultStyle =
    { backgroundColor: Nothing
    , borderColor: Nothing
    , shadow:
        Just
            { color:
                Rgba (155.0 / 255.0) (203.0 / 255.0) 1.0 1.0
            , offset: Tuple 0 0
            , blur: 0
            , size: 3
            }
    }

optionsToRecord :: Array Option -> OptionRecord
optionsToRecord options =
    let
        combine opt record =
            case opt of
                HoverOption hoverable ->
                    case record.hover of
                        Nothing ->
                            record { hover = Just hoverable }

                        _ ->
                            record

                FocusStyleOption focusStyle ->
                    case record.focus of
                        Nothing ->
                            record { focus = Just focusStyle }

                        _ ->
                            record

                RenderModeOption renderMode ->
                    case record.mode of
                        Nothing ->
                            record { mode = Just renderMode }

                        _ ->
                            record

        andFinally record =
            { hover:
                case record.hover of
                    Nothing ->
                        AllowHover

                    Just hoverable ->
                        hoverable
            , focus:
                case record.focus of
                    Nothing ->
                        focusDefaultStyle

                    Just focusable ->
                        focusable
            , mode:
                case record.mode of
                    Nothing ->
                        Layout

                    Just actualMode ->
                        actualMode
            }
    in
    foldr combine
        { hover: Nothing
        , focus: Nothing
        , mode: Nothing
        }
        options
    # andFinally

{-| -}
renderRoot :: forall aligned p i . Array Option -> Array (Attribute aligned p i) -> Element p i -> HTML p i
renderRoot optionArray attributes child =
    let
        options =
            optionsToRecord optionArray

        embedStyle =
            case options.mode of
                NoStaticStyleSheet ->
                    OnlyDynamic options

                _ ->
                    StaticRootAndDynamic options
    in
    element AsEl Generic attributes (Unkeyed [ child ])
        # toHTML embedStyle

data NodeName
    = Generic
    | NodeName String
    | Embedded String String

data Children x
    = Unkeyed (Array x)
    | Keyed (Array (Tuple String x))


addKeyedChildren :: forall a p i . a -> Array (Tuple a (HTML p i)) -> NearbyChildren p i -> Array (Tuple a (HTML p i))
addKeyedChildren key existing nearbyChildren =
    case nearbyChildren of
        NoNearbyChildren ->
            existing

        ChildrenBehind behind ->
            map (\x -> (Tuple key x)) behind <> existing

        ChildrenInFront inFront ->
            existing <> map (\x -> (Tuple key x)) inFront

        ChildrenBehindAndInFront behind inFront ->
            map (\x -> (Tuple key x)) behind
                <> existing
                <> map (\x -> (Tuple key x)) inFront

rowClass = [classes.any, classes.row]
columnClass = [classes.any, classes.column]
singleClass = [classes.any, classes.single]
gridClass = [classes.any, classes.grid]
paragraphClass = [classes.any, classes.paragraph]
pageClass = [classes.any, classes.page]

iprop' :: forall aligned r p i. HP.IProp r i -> Attribute aligned p i
iprop' x = Attr (unwrap x)

htmlClass :: forall aligned p i. ClassName -> Attribute aligned p i
htmlClass cls =
    Attr (unwrap (HP.class_ cls))

htmlClasses :: forall aligned p i. Array ClassName -> Attribute aligned p i
htmlClasses cls =
    Attr (unwrap (HP.classes cls))

contextClasses context =
    case context of
        AsRow ->
            rowClass

        AsColumn ->
            columnClass

        AsEl ->
            singleClass

        AsGrid ->
            gridClass

        AsParagraph ->
            paragraphClass

        AsTextColumn ->
            pageClass

element :: forall aligned p i . LayoutContext -> NodeName -> Array (Attribute aligned p i) -> Children (Element p i) -> Element p i
element context node attributes children =
    attributes
        # List.fromFoldable
        # List.reverse
        # gatherAttrRecursive (contextClasses context) node Flag.none Untransformed [] Nil NoNearbyChildren
        # createElement context children

addChildren :: forall p i . Array (HTML p i) -> NearbyChildren p i -> Array (HTML p i)
addChildren existing nearbyChildren =
    case nearbyChildren of
        NoNearbyChildren ->
            existing

        ChildrenBehind behind ->
            behind <> existing

        ChildrenInFront inFront ->
            existing <> inFront

        ChildrenBehindAndInFront behind inFront ->
            behind <> existing <> inFront

createElement :: forall p i . LayoutContext -> Children (Element p i) -> Gathered p i -> Element p i
createElement context children rendered =
    let
        gather child (Tuple htmls existingStyles) =
            case child of
                Unstyled html ->
                    if context == AsParagraph then
                        Tuple
                            (html context `Array.cons` htmls)
                            existingStyles

                    else
                        Tuple
                            (html context `Array.cons` htmls)
                            existingStyles

                Styled styled ->
                    if context == AsParagraph then
                        Tuple
                            (styled.html NoStyleSheet context `Array.cons` htmls)
                            (if Array.null existingStyles then
                                styled.styles

                            else
                                styled.styles <> existingStyles)

                    else
                        Tuple
                            (styled.html NoStyleSheet context `Array.cons` htmls)
                            (if Array.null existingStyles then
                                styled.styles
                            else
                                styled.styles <> existingStyles)

                Text str ->
                    -- TEXT OPTIMIZATION
                    -- You can have raw text if the element is an el, and has `width-content` and `height-content`
                    -- Same if it's a column or row with one child and width-content, height-content
                    -- interferes with css grid
                    -- Maybe we could unpack text elements in a paragraph as well,
                    -- however, embedded elements that are larger than the line height will overlap with exisitng text.
                    -- I don't think that's what we want.
                    -- if
                    --     context
                    --         == asEl
                    --         || context
                    --         == asParagraph
                    -- then
                    --     ( VirtualDom.text
                    --         (if context == asParagraph then
                    --             str
                    --          else
                    --             str
                    --         )
                    --         :: htmls
                    --     , existingStyles
                    --     )
                    -- else
                    (Tuple ((if context == AsEl then
                                            textElementFill str
                    
                                           else
                                            textElement str
                                          )
                                            `Array.cons` htmls)
                     existingStyles
                    )

                Empty ->
                    (Tuple htmls existingStyles)

        gatherKeyed (Tuple key child) (Tuple htmls existingStyles) =
            case child of
                Unstyled html ->
                    if context == AsParagraph then
                        ( Tuple ((Tuple key (html context)) `Array.cons` htmls)
                          existingStyles
                        )

                    else
                        ( Tuple ((Tuple key (html context) ) `Array.cons` htmls)
                          existingStyles
                        )

                Styled styled ->
                    if context == AsParagraph then
                        ( Tuple ((Tuple key (styled.html NoStyleSheet context))
                                                    `Array.cons` htmls)
                          (if Array.null existingStyles then
                                                    styled.styles
                        
                                                  else
                                                    styled.styles <> existingStyles)
                        )

                    else
                        ( Tuple (( Tuple key (styled.html NoStyleSheet context) ) `Array.cons` htmls)
                          (if Array.null existingStyles then
                                                    styled.styles
                        
                                                  else
                                                    styled.styles <> existingStyles)
                        )

                Text str ->
                    -- TEXT OPTIMIZATION
                    -- You can have raw text if the element is an el, and has `width-content` and `height-content`
                    -- Same if it's a column or row with one child and width-content, height-content
                    -- if
                    --     context
                    --         == asEl
                    --         || context
                    --         == asParagraph
                    -- then
                    --     ( ( key
                    --       , VirtualDom.text
                    --             str
                    --       )
                    --         :: htmls
                    --     , existingStyles
                    --     )
                    -- else
                    (Tuple ((Tuple key
                                            (if context == AsEl then
                                                                        textElementFill str
                                            
                                                                    else
                                                                        textElement str)
                                          )
                                            `Array.cons` htmls)
                     existingStyles
                    )

                Empty ->
                    (Tuple htmls existingStyles)
    in
    case children of
        Keyed keyedChildren ->
            case Array.foldr gatherKeyed (Tuple [] []) keyedChildren of
                (Tuple keyed styles) ->
                    let
                        newStyles =
                            if Array.null styles then
                                rendered.styles

                            else
                                rendered.styles <> styles
                    in
                    case newStyles of
                        [] ->
                            Unstyled
                                (finalizeNode rendered.has
                                    rendered.node
                                    rendered.attributes
                                    (Keyed
                                        (addKeyedChildren "nearby-element-pls" keyed rendered.children)
                                    )
                                    NoStyleSheet
                                )

                        allStyles ->
                            Styled
                                { styles: allStyles
                                , html:
                                    finalizeNode
                                        rendered.has
                                        rendered.node
                                        rendered.attributes
                                        (Keyed
                                            (addKeyedChildren "nearby-element-pls" keyed rendered.children)
                                        )
                                }

        Unkeyed unkeyedChildren ->
            case Array.foldr gather (Tuple [] []) unkeyedChildren of
                (Tuple unkeyed styles) ->
                    let
                        newStyles =
                            if Array.null styles then
                                rendered.styles

                            else
                                rendered.styles <> styles
                    in
                    case newStyles of
                        [] ->
                            Unstyled
                                (finalizeNode
                                    rendered.has
                                    rendered.node
                                    rendered.attributes
                                    (Unkeyed (addChildren unkeyed rendered.children))
                                    NoStyleSheet
                                )

                        allStyles ->
                            Styled
                                { styles: allStyles
                                , html:
                                    finalizeNode
                                        rendered.has
                                        rendered.node
                                        rendered.attributes
                                        (Unkeyed (addChildren unkeyed rendered.children))
                                }

reduceStylesRecursive :: Set String -> Array Style -> Array Style -> Array Style
reduceStylesRecursive cache found styles =
    reduceStylesRecursiveInternal cache (List.fromFoldable found) (List.fromFoldable styles)
    # Array.fromFoldable

reduceStylesRecursiveInternal :: Set String -> List Style -> List Style -> List Style
reduceStylesRecursiveInternal cache found styles =
    case styles of
        Nil ->
            found

        (Cons head remaining) ->
            let
                styleName =
                    getStyleName head
            in
            if Set.member styleName cache then
                reduceStylesRecursiveInternal cache found remaining

            else
                reduceStylesRecursiveInternal (Set.insert styleName cache) (head : found) remaining


reduceStyles :: Style -> (Tuple (Set String) (Array Style)) -> (Tuple (Set String) (Array Style))
reduceStyles style (nevermind @ (Tuple cache existing)) =
    let
        styleName =
            getStyleName style
    in
    if Set.member styleName cache then
        nevermind

    else
        Tuple
            (Set.insert styleName cache)
            (style `Array.cons` existing)

elmFoldl :: forall a b. (a -> b -> b) -> b -> List a -> b
elmFoldl func acc list =
  case list of
    Nil ->
      acc

    (Cons x xs) ->
      elmFoldl func (func x acc) xs

elmFoldlArray func acc list = elmFoldl func acc (List.fromFoldable list)

renderStyle :: OptionRecord -> Maybe PseudoClass -> String -> Array Property -> Array String
renderStyle options maybePseudo selector props =
    case maybePseudo of
        Nothing ->
            [ selector <> "{" <> elmFoldlArray (renderProps false) "" props <> "\n}" ]

        Just pseudo ->
            case pseudo of
                Hover ->
                    case options.hover of
                        NoHover ->
                            []

                        ForceHover ->
                            [ selector <> "-hv {" <> elmFoldlArray (renderProps true) "" props <> "\n}" ]

                        AllowHover ->
                            [ selector <> "-hv:hover {" <> elmFoldlArray (renderProps false) "" props <> "\n}" ]

                Focus ->
                    let
                        renderedProps =
                            elmFoldlArray (renderProps false) "" props
                    in
                    [ selector <> "-fs:focus {" <> renderedProps <> "\n}"
                    , ("." <> unwrap classes.any <> ":focus " <> selector <> "-fs  {")
                        <> renderedProps
                        <> "\n}"
                    , (selector <> "-fs:focus-within {")
                        <> renderedProps
                        <> "\n}"
                    , (".ui-slide-bar:focus + " <> unwrap (EStyle.dot classes.any) <> " .focusable-thumb" <> selector <> "-fs {")
                        <> renderedProps
                        <> "\n}"
                    ]

                Active ->
                    [ selector <> "-act:active {" <> elmFoldlArray (renderProps false) "" props <> "\n}" ]

renderProps :: Boolean -> Property -> String -> String
renderProps force (Property key val) existing =
    if force then
        existing <> "\n  " <> key <> ": " <> val <> " !important;"

    else
        existing <> "\n  " <> key <> ": " <> val <> ";"

variantName var =
    case var of
        VariantActive name ->
            name

        VariantOff name ->
            name <> "-0"

        VariantIndexed name index ->
            name <> "-" <> toStringAs decimal index

renderVariant var =
    case var of
        VariantActive name ->
            "\"" <> name <> "\""

        VariantOff name ->
            "\"" <> name <> "\" 0"

        VariantIndexed name index ->
            "\"" <> name <> "\" " <> toStringAs decimal index

renderVariants typeface =
    case typeface of
        FontWith font ->
            Just (String.joinWith ", " (map renderVariant font.variants))

        _ ->
            Nothing

fontName font =
    case font of
        Serif ->
            "serif"

        SansSerif ->
            "sans-serif"

        Monospace ->
            "monospace"

        Typeface name ->
            "\"" <> name <> "\""

        ImportFont name _ ->
            "\"" <> name <> "\""

        FontWith { name } ->
            "\"" <> name <> "\""

isSmallCaps var =
    case var of
        VariantActive name ->
            name == "smcp"

        VariantOff name ->
            false

        VariantIndexed name index ->
            name == "smcp" && index == 1


hasSmallCaps typeface =
    case typeface of
        FontWith font ->
            Array.any isSmallCaps font.variants

        _ ->
            false

renderStyleRule :: OptionRecord -> Style -> Maybe PseudoClass -> Array String
renderStyleRule options rule maybePseudo =
    case rule of
        Style selector props ->
            renderStyle options maybePseudo selector props

        Shadows name prop ->
            renderStyle options
                maybePseudo
                ("." <> name)
                [ Property "box-shadow" prop
                ]

        Transparency name transparency ->
            let
                opacity =
                    (1.0 - transparency)
                        # min 1.0
                        # max 0.0
            in
            renderStyle options
                maybePseudo
                ("." <> name)
                [ Property "opacity" (NumberFormat.toString opacity)
                ]

        FontSize i ->
            renderStyle options
                maybePseudo
                (".font-size-" <> toStringAs decimal i)
                [ Property "font-size" (toStringAs decimal i <> "px")
                ]

        FontFamily name typefaces ->
            let
                features =
                    typefaces
                        # Array.mapMaybe renderVariants
                        # String.joinWith ", "

                families =
                    [ Property "font-family"
                        (typefaces
                            # map fontName
                            # String.joinWith ", "
                        )
                    , Property "font-feature-settings" features
                    , Property "font-variant"
                        (if Array.any hasSmallCaps typefaces then
                            "small-caps"

                         else
                            "normal"
                        )
                    ]
            in
            renderStyle options
                maybePseudo
                ("." <> name)
                families

        Single klass prop val ->
            renderStyle options
                maybePseudo
                (unwrap (EStyle.dot' klass))
                [ Property prop val
                ]

        Colored klass prop color ->
            renderStyle options
                maybePseudo
                ("." <> klass)
                [ Property prop (formatColor color)
                ]

        SpacingStyle cls x y ->
            let
                klass =
                    "." <> cls

                halfX =
                    toStringAs decimal (x / 2) <> "px"

                halfY =
                    toStringAs decimal (y / 2) <> "px"

                xPx =
                    toStringAs decimal x <> "px"

                yPx =
                    toStringAs decimal y <> "px"

                row =
                    "." <> unwrap EStyle.classes.row

                wrappedRow =
                    "." <> unwrap EStyle.classes.wrapped <> row

                column =
                    "." <> unwrap EStyle.classes.column

                page =
                    "." <> unwrap EStyle.classes.page

                paragraph =
                    "." <> unwrap EStyle.classes.paragraph

                left =
                    "." <> unwrap EStyle.classes.alignLeft

                right =
                    "." <> unwrap EStyle.classes.alignRight

                any =
                    "." <> unwrap EStyle.classes.any

                single =
                    "." <> unwrap EStyle.classes.single
            in
            Array.concat
                [ renderStyle options maybePseudo (klass <> row <> " > " <> any <> " + " <> any) [ Property "margin-left" xPx ]

                -- margins don't apply to last element of normal, unwrapped rows
                -- , renderStyle options maybePseudo (class <> row <> " > " <> any <> ":first-child") [ Property "margin" "0" ]
                -- For wrapped rows, margins always apply because we handle "canceling out" the other margins manually in the element.
                , renderStyle options
                    maybePseudo
                    (klass <> wrappedRow <> " > " <> any)
                    [ Property "margin" (halfY <> " " <> halfX)
                    ]

                -- , renderStyle options maybePseudo
                --     (class <> wrappedRow <> " > " <> any <> ":last-child")
                --     [ Property "margin-right" "0"
                --     ]
                -- columns
                , renderStyle options maybePseudo (klass <> column <> " > " <> any <> " + " <> any) [ Property "margin-top" yPx ]
                , renderStyle options maybePseudo (klass <> page <> " > " <> any <> " + " <> any) [ Property "margin-top" yPx ]
                , renderStyle options maybePseudo (klass <> page <> " > " <> left) [ Property "margin-right" xPx ]
                , renderStyle options maybePseudo (klass <> page <> " > " <> right) [ Property "margin-left" xPx ]
                , renderStyle options
                    maybePseudo
                    (klass <> paragraph)
                    [ Property "line-height" ("calc(1em + " <> toStringAs decimal y <> "px)")
                    ]
                , renderStyle options
                    maybePseudo
                    ("textarea" <> any <> klass)
                    [ Property "line-height" ("calc(1em + " <> toStringAs decimal y <> "px)")
                    , Property "height" ("calc(100% + " <> toStringAs decimal y <> "px)")
                    ]

                -- , renderStyle options
                --     maybePseudo
                --     (class <> paragraph <> " > " <> any)
                --     [ Property "margin-right" xPx
                --     , Property "margin-bottom" yPx
                --     ]
                , renderStyle options
                    maybePseudo
                    (klass <> paragraph <> " > " <> left)
                    [ Property "margin-right" xPx
                    ]
                , renderStyle options
                    maybePseudo
                    (klass <> paragraph <> " > " <> right)
                    [ Property "margin-left" xPx
                    ]
                , renderStyle options
                    maybePseudo
                    (klass <> paragraph <> "::after")
                    [ Property "content" "''"
                    , Property "display" "block"
                    , Property "height" "0"
                    , Property "width" "0"
                    , Property "margin-top" (toStringAs decimal (-1 * (y / 2)) <> "px")
                    ]
                , renderStyle options
                    maybePseudo
                    (klass <> paragraph <> "::before")
                    [ Property "content" "''"
                    , Property "display" "block"
                    , Property "height" "0"
                    , Property "width" "0"
                    , Property "margin-bottom" (toStringAs decimal (-1 * (y / 2)) <> "px")
                    ]
                ]

        PaddingStyle cls top right bottom left ->
            let
                klass =
                    "."
                        <> cls
            in
            renderStyle options
                maybePseudo
                klass
                [ Property "padding"
                    (NumberFormat.toString top
                        <> "px "
                        <> NumberFormat.toString right
                        <> "px "
                        <> NumberFormat.toString bottom
                        <> "px "
                        <> NumberFormat.toString left
                        <> "px"
                    )
                ]

        BorderWidth cls top right bottom left ->
            let
                klass =
                    "."
                        <> cls
            in
            renderStyle options
                maybePseudo
                klass
                [ Property "border-width"
                    (toStringAs decimal top
                        <> "px "
                        <> toStringAs decimal right
                        <> "px "
                        <> toStringAs decimal bottom
                        <> "px "
                        <> toStringAs decimal left
                        <> "px"
                    )
                ]

        GridTemplateStyle template ->
            let
                klass =
                    ".grid-rows-"
                        <> String.joinWith "-" (map lengthClassName template.rows)
                        <> "-cols-"
                        <> String.joinWith "-" (map lengthClassName template.columns)
                        <> "-space-x-"
                        <> lengthClassName (fst template.spacing)
                        <> "-space-y-"
                        <> lengthClassName (snd template.spacing)

                ySpacing =
                    toGridLength (snd template.spacing)

                xSpacing =
                    toGridLength (fst template.spacing)

                toGridLength x =
                    toGridLengthHelper Nothing Nothing x

                toGridLengthHelper minimum maximum x =
                    case x of
                        Px px ->
                            toStringAs decimal px <> "px"

                        Content ->
                            case (Tuple minimum maximum) of
                                (Tuple Nothing Nothing) ->
                                    "max-content"

                                (Tuple (Just minSize) Nothing) ->
                                    "minmax(" <> toStringAs decimal minSize <> "px, " <> "max-content)"

                                (Tuple Nothing (Just maxSize)) ->
                                    "minmax(max-content, " <> toStringAs decimal maxSize <> "px)"

                                (Tuple (Just minSize) (Just maxSize)) ->
                                    "minmax(" <> toStringAs decimal minSize <> "px, " <> toStringAs decimal maxSize <> "px)"

                        Fill i ->
                            case (Tuple minimum maximum) of
                                (Tuple Nothing Nothing) ->
                                    toStringAs decimal i <> "fr"

                                (Tuple (Just minSize) Nothing) ->
                                    "minmax(" <> toStringAs decimal minSize <> "px, " <> toStringAs decimal i <> "fr" <> "fr)"

                                (Tuple Nothing (Just maxSize)) ->
                                    "minmax(max-content, " <> toStringAs decimal maxSize <> "px)"

                                (Tuple (Just minSize) (Just maxSize)) ->
                                    "minmax(" <> toStringAs decimal minSize <> "px, " <> toStringAs decimal maxSize <> "px)"

                        Min m len ->
                            toGridLengthHelper (Just m) maximum len

                        Max m len ->
                            toGridLengthHelper minimum (Just m) len

                msColumns =
                    template.columns
                        # map toGridLength
                        # String.joinWith ySpacing
                        # (\x -> "-ms-grid-columns: " <> x <> ";")

                msRows =
                    template.columns
                        # map toGridLength
                        # String.joinWith ySpacing
                        # (\x -> "-ms-grid-rows: " <> x <> ";")

                base =
                    klass <> "{" <> msColumns <> msRows <> "}"

                columns =
                    template.columns
                        # map toGridLength
                        # String.joinWith " "
                        # (\x -> "grid-template-columns: " <> x <> ";")

                rows =
                    template.rows
                        # map toGridLength
                        # String.joinWith " "
                        # (\x -> "grid-template-rows: " <> x <> ";")

                gapX =
                    "grid-column-gap:" <> toGridLength (fst template.spacing) <> ";"

                gapY =
                    "grid-row-gap:" <> toGridLength (snd template.spacing) <> ";"

                modernGrid =
                    klass <> "{" <> columns <> rows <> gapX <> gapY <> "}"

                supports =
                    "@supports (display:grid) {" <> modernGrid <> "}"
            in
            [ base
            , supports
            ]

        GridPosition position ->
            let
                klass =
                    ".grid-pos-"
                        <> toStringAs decimal position.row
                        <> "-"
                        <> toStringAs decimal position.col
                        <> "-"
                        <> toStringAs decimal position.width
                        <> "-"
                        <> toStringAs decimal position.height

                msPosition =
                    String.joinWith " "
                        [ "-ms-grid-row: "
                            <> toStringAs decimal position.row
                            <> ";"
                        , "-ms-grid-row-span: "
                            <> toStringAs decimal position.height
                            <> ";"
                        , "-ms-grid-column: "
                            <> toStringAs decimal position.col
                            <> ";"
                        , "-ms-grid-column-span: "
                            <> toStringAs decimal position.width
                            <> ";"
                        ]

                base =
                    klass <> "{" <> msPosition <> "}"

                modernPosition =
                    String.joinWith " "
                        [ "grid-row: "
                            <> toStringAs decimal position.row
                            <> " / "
                            <> toStringAs decimal (position.row + position.height)
                            <> ";"
                        , "grid-column: "
                            <> toStringAs decimal position.col
                            <> " / "
                            <> toStringAs decimal (position.col + position.width)
                            <> ";"
                        ]

                modernGrid =
                    klass <> "{" <> modernPosition <> "}"

                supports =
                    "@supports (display:grid) {" <> modernGrid <> "}"
            in
            [ base
            , supports
            ]

        PseudoSelector klass styles ->
            let
                renderPseudoRule style =
                    renderStyleRule options style (Just klass)
            in
            Array.concatMap renderPseudoRule styles

        Transform transform ->
            let
                val =
                    transformValue transform

                klass =
                    transformClass transform
            in
            case (Tuple klass val) of
                (Tuple (Just cls) (Just v)) ->
                    renderStyle options
                        maybePseudo
                        ("." <> unwrap cls)
                        [ Property "transform"
                            v
                        ]

                _ ->
                    []

transformValue transform =
    case transform of
        Untransformed ->
            Nothing

        Moved (XYZ x y z) ->
            Just $
                "translate3d("
                    <> NumberFormat.toString x
                    <> "px, "
                    <> NumberFormat.toString y
                    <> "px, "
                    <> NumberFormat.toString z
                    <> "px)"

        FullTransform (XYZ tx ty tz) (XYZ sx sy sz) (XYZ ox oy oz) (Angle angle) ->
            let
                translate =
                    "translate3d("
                        <> NumberFormat.toString tx
                        <> "px, "
                        <> NumberFormat.toString ty
                        <> "px, "
                        <> NumberFormat.toString tz
                        <> "px)"

                scale =
                    "scale3d("
                        <> NumberFormat.toString sx
                        <> ", "
                        <> NumberFormat.toString sy
                        <> ", "
                        <> NumberFormat.toString sz
                        <> ")"

                rotate =
                    "rotate3d("
                        <> NumberFormat.toString ox
                        <> ", "
                        <> NumberFormat.toString oy
                        <> ", "
                        <> NumberFormat.toString oz
                        <> ", "
                        <> NumberFormat.toString angle
                        <> "rad)"
            in
            Just $ translate <> " " <> scale <> " " <> rotate

topLevelValue rule =
    case rule of
        FontFamily name typefaces ->
            Just (Tuple name typefaces)

        _ ->
            Nothing

fontAdjustmentRules converted =
    (Tuple [ (Tuple "display" "block")
      ]
      [ (Tuple "display" "inline-block")
      , (Tuple "line-height" (NumberFormat.toString converted.height))
      , (Tuple "vertical-align" (NumberFormat.toString converted.vertical <> "em"))
      , (Tuple "font-size" (NumberFormat.toString converted.size <> "em"))
      ]
    )

typefaceAdjustment typefaces =
    elmFoldlArray
        (\face found ->
            case found of
                Nothing ->
                    case face of
                        FontWith with ->
                            case with.adjustment of
                                Nothing ->
                                    found

                                Just adjustment ->
                                    Just
                                        (Tuple (fontAdjustmentRules
                                                                                    (_.full (convertAdjustment adjustment)))
                                         (fontAdjustmentRules
                                                                                    (_.capital (convertAdjustment adjustment)))
                                        )

                        _ ->
                            found

                Just _ ->
                    found
        )
        Nothing
        typefaces

{- Font Adjustments -}


convertAdjustment adjustment =
    let
        lineHeight =
            1.5

        base =
            lineHeight

        normalDescender =
            (lineHeight - 1.0)
                / 2.0

        oldMiddle =
            lineHeight / 2.0

        newCapitalMiddle =
            ((ascender - newBaseline) / 2.0) + newBaseline

        newFullMiddle =
            ((ascender - descender) / 2.0) + descender

        lines :: Array Number
        lines =
            [ adjustment.capital
            , adjustment.baseline
            , adjustment.descender
            , adjustment.lowercase
            ]

        ascender =
            Maybe.fromMaybe adjustment.capital (maximum lines)

        descender =
            Maybe.fromMaybe adjustment.descender (minimum lines)

        newBaseline =
            lines
                # Array.filter (\x -> x /= descender)
                # minimum
                # Maybe.fromMaybe adjustment.baseline

        capitalVertical =
            1.0 - ascender

        capitalSize =
            1.0 / (ascender - newBaseline)

        fullSize =
            1.0 / (ascender - descender)

        fullVertical =
            1.0 - ascender

        -- (oldMiddle - newFullMiddle) * 2
    in
    { full:
        adjust fullSize (ascender - descender) fullVertical
    , capital:
        adjust capitalSize (ascender - newBaseline) capitalVertical
    }


adjust size height vertical =
    { vertical: vertical
    , height:
        height / size
    , size: size
    }


renderTopLevelValues :: Array (Tuple String (Array Font)) -> String
renderTopLevelValues rules =
    let
        withImport :: Font -> Maybe String
        withImport font =
            case font of
                ImportFont _ url ->
                    Just ("@import url('" <> url <> "');")

                -- FontWith with ->
                --     case with.url of
                --         Just x ->
                --             Just ("@import url('" <> x <> "');")
                --         Nothing ->
                --             Nothing
                _ ->
                    Nothing

        allNames =
            map fst rules

        fontImports :: forall a. (Tuple a (Array Font)) -> String
        fontImports (Tuple name typefaces) =
            let
                imports =
                    String.joinWith "\n" (Array.mapMaybe withImport typefaces)
            in
            imports

        fontAdjustments (Tuple name typefaces) =
            case typefaceAdjustment typefaces of
                Nothing ->
                    String.joinWith ""
                        (map (renderNullAdjustmentRule name) allNames)

                Just adjustment ->
                    String.joinWith ""
                        (map (renderFontAdjustmentRule name adjustment) allNames)
    in
    String.joinWith "\n" (map fontImports rules)
        <> String.joinWith "\n" (map fontAdjustments rules)

renderNullAdjustmentRule fontToAdjust otherFontName =
    let
        name =
            if fontToAdjust == otherFontName then
                fontToAdjust

            else
                otherFontName <> " ." <> fontToAdjust
    in
    String.joinWith " "
        [ bracket
            ("."
                <> name
                <> "."
                <> unwrap classes.sizeByCapital
                <> ", "
                <> "."
                <> name
                <> " ."
                <> unwrap classes.sizeByCapital
            )
            [ (Tuple "line-height" "1")
            ]
        , bracket
            ("."
                <> name
                <> "."
                <> unwrap classes.sizeByCapital
                <> "> ."
                <> unwrap EStyle.classes.text
                <> ", ."
                <> name
                <> " ."
                <> unwrap classes.sizeByCapital
                <> " > ."
                <> unwrap EStyle.classes.text
            )
            [ (Tuple "vertical-align" "0")
            , (Tuple "line-height" "1")
            ]
        ]

bracket selector rules =
    let
        renderPair (Tuple name val) =
            name <> ": " <> val <> ";"
    in
    selector <> " {" <> String.joinWith "" (map renderPair rules) <> "}"

fontRule :: String -> ClassName -> Tuple (Array (Tuple String String)) (Array (Tuple String String)) -> Array String
fontRule name modifier (Tuple parentAdj textAdjustment) =
    [ bracket
        ("."
            <> name
            <> "."
            <> unwrap modifier
            <> ", "
            <> "."
            <> name
            <> " ."
            <> unwrap modifier
        )
        parentAdj
    , bracket
        ("."
            <> name
            <> "."
            <> unwrap modifier
            <> "> ."
            <> unwrap EStyle.classes.text
            <> ", ."
            <> name
            <> " ."
            <> unwrap modifier
            <> " > ."
            <> unwrap EStyle.classes.text
        )
        textAdjustment
    ]

renderFontAdjustmentRule fontToAdjust (Tuple full capital) otherFontName =
    let
        name =
            if fontToAdjust == otherFontName then
                fontToAdjust

            else
                otherFontName <> " ." <> fontToAdjust
    in
    String.joinWith " "
        (fontRule name classes.sizeByCapital capital <> fontRule name classes.fullSize full)

toStyleSheetString :: OptionRecord -> Array Style -> String
toStyleSheetString options stylesheet =
    let
        combine style rendered =
            { rules: rendered.rules <> renderStyleRule options style Nothing
            , topLevel:
                case topLevelValue style of
                    Nothing ->
                        rendered.topLevel

                    Just topLevel ->
                        topLevel `Array.cons` rendered.topLevel
            }
    in
    case elmFoldlArray combine { topLevel: [], rules: [] } stylesheet of
        { topLevel, rules } ->
            renderTopLevelValues topLevel <> String.joinWith "" rules

toStyleSheet :: forall p i . OptionRecord -> Array Style -> HTML p i
toStyleSheet options styleSheet =
    case options.mode of
        Layout ->
            -- wrap the style node in a div to prevent `Dark Reader` from blowin up the dom.
            HH.div
                []
                [ HH.style
                    []
                    [ HH.text (toStyleSheetString options styleSheet) ]
                ]

        NoStaticStyleSheet ->
            -- wrap the style node in a div to prevent `Dark Reader` from blowin up the dom.
            HH.div
                []
                [ HH.style
                    []
                    [ HH.text (toStyleSheetString options styleSheet) ]
                ]

        WithVirtualCss ->
            unsafeCrashWith "elm-ui-rules not implemented"
            -- HH.element (ElemName "elm-ui-rules")
            --     [ HH.prop (PropName "rules")
            --         (encodeStyles options styleSheet)
            --     ]
            --     []

encodeStyles :: OptionRecord -> Array Style -> Foreign
encodeStyles options stylesheet =
    unsafeCrashWith "not implemented"
    -- stylesheet
    --     # map
    --         (\style ->
    --             let
    --                 styled =
    --                     renderStyleRule options style Nothing
    --             in
    --             (Tuple (getStyleName style) (?styled))
    --         )
    --     # ?object

staticRoot :: forall p i . OptionRecord -> HTML p i
staticRoot opts =
    case opts.mode of
        Layout ->
            -- wrap the style node in a div to prevent `Dark Reader` from blowin up the dom.
            HH.div
                []
                [ HH.style [] [ HH.text EStyle.rules ] ]

        NoStaticStyleSheet ->
            HH.text ""

        WithVirtualCss ->
            HH.element (ElemName "elm-ui-static-rules") [ HH.prop (PropName "rules") EStyle.rules ] []

embedWith static opts styles children =
    let
        dynamicStyleSheet =
            styles
                # elmFoldlArray reduceStyles (Tuple Set.empty (renderFocusStyle opts.focus))
                # snd
                -- |> reduceStylesRecursive Set.empty [ ]) --renderFocusStyle opts.focus ]
                -- |> sortedReduce
                # toStyleSheet opts
    in
    if static then
        staticRoot opts
            : dynamicStyleSheet
            : children

    else
        dynamicStyleSheet
            : children

embedKeyed static opts styles children =
    let
        dynamicStyleSheet =
            styles
                # elmFoldlArray reduceStyles (Tuple Set.empty (renderFocusStyle opts.focus))
                # snd
                -- |> reduceStylesRecursive Set.empty [ ]) --renderFocusStyle opts.focus ]
                -- |> sortedReduce
                # toStyleSheet opts
    in
    if static then
        (Tuple "static-stylesheet" (staticRoot opts))
            : (Tuple "dynamic-stylesheet"
                 dynamicStyleSheet
               )
            : children

    else
        (Tuple "dynamic-stylesheet"
          dynamicStyleSheet
        )
            : children

formatColor :: Color -> String
formatColor (Rgba red green blue alpha) =
    "rgba("
        <> toStringAs decimal (Int.round (red * 255.0))
        <> ("," <> toStringAs decimal (Int.round (green * 255.0)))
        <> ("," <> toStringAs decimal (Int.round (blue * 255.0)))
        <> ("," <> NumberFormat.toString alpha <> ")")

formatBoxShadow shadow =
    String.joinWith " " $
        Array.mapMaybe identity
            [ if shadow.inset then
                Just "inset"

              else
                Nothing
            , Just $ NumberFormat.toString (fst shadow.offset) <> "px"
            , Just $ NumberFormat.toString (snd shadow.offset) <> "px"
            , Just $ NumberFormat.toString shadow.blur <> "px"
            , Just $ NumberFormat.toString shadow.size <> "px"
            , Just $ formatColor shadow.color
            ]



renderFocusStyle :: FocusStyle -> Array Style
renderFocusStyle focus =
    [ Style (unwrap ((EStyle.dot classes.focusedWithin) <> Selector ":focus-within"))
        (Array.mapMaybe identity
            [ map (\color -> Property "border-color" (formatColor color)) focus.borderColor
            , map (\color -> Property "background-color" (formatColor color)) focus.backgroundColor
            , map
                (\shadow ->
                    Property "box-shadow"
                        (formatBoxShadow
                            { color: shadow.color
                            , offset:
                                shadow.offset
                                    # bimap Int.toNumber Int.toNumber
                            , inset: false
                            , blur:
                                Int.toNumber shadow.blur
                            , size:
                                Int.toNumber shadow.size
                            }
                        )
                )
                focus.shadow
            , Just $ Property "outline" "none"
            ]
        )
    , Style
        (unwrap ((EStyle.dot classes.any <> Selector ":focus .focusable, ")
                    <> (EStyle.dot classes.any <> Selector ".focusable:focus, ")
                    <> (Selector ".ui-slide-bar:focus + " <> EStyle.dot classes.any <> Selector " .focusable-thumb"))
        )
        (Array.mapMaybe identity
            [ map (\color -> Property "border-color" (formatColor color)) focus.borderColor
            , map (\color -> Property "background-color" (formatColor color)) focus.backgroundColor
            , map
                (\shadow ->
                    Property "box-shadow"
                        (formatBoxShadow
                            { color: shadow.color
                            , offset:
                                shadow.offset
                                    # bimap Int.toNumber Int.toNumber
                            , inset: false
                            , blur: Int.toNumber shadow.blur
                            , size: Int.toNumber shadow.size
                            }
                        )
                )
                focus.shadow
            , Just $ Property "outline" "none"
            ]
        )
    ]

finalizeNode :: forall p i . Flag.Field -> NodeName -> Array (VDomProp' i) -> Children (HTML p i) -> EmbedStyle -> LayoutContext -> HTML p i
finalizeNode has node attributes children embedMode parentContext =
    let
        createNode :: ElemName -> Array (VDomProp' i) -> HTML p i
        createNode nodeName attrs =
            case children of
                Keyed keyed ->
                    HH.keyed
                        nodeName
                        (map wrap (preProcess attrs))
                        (case embedMode of
                            NoStyleSheet ->
                                keyed

                            OnlyDynamic opts styles ->
                                Array.fromFoldable $ embedKeyed false opts styles (List.fromFoldable keyed)

                            StaticRootAndDynamic opts styles ->
                                Array.fromFoldable $ embedKeyed true opts styles (List.fromFoldable keyed)
                        )

                Unkeyed unkeyed ->
                    (case nodeName of
                        (ElemName "div") ->
                            HH.div

                        (ElemName "p") ->
                            HH.p

                        _ ->
                            HH.element nodeName
                    )
                        (map wrap (preProcess attrs))
                        (case embedMode of
                            NoStyleSheet ->
                                unkeyed

                            OnlyDynamic opts styles ->
                                Array.fromFoldable $ embedWith false opts styles (List.fromFoldable unkeyed)

                            StaticRootAndDynamic opts styles ->
                                Array.fromFoldable $ embedWith true opts styles (List.fromFoldable unkeyed)
                        )

        html =
            case node of
                Generic ->
                    createNode (ElemName "div") attributes

                NodeName nodeName ->
                    createNode (ElemName nodeName) attributes

                Embedded nodeName internal ->
                    HH.element (ElemName nodeName)
                        (map wrap (preProcess attributes))
                        [ createNode (ElemName internal)
                            [ unwrap (HP.classes [classes.any, classes.single])
                            ]
                        ]
    in
    case parentContext of
        AsRow ->
            if Flag.present Flag.widthFill has && not (Flag.present Flag.widthBetween has) then
                html

            else if Flag.present Flag.alignRight has then
                HH.u
                    [ HP.classes
                        [ classes.any
                        , classes.single
                        , classes.container
                        , classes.contentCenterY
                        , classes.alignContainerRight
                        ]
                    ]
                    [ html ]

            else if Flag.present Flag.centerX has then
                HH.element (ElemName "s")
                    [ HP.classes
                        [ classes.any
                        , classes.single
                        , classes.container
                        , classes.contentCenterY
                        , classes.alignContainerCenterX
                        ]
                    ]
                    [ html ]

            else
                html

        AsColumn ->
            if Flag.present Flag.heightFill has && not (Flag.present Flag.heightBetween has) then
                html

            else if Flag.present Flag.centerY has then
                HH.element (ElemName "s")
                    [ HP.classes
                        [ classes.any
                        , classes.single
                        , classes.container
                        , classes.alignContainerCenterY
                        ]
                    ]
                    [ html ]

            else if Flag.present Flag.alignBottom has then
                HH.u
                    [ HP.classes
                        [ classes.any
                        , classes.single
                        , classes.container
                        , classes.alignContainerBottom
                        ]
                    ]
                    [ html ]

            else
                html

        _ ->
            html

type Sizing =
    { fields :: Flag.Field
    , classes :: Array ClassName
    , styles :: Array Style
    }

renderHeight :: Length -> Sizing
renderHeight h =
    case h of
        Px px ->
            let
                val =
                    toStringAs decimal px

                name =
                    "height-px-" <> val
            in
            { fields: Flag.none
            , classes: [EStyle.classes.heightExact, ClassName name]
            , styles: [ Single (Selector name) "height" (val <> "px") ]
            }

        Content ->
            { fields: Flag.add Flag.heightContent Flag.none
            , classes: [EStyle.classes.heightContent]
            , styles: []
            }

        Fill portion ->
            if portion == 1 then
                { fields: Flag.add Flag.heightFill Flag.none
                , classes: [EStyle.classes.heightFill]
                , styles: []
                }

            else
                { fields: Flag.add Flag.heightFill Flag.none
                , classes: [EStyle.classes.heightFillPortion, ClassName "height-fill-" `EStyle.concatClassNames` ClassName (toStringAs decimal portion)]
                , styles: [ Single
                        (Selector (unwrap EStyle.classes.any
                                <> "."
                                <> unwrap EStyle.classes.column
                                <> " > "
                                <> unwrap (EStyle.dot $ ClassName "height-fill-" `EStyle.concatClassNames` ClassName (toStringAs decimal portion))
                            ))
                        "flex-grow"
                        (toStringAs decimal (portion * 100000))
                  ]
                }

        Min minSize len ->
            let
                cls =
                    "min-height-"
                        <> toStringAs decimal minSize

                style =
                    Single
                        (Selector cls)
                        "min-height"
                        -- This needs to be !important because we're using `min-height: min-content`
                        -- to correct for safari's incorrect implementation of flexbox.
                        (toStringAs decimal minSize <> "px !important")

                { fields: newFlag, classes: newAttrs, styles: newStyle } =
                    renderHeight len
            in
            { fields: Flag.add Flag.heightBetween newFlag
            , classes: ClassName cls `Array.cons` newAttrs
            , styles: style `Array.cons` newStyle
            }

        Max maxSize len ->
            let
                cls =
                    "max-height-" <> toStringAs decimal maxSize

                style =
                    Single (Selector cls)
                        "max-height"
                        (toStringAs decimal maxSize <> "px")

                { fields: newFlag, classes: newAttrs, styles: newStyle } =
                    renderHeight len
            in
            { fields: Flag.add Flag.heightBetween newFlag
            , classes: ClassName cls `Array.cons` newAttrs
            , styles: style `Array.cons` newStyle
            }

renderWidth :: Length -> Sizing
renderWidth w =
    case w of
        Px px ->
            { fields: Flag.none
            , classes: [EStyle.classes.widthExact, ClassName "width-px-" `EStyle.concatClassNames` ClassName (toStringAs decimal px)]
            , styles: [ Single (Selector ("width-px-" <> toStringAs decimal px)) "width" (toStringAs decimal px <> "px") ]
            }

        Content ->
            { fields: Flag.add Flag.widthContent Flag.none
            , classes: [EStyle.classes.widthContent]
            , styles: []
            }

        Fill portion ->
            if portion == 1 then
                { fields: Flag.add Flag.widthFill Flag.none
                , classes: [EStyle.classes.widthFill]
                , styles: []
                }

            else
                { fields: Flag.add Flag.widthFill Flag.none
                , classes: [EStyle.classes.widthFillPortion, ClassName "width-fill-" `EStyle.concatClassNames` ClassName (toStringAs decimal portion)]
                , styles: [ Single
                        (Selector (unwrap EStyle.classes.any
                            <> "."
                            <> unwrap EStyle.classes.row
                            <> " > "
                            <> unwrap (ClassName "width-fill-" `EStyle.concatClassNames` ClassName (toStringAs decimal portion) # EStyle.dot))
                        )
                        "flex-grow"
                        (toStringAs decimal (portion * 100000))
                  ]
                }

        Min minSize len ->
            let
                cls =
                    "min-width-"
                        <> toStringAs decimal minSize

                style =
                    Single
                        (Selector cls)
                        "min-width"
                        (toStringAs decimal minSize <> "px")

                { fields: newFlag, classes: newAttrs, styles: newStyle } =
                    renderWidth len
            in
            { fields: Flag.add Flag.widthBetween newFlag
            , classes: ClassName cls `Array.cons` newAttrs
            , styles: style `Array.cons` newStyle
            }

        Max maxSize len ->
            let
                cls =
                    "max-width-" <> toStringAs decimal maxSize

                style =
                    Single (Selector cls)
                        "max-width"
                        (toStringAs decimal maxSize <> "px")

                { fields: newFlag, classes: newAttrs, styles: newStyle } =
                    renderWidth len
            in
            { fields: Flag.add Flag.widthBetween newFlag
            , classes: ClassName cls `Array.cons` newAttrs
            , styles: style `Array.cons` newStyle
            }

data NearbyChildren p i
    = NoNearbyChildren
    | ChildrenBehind (Array (HH.HTML p i))
    | ChildrenInFront (Array (HH.HTML p i))
    | ChildrenBehindAndInFront (Array (HH.HTML p i)) (Array (HH.HTML p i))

type Gathered p i =
    { node :: NodeName
    , attributes :: Array (VDomProp' i)
    , styles :: Array Style
    , children :: NearbyChildren p i
    , has :: Flag.Field
    }

floatClass :: Number -> String
floatClass x =
    toStringAs decimal (Int.round (x * 255.0))

lengthClassName :: Length -> String
lengthClassName x =
    case x of
        Px px ->
            toStringAs decimal px <> "px"

        Content ->
            "auto"

        Fill i ->
            toStringAs decimal i <> "fr"

        Min min len ->
            "min" <> toStringAs decimal min <> lengthClassName len

        Max max len ->
            "max" <> toStringAs decimal max <> lengthClassName len

getStyleName :: Style -> String
getStyleName style =
    case style of
        Shadows name _ ->
            name

        Transparency name o ->
            name

        Style klass _ ->
            klass

        FontFamily name _ ->
            name

        FontSize i ->
            "font-size-" <> toStringAs decimal i

        Single (Selector klass) _ _ ->
            klass

        Colored klass _ _ ->
            klass

        SpacingStyle cls x y ->
            cls

        PaddingStyle cls top right bottom left ->
            cls

        BorderWidth cls top right bottom left ->
            cls

        GridTemplateStyle template ->
            "grid-rows-"
                <> String.joinWith "-" (map lengthClassName template.rows)
                <> "-cols-"
                <> String.joinWith "-" (map lengthClassName template.columns)
                <> "-space-x-"
                <> lengthClassName (fst template.spacing)
                <> "-space-y-"
                <> lengthClassName (snd template.spacing)

        GridPosition pos ->
            "gp grid-pos-"
                <> toStringAs decimal pos.row
                <> "-"
                <> toStringAs decimal pos.col
                <> "-"
                <> toStringAs decimal pos.width
                <> "-"
                <> toStringAs decimal pos.height

        PseudoSelector selector subStyle ->
            let
                name =
                    case selector of
                        Focus ->
                            "fs"

                        Hover ->
                            "hv"

                        Active ->
                            "act"
            in
            map
                (\sty ->
                    case getStyleName sty of
                        "" ->
                            ""

                        styleName ->
                            styleName <> "-" <> name
                )
                subStyle
                # String.joinWith " "

        Transform x ->
            Maybe.fromMaybe "" (map unwrap (transformClass x))

transformClass :: Transformation -> Maybe ClassName
transformClass transform =
    case transform of
        Untransformed ->
            Nothing

        Moved (XYZ x y z) ->
            Just $ ClassName $
                "mv-"
                    <> floatClass x
                    <> "-"
                    <> floatClass y
                    <> "-"
                    <> floatClass z

        FullTransform (XYZ tx ty tz) (XYZ sx sy sz) (XYZ ox oy oz) angle ->
            Just $ ClassName $
                "tfrm-"
                    <> floatClass tx
                    <> "-"
                    <> floatClass ty
                    <> "-"
                    <> floatClass tz
                    <> "-"
                    <> floatClass sx
                    <> "-"
                    <> floatClass sy
                    <> "-"
                    <> floatClass sz
                    <> "-"
                    <> floatClass ox
                    <> "-"
                    <> floatClass oy
                    <> "-"
                    <> floatClass oz
                    <> "-"
                    <> floatClass (unwrap angle)

skippable :: Flag -> Style -> Boolean
skippable flag style =
    if flag == Flag.borderWidth then
        case style of
            Single _ _ val ->
                case val of
                    "0px" ->
                        true

                    "1px" ->
                        true

                    "2px" ->
                        true

                    "3px" ->
                        true

                    "4px" ->
                        true

                    "5px" ->
                        true

                    "6px" ->
                        true

                    _ ->
                        false

            _ ->
                false

    else
        case style of
            FontSize i ->
                i >= 8 && i <= 32

            PaddingStyle name t r b l ->
                t == b && t == r && t == l && t >= 0.0 && t <= 24.0

            _ ->
                false

composeTransformation :: Transformation -> TransformComponent -> Transformation
composeTransformation transform component =
    case transform of
        Untransformed ->
            case component of
                MoveX x ->
                    Moved (XYZ x 0.0 0.0)

                MoveY y ->
                    Moved (XYZ 0.0 y 0.0)

                MoveZ z ->
                    Moved (XYZ 0.0 0.0 z)

                MoveXYZ xyz ->
                    Moved xyz

                Rotate xyz angle ->
                    FullTransform (XYZ 0.0 0.0 0.0) (XYZ 1.0 1.0 1.0) xyz (Angle angle)

                Scale xyz ->
                    FullTransform (XYZ 0.0 0.0 0.0) xyz (XYZ 0.0 0.0 1.0) (Angle 0.0)

        Moved (moved @ (XYZ x y z )) ->
            case component of
                MoveX newX ->
                    Moved (XYZ newX y z)

                MoveY newY ->
                    Moved (XYZ x newY z)

                MoveZ newZ ->
                    Moved (XYZ x y newZ)

                MoveXYZ xyz ->
                    Moved xyz

                Rotate xyz angle ->
                    FullTransform moved (XYZ 1.0 1.0 1.0) xyz (Angle angle)

                Scale scale ->
                    FullTransform moved scale (XYZ 0.0 0.0 1.0) (Angle 0.0)

        FullTransform (moved @ (XYZ x y z)) scaled origin angle ->
            case component of
                MoveX newX ->
                    FullTransform (XYZ newX y z) scaled origin angle

                MoveY newY ->
                    FullTransform (XYZ x newY z) scaled origin angle

                MoveZ newZ ->
                    FullTransform (XYZ x y newZ) scaled origin angle

                MoveXYZ newMove ->
                    FullTransform newMove scaled origin angle

                Rotate newOrigin newAngle ->
                    FullTransform moved scaled newOrigin (Angle newAngle)

                Scale newScale ->
                    FullTransform moved newScale origin angle

addNodeName :: String -> NodeName -> NodeName
addNodeName newNode old =
    case old of
        Generic ->
            NodeName newNode

        NodeName name ->
            Embedded name newNode

        Embedded x y ->
            Embedded x y

nearbyElement :: forall p i . Location -> Element p i -> HTML p i
nearbyElement location element =
    HH.div
        [ HP.classes $
            case location of
                Above ->
                    [ EStyle.classes.nearby
                    , EStyle.classes.single
                    , EStyle.classes.above
                    ]

                Below ->
                    [ EStyle.classes.nearby
                    , EStyle.classes.single
                    , EStyle.classes.below
                    ]

                OnRight ->
                    [ EStyle.classes.nearby
                    , EStyle.classes.single
                    , EStyle.classes.onRight
                    ]

                OnLeft ->
                    [ EStyle.classes.nearby
                    , EStyle.classes.single
                    , EStyle.classes.onLeft
                    ]

                InFront ->
                    [ EStyle.classes.nearby
                    , EStyle.classes.single
                    , EStyle.classes.inFront
                    ]

                Behind ->
                    [ EStyle.classes.nearby
                    , EStyle.classes.single
                    , EStyle.classes.behind
                    ]
        ]
        [ case element of
            Empty ->
                HH.text ""

            Text str ->
                textElement str

            Unstyled html ->
                html AsEl

            Styled styled ->
                styled.html NoStyleSheet AsEl
        ]

addNearbyElement :: forall p i . Location -> Element p i -> NearbyChildren p i -> NearbyChildren p i
addNearbyElement location elem existing =
    let
        nearby =
            nearbyElement location elem
    in
    case existing of
        NoNearbyChildren ->
            case location of
                Behind ->
                    ChildrenBehind [ nearby ]

                _ ->
                    ChildrenInFront [ nearby ]

        ChildrenBehind existingBehind ->
            case location of
                Behind ->
                    ChildrenBehind (nearby `Array.cons` existingBehind)

                _ ->
                    ChildrenBehindAndInFront existingBehind [ nearby ]

        ChildrenInFront existingInFront ->
            case location of
                Behind ->
                    ChildrenBehindAndInFront [ nearby ] existingInFront

                _ ->
                    ChildrenInFront (nearby `Array.cons` existingInFront)

        ChildrenBehindAndInFront existingBehind existingInFront ->
            case location of
                Behind ->
                    ChildrenBehindAndInFront (nearby `Array.cons` existingBehind) existingInFront

                _ ->
                    ChildrenBehindAndInFront existingBehind (nearby `Array.cons` existingInFront)

alignXName :: HAlign -> Array ClassName
alignXName align =
    case align of
        Left ->
            [classes.alignedHorizontally, classes.alignLeft]

        Right ->
            [classes.alignedHorizontally, classes.alignRight]

        CenterX ->
            [classes.alignedHorizontally, classes.alignCenterX]


alignYName :: VAlign -> Array ClassName
alignYName align =
    case align of
        Top ->
            [classes.alignedVertically, classes.alignTop]

        Bottom ->
            [classes.alignedVertically, classes.alignBottom]

        CenterY ->
            [classes.alignedVertically, classes.alignCenterY]

gatherAttrRecursive :: forall aligned p i .
    Array ClassName
    -> NodeName
    -> Flag.Field
    -> Transformation
    -> Array Style
    -> List (VDomProp' i)
    -> NearbyChildren p i
    -> List (Attribute aligned p i)
    -> Gathered p i
gatherAttrRecursive classes node has transform styles attrs children elementAttrs =
    case elementAttrs of
        Nil ->
            case transformClass transform of
                Nothing ->
                    { attributes: Array.fromFoldable $ (unwrap (HP.classes classes)) : attrs
                    , styles: styles
                    , node: node
                    , children: children
                    , has: has
                    }

                Just klass ->
                    { attributes: Array.fromFoldable $ (unwrap (HP.classes $ klass `Array.cons` classes)) : attrs
                    , styles: Transform transform `Array.cons` styles
                    , node: node
                    , children: children
                    , has: has
                    }

        (Cons attribute remaining) ->
            case attribute of
                NoAttribute ->
                    gatherAttrRecursive classes node has transform styles attrs children remaining

                Class flag exactClassName ->
                    if Flag.present flag has then
                        gatherAttrRecursive classes node has transform styles attrs children remaining

                    else
                        gatherAttrRecursive (exactClassName `Array.cons` classes) node (Flag.add flag has) transform styles attrs children remaining

                Attr actualAttribute ->
                    gatherAttrRecursive classes node has transform styles (actualAttribute : attrs) children remaining

                StyleClass flag style ->
                    Debug.trace (Tuple (show flag) (show has)) \_ ->
                    if Flag.present flag has then
                        Debug.trace "flag present" \_ ->
                        Debug.trace (Tuple flag style) \_ ->
                        -- Debug.spy "present return value" $
                        gatherAttrRecursive classes node has transform styles attrs children remaining

                    else if skippable flag style then
                        Debug.trace "flag not present but is skippable" \_ ->
                        Debug.trace (Tuple flag style) \_ ->
                        -- Debug.spy "skippable return value" $
                        gatherAttrRecursive (wrap (getStyleName style) `Array.cons` classes)
                            node
                            (Flag.add flag has)
                            transform
                            styles
                            attrs
                            children
                            remaining

                    else
                        Debug.trace "flag not present and not skippable" \_ ->
                        Debug.trace (Tuple flag style) \_ ->
                        -- Debug.spy "none left beef return value" $
                        gatherAttrRecursive (wrap (getStyleName style) `Array.cons` classes)
                            node
                            (Flag.add flag has)
                            transform
                            (style `Array.cons` styles)
                            attrs
                            children
                            remaining

                TransformComponent flag component ->
                    gatherAttrRecursive classes
                        node
                        (Flag.add flag has)
                        (composeTransformation transform component)
                        styles
                        attrs
                        children
                        remaining

                Width width ->
                    if Flag.present Flag.width has then
                        gatherAttrRecursive classes node has transform styles attrs children remaining

                    else
                        case width of
                            Px px ->
                                gatherAttrRecursive (EStyle.classes.widthExact `Array.cons` (ClassName ("width-px-" <> toStringAs decimal px) `Array.cons` classes))
                                    node
                                    (Flag.add Flag.width has)
                                    transform
                                    (Single (Selector $ "width-px-" <> toStringAs decimal px) "width" (toStringAs decimal px <> "px") `Array.cons` styles)
                                    attrs
                                    children
                                    remaining

                            Content ->
                                gatherAttrRecursive (EStyle.classes.widthContent `Array.cons` classes)
                                    node
                                    (Flag.add Flag.widthContent (Flag.add Flag.width has))
                                    transform
                                    styles
                                    attrs
                                    children
                                    remaining

                            Fill portion ->
                                if portion == 1 then
                                    gatherAttrRecursive (EStyle.classes.widthFill `Array.cons` classes)
                                        node
                                        (Flag.add Flag.widthFill (Flag.add Flag.width has))
                                        transform
                                        styles
                                        attrs
                                        children
                                        remaining

                                else
                                    gatherAttrRecursive ((ClassName ("width-fill-" <> toStringAs decimal portion)) `Array.cons` (EStyle.classes.widthFillPortion `Array.cons` classes))
                                        node
                                        (Flag.add Flag.widthFill (Flag.add Flag.width has))
                                        transform
                                        (Single
                                            (Selector (unwrap EStyle.classes.any
                                                    <> "."
                                                    <> unwrap EStyle.classes.row
                                                    <> " > "
                                                    <> unwrap (EStyle.dot $ wrap ("width-fill-" <> toStringAs decimal portion))
                                                ))
                                            "flex-grow"
                                            (toStringAs decimal (portion * 100000))
                                            `Array.cons` styles
                                        )
                                        attrs
                                        children
                                        remaining

                            _ ->
                                let
                                    { fields: addToFlags, classes: newClass, styles: newStyles } =
                                        renderWidth width
                                in
                                gatherAttrRecursive (newClass <> classes)
                                    node
                                    (Flag.merge addToFlags (Flag.add Flag.width has))
                                    transform
                                    (newStyles <> styles)
                                    attrs
                                    children
                                    remaining

                Height height ->
                    if Flag.present Flag.height has then
                        gatherAttrRecursive classes node has transform styles attrs children remaining

                    else
                        case height of
                            Px px ->
                                let
                                    val =
                                        toStringAs decimal px <> "px"

                                    name =
                                        "height-px-" <> val
                                in
                                gatherAttrRecursive (EStyle.classes.heightExact `Array.cons` ((ClassName name) `Array.cons` classes))
                                    node
                                    (Flag.add Flag.height has)
                                    transform
                                    (Single (Selector name) "height " val `Array.cons` styles)
                                    attrs
                                    children
                                    remaining

                            Content ->
                                gatherAttrRecursive (EStyle.classes.heightContent `Array.cons` classes)
                                    node
                                    (Flag.add Flag.heightContent (Flag.add Flag.height has))
                                    transform
                                    styles
                                    attrs
                                    children
                                    remaining

                            Fill portion ->
                                if portion == 1 then
                                    gatherAttrRecursive (EStyle.classes.heightFill `Array.cons` classes)
                                        node
                                        (Flag.add Flag.heightFill (Flag.add Flag.height has))
                                        transform
                                        styles
                                        attrs
                                        children
                                        remaining

                                else
                                    gatherAttrRecursive (EStyle.classes.heightFillPortion `Array.cons` ((ClassName ("height-fill-" <> toStringAs decimal portion)) `Array.cons` classes))
                                        node
                                        (Flag.add Flag.heightFill (Flag.add Flag.height has))
                                        transform
                                        (Single
                                            (Selector $ unwrap EStyle.classes.any
                                                <> "."
                                                <> unwrap EStyle.classes.column
                                                <> " > "
                                                <> (unwrap (EStyle.dot $ wrap ("height-fill-" <> toStringAs decimal portion)))
                                            )
                                            "flex-grow"
                                            (toStringAs decimal (portion * 100000))
                                            `Array.cons` styles
                                        )
                                        attrs
                                        children
                                        remaining

                            _ ->
                                let
                                    { fields: addToFlags, classes: newClass, styles: newStyles } =
                                        renderHeight height
                                in
                                gatherAttrRecursive (newClass <> classes)
                                    node
                                    (Flag.merge addToFlags (Flag.add Flag.height has))
                                    transform
                                    (newStyles <> styles)
                                    attrs
                                    children
                                    remaining

                Describe description ->
                    case description of
                        Main ->
                            gatherAttrRecursive classes (addNodeName "main" node) has transform styles attrs children remaining

                        Navigation ->
                            gatherAttrRecursive classes (addNodeName "nav" node) has transform styles attrs children remaining

                        ContentInfo ->
                            gatherAttrRecursive classes (addNodeName "footer" node) has transform styles attrs children remaining

                        Complementary ->
                            gatherAttrRecursive classes (addNodeName "aside" node) has transform styles attrs children remaining

                        Heading i ->
                            if i <= 1 then
                                gatherAttrRecursive classes (addNodeName "h1" node) has transform styles attrs children remaining

                            else if i < 7 then
                                gatherAttrRecursive classes (addNodeName ("h" <> toStringAs decimal i) node) has transform styles attrs children remaining

                            else
                                gatherAttrRecursive classes (addNodeName "h6" node) has transform styles attrs children remaining

                        Paragraph ->
                            -- previously we rendered a <p> tag, though apparently this invalidates the html if it has <div>s inside.
                            -- Since we can't guaranteee that there are no divs, we need another strategy.
                            -- While it's not documented in many places, there apparently is a paragraph aria role
                            -- https://github.com/w3c/aria/blob/11f85f41a5b621fdbe85fc9bcdcd270e653a48ba/common/script/roleInfo.js
                            -- Though we'll need to wait till it gets released in an official wai-aria spec to use it.
                            -- If it's used at the moment, then Lighthouse complains (likely rightfully) that role paragraph is not recognized.
                            gatherAttrRecursive
                                classes
                                node
                                has
                                transform
                                styles
                                attrs
                                children
                                remaining

                        Button ->
                            gatherAttrRecursive classes node has transform styles (unwrap (HP.attr (AttrName "role") "button") : attrs) children remaining

                        Label label ->
                            gatherAttrRecursive classes node has transform styles (unwrap (HP.attr (AttrName "aria-label") label) : attrs) children remaining

                        LivePolite ->
                            gatherAttrRecursive classes node has transform styles (unwrap (HP.attr (AttrName "aria-live") "polite") : attrs) children remaining

                        LiveAssertive ->
                            gatherAttrRecursive classes node has transform styles (unwrap (HP.attr (AttrName "aria-live") "assertive") : attrs) children remaining

                Nearby location elem ->
                    let
                        newStyles =
                            case elem of
                                Empty ->
                                    styles

                                Text str ->
                                    styles

                                Unstyled html ->
                                    styles

                                Styled styled ->
                                    styles <> styled.styles
                    in
                    gatherAttrRecursive
                        classes
                        node
                        has
                        transform
                        newStyles
                        attrs
                        (addNearbyElement location elem children)
                        remaining

                AlignX x ->
                    if Flag.present Flag.xAlign has then
                        gatherAttrRecursive classes node has transform styles attrs children remaining

                    else
                        gatherAttrRecursive (alignXName x <> classes)
                            node
                            (has
                                # Flag.add Flag.xAlign
                                # (\flags ->
                                        case x of
                                            CenterX ->
                                                Flag.add Flag.centerX flags

                                            Right ->
                                                Flag.add Flag.alignRight flags

                                            _ ->
                                                flags
                                   )
                            )
                            transform
                            styles
                            attrs
                            children
                            remaining

                AlignY y ->
                    if Flag.present Flag.yAlign has then
                        gatherAttrRecursive classes node has transform styles attrs children remaining

                    else
                        gatherAttrRecursive (alignYName y <> classes)
                            node
                            (Flag.add Flag.yAlign has
                                # (\flags ->
                                        case y of
                                            CenterY ->
                                                Flag.add Flag.centerY flags

                                            Bottom ->
                                                Flag.add Flag.alignBottom flags

                                            _ ->
                                                flags
                                   )
                            )
                            transform
                            styles
                            attrs
                            children
                            remaining

unwrapDecorations :: Array (Attribute Void Void Void) -> Array Style
unwrapDecorations attrs =
    case elmFoldlArray unwrapDecsHelper (Tuple [] Untransformed) attrs of
        (Tuple styles transform) ->
            Transform transform `Array.cons` styles


unwrapDecsHelper attr (Tuple styles trans) =
    case removeNever attr of
        StyleClass _ style ->
            (Tuple (style `Array.cons` styles) trans)

        TransformComponent flag component ->
            (Tuple styles (composeTransformation trans component))

        _ ->
            (Tuple styles trans)

removeNever :: forall p i. Attribute Void p Void -> Attribute Unit p i
removeNever style =
    mapAttrFromStyle absurd style

mapAttrFromStyle :: forall p i1 i2. (i1 -> i2) -> Attribute Void p i1 -> Attribute Unit p i2
mapAttrFromStyle fn attr =
    case attr of
        NoAttribute ->
            NoAttribute

        Describe description ->
            Describe description

        AlignX x ->
            AlignX x

        AlignY y ->
            AlignY y

        Width x ->
            Width x

        Height x ->
            Height x

        -- invalidation key "border-color" as opposed to "border-color-10-10-10" that will be the key for the class
        Class x y ->
            Class x y

        StyleClass flag style ->
            StyleClass flag style

        Nearby location elem ->
            Nearby location (map fn elem)

        Attr htmlAttr ->
            Attr (map (map fn) htmlAttr)

        TransformComponent fl trans ->
            TransformComponent fl trans
