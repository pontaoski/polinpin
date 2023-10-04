module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Data.Tuple (Tuple(..), fst)
import Effect.Class.Console (log)
import Element.Internal.Flag as Flag
import Test.Unit (suite, test, failure, success, Test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Data.Array as Array
import Element.Internal.Int53 as Int53
import Element.Internal.Style as Internal.Style
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Data.Bifunctor (lmap)

main :: Effect Unit
main = runTest do
  suite "flag operations" do
    test "all flags invalidate themselves" do
      assertAll (\flag -> Flag.present flag (Flag.add flag Flag.none)) allFlags
    test "all flags don't interfere with eachother" do
      assertAll (doesntInvalidateOthers allFlags) allFlags
    test "flags are null by default" do
      assertAll (\flag -> not (Flag.present flag Flag.none)) allFlags
  suite "classname collisions" do
    test "no duplicates" do
      Assert.equal' "class names" [] onlyDuplicates

assertAll :: forall a. Show a => (a -> Boolean) -> Array a -> Test
assertAll fn items =
  case Array.findMap (\item -> if fn item then Nothing else Just item) items of
    Just failureCase ->
      failure $ "predicate failed for " <> (show failureCase)
    Nothing ->
      success

doesntInvalidateOthers :: Array Flag.Flag -> Flag.Flag -> Boolean
doesntInvalidateOthers others flag =
    let
        withFlag =
            Flag.none
                # Flag.add flag
    in
    Array.all identity $
        map
            (\otherFlag ->
                Flag.present otherFlag (Flag.add otherFlag withFlag)
            )
            others

allFlags :: Array Flag.Flag
allFlags =
    [ Flag.transparency
    , Flag.padding
    , Flag.spacing
    , Flag.fontSize
    , Flag.fontFamily
    , Flag.width
    , Flag.height
    , Flag.bgColor
    , Flag.bgImage
    , Flag.bgGradient
    , Flag.borderStyle
    , Flag.fontAlignment
    , Flag.fontWeight
    , Flag.fontColor
    , Flag.wordSpacing
    , Flag.letterSpacing
    , Flag.borderRound
    , Flag.shadows
    , Flag.overflow
    , Flag.cursor
    , Flag.scale
    , Flag.rotate
    , Flag.moveX
    , Flag.moveY
    , Flag.borderWidth
    , Flag.borderColor
    , Flag.yAlign
    , Flag.xAlign
    , Flag.focus
    , Flag.active
    , Flag.hover
    , Flag.gridTemplate
    , Flag.gridPosition
    , Flag.heightContent
    , Flag.heightFill
    , Flag.widthContent
    , Flag.widthFill
    , Flag.alignRight
    , Flag.alignBottom
    , Flag.centerX
    , Flag.centerY
    , Flag.fontVariant
    ]

viewPair :: forall p i. (Tuple String String) -> HTML p i
viewPair (Tuple name description) =
    HH.div []
        [ HH.text name
        , HH.text ": "
        , HH.text description
        ]

onlyDuplicates =
    Array.filter findDuplicates allClassNames

findDuplicates (Tuple name description) =
    Array.any
        (\(Tuple checkName checkDescription) ->
            checkName == name && description /= checkDescription
        )
        allClassNames

allClassNames =
    map (lmap \fn -> fn Internal.Style.classes) allClassNameFns

allClassNameFns =
    [ (Tuple _.root "root")
    , (Tuple _.any "any")
    , (Tuple _.single "single")
    , (Tuple _.row "row")
    , (Tuple _.column "column")
    , (Tuple _.page "page")
    , (Tuple _.paragraph "paragraph")
    , (Tuple _.text "text")
    , (Tuple _.grid "grid")
    , (Tuple _.imageContainer "imageContainer")

    -- widhts/heights
    , (Tuple _.widthFill "widthFill")
    , (Tuple _.widthContent "widthContent")
    , (Tuple _.widthExact "widthExact")
    , (Tuple _.widthFillPortion "widthFillPortion")
    , (Tuple _.heightFill "heightFill")
    , (Tuple _.heightContent "heightContent")
    , (Tuple _.heightFillPortion "heightFillPortion")
    , (Tuple _.seButton "seButton")

    -- nearby elements
    , (Tuple _.above "above")
    , (Tuple _.below "below")
    , (Tuple _.onRight "onRight")
    , (Tuple _.onLeft "onLeft")
    , (Tuple _.inFront "inFront")
    , (Tuple _.behind "behind")

    -- alignments
    , (Tuple _.alignTop "alignTop")
    , (Tuple _.alignBottom "alignBottom")
    , (Tuple _.alignRight "alignRight")
    , (Tuple _.alignLeft "alignLeft")
    , (Tuple _.alignCenterX "alignCenterX")
    , (Tuple _.alignCenterY "alignCenterY")
    , (Tuple _.alignedHorizontally "alignedHorizontally")
    , (Tuple _.alignedVertically "alignedVertically")

    -- space evenly
    , (Tuple _.spaceEvenly "spaceEvenly")
    , (Tuple _.container "container")
    , (Tuple _.alignContainerRight "alignContainerRight")
    , (Tuple _.alignContainerBottom "alignContainerBottom")
    , (Tuple _.alignContainerCenterX "alignContainerCenterX")
    , (Tuple _.alignContainerCenterY "alignContainerCenterY")

    -- content alignments
    , (Tuple _.contentTop "contentTop")
    , (Tuple _.contentBottom "contentBottom")
    , (Tuple _.contentRight "contentRight")
    , (Tuple _.contentLeft "contentLeft")
    , (Tuple _.contentCenterX "contentCenterX")
    , (Tuple _.contentCenterY "contentCenterY")

    -- selection
    , (Tuple _.noTextSelection "noTextSelection")
    , (Tuple _.cursorPointer "cursorPointer")
    , (Tuple _.cursorText "cursorText")

    -- pointer events
    , (Tuple _.passPointerEvents "passPointerEvents")
    , (Tuple _.capturePointerEvents "capturePointerEvents")
    , (Tuple _.transparent "transparent")
    , (Tuple _.opaque "opaque")
    , (Tuple _.overflowHidden "overflowHidden")

    -- special state classes
    , (Tuple _.hover "hover")

    -- , ( .hoverOpaque, "hoverOpaque" )
    , (Tuple _.focus "focus")

    -- , ( .focusOpaque, "focusOpaque" )
    , (Tuple _.active "active")

    -- , ( .activeOpaque, "activeOpaque" )
    --scrollbars
    , (Tuple _.scrollbars "scrollbars")
    , (Tuple _.scrollbarsX "scrollbarsX")
    , (Tuple _.scrollbarsY "scrollbarsY")
    , (Tuple _.clip "clip")
    , (Tuple _.clipX "clipX")
    , (Tuple _.clipY "clipY")

    -- borders
    , (Tuple _.borderNone "borderNone")
    , (Tuple _.borderDashed "borderDashed")
    , (Tuple _.borderDotted "borderDotted")
    , (Tuple _.borderSolid "borderSolid")

    -- text weight
    , (Tuple _.textThin "textThin")
    , (Tuple _.textExtraLight "textExtraLight")
    , (Tuple _.textLight "textLight")
    , (Tuple _.textNormalWeight "textNormalWeight")
    , (Tuple _.textMedium "textMedium")
    , (Tuple _.textSemiBold "textSemiBold")
    , (Tuple _.bold "bold")
    , (Tuple _.textExtraBold "textExtraBold")
    , (Tuple _.textHeavy "textHeavy")
    , (Tuple _.italic "italic")
    , (Tuple _.strike "strike")
    , (Tuple _.underline "underline")
    , (Tuple _.textUnitalicized "textUnitalicized")

    -- text alignment
    , (Tuple _.textJustify "textJustify")
    , (Tuple _.textJustifyAll "textJustifyAll")
    , (Tuple _.textCenter "textCenter")
    , (Tuple _.textRight "textRight")
    , (Tuple _.textLeft "textLeft")
    , (Tuple _.transition "transition")

    -- inputText
    , (Tuple _.inputText "inputText")
    , (Tuple _.inputMultiline "inputMultiline")
    ]

