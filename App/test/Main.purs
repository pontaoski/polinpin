module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Element.Internal.Flag as Flag
import Test.Unit (suite, test, failure, success, Test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Data.Array as Array
import Element.Internal.Int53 as Int53

main :: Effect Unit
main = runTest do
  suite "flag operations" do
    test "all flags invalidate themselves" do
      assertAll (\flag -> Flag.present flag (Flag.add flag Flag.none)) allFlags
    test "all flags don't interfere with eachother" do
      assertAll (doesntInvalidateOthers allFlags) allFlags
    test "flags are null by default" do
      assertAll (\flag -> not (Flag.present flag Flag.none)) allFlags

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

