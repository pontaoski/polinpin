module Element.Internal.Merginator where

import Prelude
import Halogen.VDom.DOM.Prop (Prop(..), PropValue, propFromString)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen.Query.Input (Input)
import Unsafe.Coerce (unsafeCoerce)
import Data.String as String
import Data.String (Pattern(..))

preProcess :: forall i. Array (Prop (Input i)) -> Array (Prop (Input i))
preProcess props =
    let
        withClasses :: Array PropValue
        withClasses =
            Array.mapMaybe (\item ->
                case item of
                    (Property "className" uwu) ->
                        Just uwu
                    _ ->
                        Nothing) props

        classesAsStrings :: Array String
        classesAsStrings =
            map (\item -> unsafeCoerce item) withClasses

        classesSeparated :: Array String
        classesSeparated =
            Array.concatMap (String.split (Pattern " ")) classesAsStrings
            # Array.nub

        classesRejoined :: String
        classesRejoined =
            String.joinWith " " classesSeparated

        withoutClasses :: Array (Prop (Input i))
        withoutClasses =
            Array.mapMaybe (\item ->
                case item of
                    (Property "className" _) ->
                        Nothing
                    _ ->
                        Just item) props

        allTogetherNow :: Array (Prop (Input i))
        allTogetherNow =
            if classesRejoined == "" then
                withoutClasses
            else
                (Property "className" (propFromString classesRejoined)) `Array.cons` withoutClasses
    in
    allTogetherNow
