module Element.Internal.Merginator where

import Prelude
import Halogen.HTML (IProp)
import Halogen.VDom.DOM.Prop (Prop(..), PropValue, propFromString)
import Debug as Debug
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Halogen.Query.Input (Input)
import Unsafe.Coerce (unsafeCoerce)
import Data.String as String
import Data.String (Pattern(..))

preProcess :: forall r i. Array (IProp r i) -> Array (IProp r i)
preProcess props =
    let
        withClasses :: Array PropValue
        withClasses =
            Array.mapMaybe (\item ->
                case item of
                    (Property "className" uwu) ->
                        Just uwu
                    _ ->
                        Nothing) (map unwrap props)

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
                        Just item) (map unwrap props)

        allTogetherNow :: Array (Prop (Input i))
        allTogetherNow =
            if classesRejoined == "" then
                withoutClasses
            else
                (Property "className" (propFromString classesRejoined)) `Array.cons` withoutClasses

        backToHalogen :: Array (IProp r i)
        backToHalogen =
            map wrap allTogetherNow
    in
    backToHalogen -- Debug.trace backToHalogen \_ -> backToHalogen
