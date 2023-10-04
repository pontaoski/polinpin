module Element.Internal.Flag where

import Prelude

import Data.Number (round, log)
import Element.Internal.Int53 (Int53)
import Element.Internal.Int53 as Int53
import Data.Array as Array
import Data.String as String

data Field
    = Field Int53 Int53

data Flag
    = Flag Int53
    | Second Int53

derive instance flagEq :: Eq Flag

instance flagShow :: Show Flag where
    show flag' =
        if flag' == transparency then "transparency" else
        if flag' == padding then "padding" else
        if flag' == spacing then "spacing" else
        if flag' == fontSize then "fontSize" else
        if flag' == fontFamily then "fontFamily" else
        if flag' == width then "width" else
        if flag' == height then "height" else
        if flag' == bgColor then "bgColor" else
        if flag' == bgImage then "bgImage" else
        if flag' == bgGradient then "bgGradient" else
        if flag' == borderStyle then "borderStyle" else
        if flag' == fontAlignment then "fontAlignment" else
        if flag' == fontWeight then "fontWeight" else
        if flag' == fontColor then "fontColor" else
        if flag' == wordSpacing then "wordSpacing" else
        if flag' == letterSpacing then "letterSpacing" else
        if flag' == borderRound then "borderRound" else
        if flag' == txtShadows then "txtShadows" else
        if flag' == shadows then "shadows" else
        if flag' == overflow then "overflow" else
        if flag' == cursor then "cursor" else
        if flag' == scale then "scale" else
        if flag' == rotate then "rotate" else
        if flag' == moveX then "moveX" else
        if flag' == moveY then "moveY" else
        if flag' == borderWidth then "borderWidth" else
        if flag' == borderColor then "borderColor" else
        if flag' == yAlign then "yAlign" else
        if flag' == xAlign then "xAlign" else
        if flag' == focus then "focus" else
        if flag' == active then "active" else
        if flag' == hover then "hover" else
        if flag' == gridTemplate then "gridTemplate" else
        if flag' == gridPosition then "gridPosition" else
        if flag' == heightContent then "heightContent" else
        if flag' == heightFill then "heightFill" else
        if flag' == widthContent then "widthContent" else
        if flag' == widthFill then "widthFill" else
        if flag' == alignRight then "alignRight" else
        if flag' == alignBottom then "alignBottom" else
        if flag' == centerX then "centerX" else
        if flag' == centerY then "centerY" else
        if flag' == widthBetween then "widthBetween" else
        if flag' == heightBetween then "heightBetween" else
        if flag' == behind then "behind" else
        if flag' == heightTextAreaContent then "heightTextAreaContent" else
        if flag' == fontVariant then "fontVariant" else
        "nothing"

instance fieldShow :: Show Field where
    show field =
        Array.concat [ if present transparency field then ["transparency"] else []
        , if present padding field then ["padding"] else []
        , if present spacing field then ["spacing"] else []
        , if present fontSize field then ["fontSize"] else []
        , if present fontFamily field then ["fontFamily"] else []
        , if present width field then ["width"] else []
        , if present height field then ["height"] else []
        , if present bgColor field then ["bgColor"] else []
        , if present bgImage field then ["bgImage"] else []
        , if present bgGradient field then ["bgGradient"] else []
        , if present borderStyle field then ["borderStyle"] else []
        , if present fontAlignment field then ["fontAlignment"] else []
        , if present fontWeight field then ["fontWeight"] else []
        , if present fontColor field then ["fontColor"] else []
        , if present wordSpacing field then ["wordSpacing"] else []
        , if present letterSpacing field then ["letterSpacing"] else []
        , if present borderRound field then ["borderRound"] else []
        , if present txtShadows field then ["txtShadows"] else []
        , if present shadows field then ["shadows"] else []
        , if present overflow field then ["overflow"] else []
        , if present cursor field then ["cursor"] else []
        , if present scale field then ["scale"] else []
        , if present rotate field then ["rotate"] else []
        , if present moveX field then ["moveX"] else []
        , if present moveY field then ["moveY"] else []
        , if present borderWidth field then ["borderWidth"] else []
        , if present borderColor field then ["borderColor"] else []
        , if present yAlign field then ["yAlign"] else []
        , if present xAlign field then ["xAlign"] else []
        , if present focus field then ["focus"] else []
        , if present active field then ["active"] else []
        , if present hover field then ["hover"] else []
        , if present gridTemplate field then ["gridTemplate"] else []
        , if present gridPosition field then ["gridPosition"] else []
        , if present heightContent field then ["heightContent"] else []
        , if present heightFill field then ["heightFill"] else []
        , if present widthContent field then ["widthContent"] else []
        , if present widthFill field then ["widthFill"] else []
        , if present alignRight field then ["alignRight"] else []
        , if present alignBottom field then ["alignBottom"] else []
        , if present centerX field then ["centerX"] else []
        , if present centerY field then ["centerY"] else []
        , if present widthBetween field then ["widthBetween"] else []
        , if present heightBetween field then ["heightBetween"] else []
        , if present behind field then ["behind"] else []
        , if present heightTextAreaContent field then ["heightTextAreaContent"] else []
        , if present fontVariant field then ["fontVariant"] else []
        ]
        # String.joinWith ", "

none :: Field
none =
    Field (Int53.fromInt 0) (Int53.fromInt 0)

logBase :: Number -> Number -> Number
logBase base number = log number / log base

value :: Flag -> Number
value myFlag =
    case myFlag of
        Flag first ->
            round (logBase 2.0 (Int53.toNumber first))

        Second second ->
            round (logBase 2.0 (Int53.toNumber second)) + 32.0


{-| If the query is in the truth, return True
-}
present :: Flag -> Field -> Boolean
present myFlag (Field fieldOne fieldTwo) =
    case myFlag of
        Flag first ->
            Int53.and first fieldOne == first

        Second second ->
            Int53.and second fieldTwo == second


{-| Add a flag to a field.
-}
add :: Flag -> Field -> Field
add myFlag (Field one two) =
    case myFlag of
        Flag first ->
            Field (Int53.or first one) two

        Second second ->
            Field one (Int53.or second two)


{-| Generally you want to use `add`, which keeps a distinction between Fields and Flags.

Merging will combine two fields

-}
merge :: Field -> Field -> Field
merge (Field one two) (Field three four) =
    Field (Int53.or one three) (Int53.or two four)


flag :: Int -> Flag
flag i =
    if i > 31 then
        Second
            (Int53.shl (Int53.fromInt 1) (Int53.fromInt (i - 32)))

    else
        Flag
            (Int53.shl (Int53.fromInt 1) (Int53.fromInt i))



{- Used for Style invalidation -}


transparency =
    flag 0


padding =
    flag 2


spacing =
    flag 3


fontSize =
    flag 4


fontFamily =
    flag 5


width =
    flag 6


height =
    flag 7


bgColor =
    flag 8


bgImage =
    flag 9


bgGradient =
    flag 10


borderStyle =
    flag 11


fontAlignment =
    flag 12


fontWeight =
    flag 13


fontColor =
    flag 14


wordSpacing =
    flag 15


letterSpacing =
    flag 16


borderRound =
    flag 17


txtShadows =
    flag 18


shadows =
    flag 19


overflow =
    flag 20


cursor =
    flag 21


scale =
    flag 23


rotate =
    flag 24


moveX =
    flag 25


moveY =
    flag 26


borderWidth =
    flag 27


borderColor =
    flag 28


yAlign =
    flag 29


xAlign =
    flag 30


focus =
    flag 31


active =
    flag 32


hover =
    flag 33


gridTemplate =
    flag 34


gridPosition =
    flag 35



{- Notes -}


heightContent =
    flag 36


heightFill =
    flag 37


widthContent =
    flag 38


widthFill =
    flag 39


alignRight =
    flag 40


alignBottom =
    flag 41


centerX =
    flag 42


centerY =
    flag 43


widthBetween =
    flag 44


heightBetween =
    flag 45


behind =
    flag 46


heightTextAreaContent =
    flag 47


fontVariant =
    flag 48

