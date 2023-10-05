module Element.Events
    ( onClick, onDoubleClick, onMouseDown, onMouseUp, onMouseEnter, onMouseLeave, onMouseMove
    , onFocus, onLoseFocus, on
    -- , onClickCoords
    -- , onClickPageCoords
    -- , onClickScreenCoords
    -- , onMouseCoords
    -- , onMousePageCoords
    -- , onMouseScreenCoords
    )
where

{-|


## Mouse Events

@docs onClick, onDoubleClick, onMouseDown, onMouseUp, onMouseEnter, onMouseLeave, onMouseMove


## Focus Events

@docs onFocus, onLoseFocus

-}

import Prelude

import Element (Attribute)
import Halogen.HTML.Events as Events
import Element.Internal.Model as Internal
import Web.Event.Event (EventType, Event)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Data.Newtype (wrap, unwrap)



-- MOUSE EVENTS


{-| -}
onMouseDown :: forall p i. (MouseEvent -> i) -> Attribute p i
onMouseDown =
    Internal.iprop' <<< Events.onMouseDown


{-| -}
onMouseUp :: forall p i. (MouseEvent -> i) -> Attribute p i
onMouseUp =
    Internal.iprop' <<< Events.onMouseUp


{-| -}
onClick :: forall p i. (MouseEvent -> i) -> Attribute p i
onClick =
    Internal.iprop' <<< Events.onClick


{-| -}
onDoubleClick :: forall p i. (MouseEvent -> i) -> Attribute p i
onDoubleClick =
    Internal.iprop' <<< Events.onDoubleClick


{-| -}
onMouseEnter :: forall p i. (MouseEvent -> i) -> Attribute p i
onMouseEnter =
    Internal.iprop' <<< Events.onMouseEnter


{-| -}
onMouseLeave :: forall p i. (MouseEvent -> i) -> Attribute p i
onMouseLeave =
    Internal.iprop' <<< Events.onMouseLeave


{-| -}
onMouseMove :: forall p i. (MouseEvent -> i) -> Attribute p i
onMouseMove =
    Internal.iprop' <<< Events.onMouseMove

-- FOCUS EVENTS


{-| -}
onLoseFocus ::forall p i. (FocusEvent -> i) -> Attribute p i
onLoseFocus =
    Internal.iprop' <<< Events.onBlur


{-| -}
onFocus :: forall p i. (FocusEvent -> i) -> Attribute p i
onFocus =
    Internal.iprop' <<< Events.onFocus



-- CUSTOM EVENTS


{-| Create a custom event listener. Normally this will not be necessary, but
you have the power! Here is how `onClick` is defined for example:

    import Json.Decode as Json

    onClick :: msg -> Attribute msg
    onClick message =
        on "click" (Json.succeed message)

The first argument is the event name in the same format as with JavaScript's
[`addEventListener`][aEL] function.
The second argument is a JSON decoder. Read more about these [here][decoder].
When an event occurs, the decoder tries to turn the event object into an Elm
value. If successful, the value is routed to your `update` function. In the
case of `onClick` we always just succeed with the given `message`.
If this is confusing, work through the [Elm Architecture Tutorial][tutorial].
It really does help!
[aEL]: <https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener>
[decoder]: <http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode>
[tutorial]: <https://github.com/evancz/elm-architecture-tutorial/>

-}
on :: forall p i. EventType -> (Event -> i) -> Attribute p i
on event =
    Internal.iprop' <<< Events.handler event
