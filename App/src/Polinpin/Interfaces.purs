module Polinpin.Interfaces where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)
import Routing.Duplex (RouteDuplex', root, string, segment, record, prop)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Data.Generic.Rep (class Generic)
import Type.Proxy (Proxy(..))

type UserSlug =
    { user :: String
    , slug :: String
    }

userSlug :: RouteDuplex' UserSlug
userSlug =
    record
        # prop (Proxy :: _ "user") (string segment)
        # prop (Proxy :: _ "slug") (string segment)

data Route
    = Home
    | AuthLogin
    | AuthRegister
    | EditDesirabilityStudy UserSlug
    | TakeDesirabilityStudy UserSlug
    | EditTreeTest UserSlug
    | TakeTreeTest UserSlug

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
    { "Home": noArgs
    , "AuthLogin": "auth" / "login" / noArgs
    , "AuthRegister": "auth" / "register" / noArgs
    , "EditDesirabilityStudy": "desirability-studies" / userSlug / "edit"
    , "TakeDesirabilityStudy": "desirability-studies" / userSlug / "take"
    , "EditTreeTest": "tree-tests" / userSlug / "edit"
    , "TakeTreeTest": "tree-tests" / userSlug / "take"
    }

class Monad m <= Navigation m where
    navigate :: Route -> m Unit

instance navigationHalogenM :: Navigation m => Navigation (HalogenM st act slots msg m) where
    navigate = lift <<< navigate
