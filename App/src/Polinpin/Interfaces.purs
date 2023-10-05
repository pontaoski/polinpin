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

data UnauthenticatedRoute
    = Login
    | Register

derive instance genericUnauthRoute :: Generic UnauthenticatedRoute _
derive instance eqUnauthRoute :: Eq UnauthenticatedRoute
derive instance ordUnauthRoute :: Ord UnauthenticatedRoute

data AuthenticatedRoute
    = EditDesirabilityStudy UserSlug
    | EditTreeTest UserSlug

derive instance genericAuthRoute :: Generic AuthenticatedRoute _
derive instance eqAuthRoute :: Eq AuthenticatedRoute
derive instance ordAuthRoute :: Ord AuthenticatedRoute

data Route
    = Home
    | Unauthenticated UnauthenticatedRoute
    | Authenticated AuthenticatedRoute
    | TakeDesirabilityStudy UserSlug
    | TakeTreeTest UserSlug

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

unauthenticatedCodec :: RouteDuplex' UnauthenticatedRoute
unauthenticatedCodec =
    sum
        { "Login": "login" / noArgs
        , "Register": "register" / noArgs
        }

authenticatedCodec :: RouteDuplex' AuthenticatedRoute
authenticatedCodec =
    sum
        { "EditDesirabilityStudy": "desirability-studies" / userSlug / "edit"
        , "EditTreeTest": "tree-tests" / userSlug / "edit"
        }

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
    { "Home": noArgs
    , "Unauthenticated": "auth" / unauthenticatedCodec
    , "Authenticated": authenticatedCodec
    , "TakeDesirabilityStudy": "desirability-studies" / userSlug / "take"
    , "TakeTreeTest": "tree-tests" / userSlug / "take"
    }

class Monad m <= Navigation m where
    navigate :: Route -> m Unit

instance navigationHalogenM :: Navigation m => Navigation (HalogenM st act slots msg m) where
    navigate = lift <<< navigate
