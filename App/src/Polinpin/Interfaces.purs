module Polinpin.Interfaces where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Data.Generic.Rep (class Generic)

data Route
    = Home
    | Login

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
    { "Home": noArgs
    , "Login": "login" / noArgs
    }

class Monad m <= Navigation m where
    navigate :: Route -> m Unit

instance navigationHalogenM :: Navigation m => Navigation (HalogenM st act slots msg m) where
    navigate = lift <<< navigate
