module Polinpin.Store where

import Data.Maybe (Maybe)
import Routing.PushState (PushStateInterface)

type Store =
    { user :: Maybe String
    , nav :: PushStateInterface
    }
type Action = {}

reduce :: Store -> Action -> Store
reduce store _action =
    store
