module Polinpin.App where

import Prelude

import Effect.Aff (Aff)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT, getStore)
import Polinpin.Store as Store
import Safe.Coerce (coerce)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Polinpin.Interfaces (class Navigation, routeCodec)
import Routing.Duplex (print)
import Foreign (unsafeToForeign)

newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

runAppM :: forall q i o. Store.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppMM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore Store.Action Store.Store AppM

instance navigationAppM :: Navigation AppM where
    navigate to = do
        { nav } <- getStore
        liftEffect $ nav.pushState (unsafeToForeign {}) (print routeCodec to)
