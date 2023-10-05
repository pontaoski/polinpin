module Main where

import Prelude

import Effect (Effect)
import Polinpin.App (runAppM)
import Halogen.Aff as HA
import Polinpin.Router as Router
import Halogen.VDom.Driver (runUI)
import Halogen (liftEffect)
import Routing.PushState (makeInterface, matchesWith)
import Routing.Duplex (parse)
import Data.Maybe (Maybe(..))
import Effect.Aff (launchAff_)
import Halogen as H
import Polinpin.Interfaces (routeCodec)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  nav <- liftEffect $ makeInterface
  rootComponent <- runAppM {user: Nothing, nav: nav} Router.component
  halogenIO <- runUI rootComponent unit body
  void $ liftEffect $ matchesWith (parse routeCodec) (\old new ->
      when (old /= Just new) $ launchAff_ do
        _response <- halogenIO.query $ H.mkTell $ Router.Navigate new
        pure unit
    ) nav
