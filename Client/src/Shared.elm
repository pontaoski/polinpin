port module Shared exposing
    ( Flags
    , Model
    , Msg
    , User
    , encodeUser
    , init
    , signIn
    , subscriptions
    , update
    , userDecoder
    )

import Browser.Events
import Dict
import Gen.Route
import Http
import Json.Decode as D
import Json.Encode as E
import Network
import Request exposing (Request)



--- MODEL


type alias Flags =
    { user : D.Value
    , dimensions : { width : Int, height : Int }
    }


type alias Model =
    { user : Maybe User
    , dimensions : { width : Int, height : Int }
    }


type alias User =
    { name : String
    , username : String
    , token : String
    }


encodeUser : User -> E.Value
encodeUser user =
    E.object
        [ ( "username", E.string user.username )
        , ( "name", E.string user.name )
        , ( "token", E.string user.token )
        ]


userDecoder : D.Decoder User
userDecoder =
    D.map3 User
        (D.field "username" D.string)
        (D.field "name" D.string)
        (D.field "token" D.string)


type Msg
    = SignIn User
    | SignOut
    | CheckLoginResult String (Result Http.Error Network.UserInformation)
    | NewDimensions Int Int



--- INIT


init : Request -> Flags -> ( Model, Cmd Msg )
init req flags =
    let
        override =
            Maybe.map3 (\a b c -> ( a, b, c ))
                (Dict.get "username" req.query)
                (Dict.get "name" req.query)
                (Dict.get "token" req.query)

        ( user, msg ) =
            case override of
                Just ( username, name, token ) ->
                    ( Just (User name username token)
                    , Cmd.batch
                        [ Network.me (Network.UserSession token) (CheckLoginResult token)
                        , Request.replaceRoute Gen.Route.Home_ { req | query = Dict.empty }
                        ]
                    )

                _ ->
                    case D.decodeValue userDecoder flags.user of
                        Ok u ->
                            ( Just u, Network.me (Network.UserSession u.token) (CheckLoginResult u.token) )

                        Err _ ->
                            ( Nothing, Cmd.none )
    in
    ( Model user flags.dimensions, msg )


signIn : User -> Msg
signIn user =
    SignIn user



--- UPDATE


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        SignIn user ->
            ( { model | user = Just user }, storeUser (encodeUser user) )

        SignOut ->
            ( model, Cmd.none )

        CheckLoginResult tok (Ok info) ->
            ( { model | user = Just (User info.name info.username tok) }, storeUser (encodeUser (User info.name info.username tok)) )

        CheckLoginResult _ (Err _) ->
            ( { model | user = Nothing }, Cmd.none )

        NewDimensions w h ->
            ( { model | dimensions = { width = w, height = h } }, Cmd.none )



--- SUBSCRIPTIONS


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Browser.Events.onResize (\w h -> NewDimensions w h)



--- PORTS


port storeUser : E.Value -> Cmd msg
