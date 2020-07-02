module TagTime.Demo exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Events
import Task
import Time

import TagTime exposing (Ping)

type alias Model =
  { timeZone : Time.Zone
  , pings : List Ping
  }

type Msg
  = SetZone Time.Zone
  | GotPing Ping

waitForPingCmd : Cmd Msg
waitForPingCmd = Task.perform GotPing TagTime.waitForPing

main = Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = (\_ -> Sub.none)
  }

init : () -> ( Model , Cmd Msg )
init () =
  ( { timeZone = Time.utc
    , pings = []
    }
  , Cmd.batch
      [ waitForPingCmd
      , Task.perform SetZone Time.here
      ]
  )

view : Model -> Html.Html Msg
view {pings, timeZone} =
  Html.div []
    [ Html.text "Pings since you opened this page:"
    , pings
      |> List.map (TagTime.toTime >> localTimeString timeZone >> Html.text >> List.singleton >> Html.li [])
      |> Html.ul []
    ]

localTimeString : Time.Zone -> Time.Posix -> String
localTimeString zone time =
  (String.fromInt <| Time.toYear zone time)
  ++ " " ++ (Debug.toString <| Time.toMonth zone time)
  ++ " " ++ (String.pad 2 '0' <| String.fromInt <| Time.toDay zone time)
  ++ " " ++ (String.pad 2 '0' <| String.fromInt <| Time.toHour zone time)
  ++ ":" ++ (String.pad 2 '0' <| String.fromInt <| Time.toMinute zone time)
  ++ ":" ++ (String.pad 2 '0' <| String.fromInt <| Time.toSecond zone time)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SetZone zone ->
      ( { model | timeZone = zone }
      , Cmd.none
      )
    GotPing ping ->
      ( { model
        | pings = ping :: model.pings
        }
      , waitForPingCmd
      )
