module TagTime exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Time

unixTime : Int -> Time.Posix
unixTime seconds =
  Time.millisToPosix <| t * 1000

toUnixTime : Ping -> Int
toUnixTime ping =
  (toTime ping |> Time.posixToMillis) // 1000

urPingTests : Test
urPingTests =
    test "urPing is a ping" <|
      \_ -> let t0 = 1593671898 in
        Expect.equal t0 (t0 |> unixTime |> mostRecent |> toUnixTime)

prevNextTests : Test
prevNextTests =
  describe "prev/next"
    [ describe "smoke"
        [ test "next" <|
            \_ -> Expect.equal (mostRecent <| unixTime 1533748817) (next <| mostRecent <| unixTime 1533748817)
        ]
    , fuzz (intRange 1210798482 1588379296) "(prev << next) == identity" <|
        \n -> let ping = mostRecent (unixTime n) in
          Expect.equal ping (prev (next ping))
    , fuzz (intRange 1210798482 1588379296) "(next << prev) == identity" <|
        \n -> let ping = mostRecent (unixTime n) in
          Expect.equal ping (next (prev ping))
    , test "can go before epoch" <|
        \_ -> mostRecent (unixTime <| -100000)
    ]
