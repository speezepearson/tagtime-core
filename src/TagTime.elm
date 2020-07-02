{--
Implementation of the TagTime Universal Ping Schedule
Spec: https://forum.beeminder.com/t/official-reference-implementation-of-the-tagtime-universal-ping-schedule/4282

This module is written in a less-than-maximally Elm-like style in order to stay easily comparable to the reference implementation.

--}

module TagTime exposing
  ( Ping
  , toTime
  , next
  , prev
  , mostRecent
  , waitForPing
  , meanGap
  , urPing
  )

import Dict exposing (Dict)
import Process
import Task exposing (Task)
import Time


type Ping = Ping
  { meanGap : Int
  , lastPingUnixTime : Int
  , lcg : Lcg
  }



-- REFERENCE IMPLEMENTATION, APPROXIMATELY VERBATIM

urPing : Ping
urPing = Ping
  { meanGap          = 45*60        -- Average gap between pings, in seconds
  , lastPingUnixTime = 1184097393   -- Ur-ping ie the birth of Timepie/TagTime! (unixtime)
  , lcg              = Lcg 11193462 -- Initial state of the random number generator
  }

-- Above URPING is in 2007 and it's fine to jump to any later URPING/SEED pair
-- like this one in 2018 -- URPING = 1532992625, SEED = 75570 -- without
-- deviating from the universal ping schedule.

ia : Int
ia = 16807          -- =7^5: Multiplier for LCG random number generator
im : Int
im = 2147483647     -- =2^31-1: Modulus used for the RNG

type Lcg = Lcg Int
stepLcg : Lcg -> (Int, Lcg)
stepLcg (Lcg seed) =
  let
    newSeed = mulModIm ia seed
  in
    (newSeed, Lcg newSeed)

expRand : Float -> Lcg -> (Float, Lcg)
expRand scale lcg =
  let
    (uniformRand, newLcg) = stepLcg lcg
    result = -scale * logBase e (toFloat uniformRand / toFloat im)
  in
    (result, newLcg)

nextGap : Int -> Lcg -> (Int, Lcg)
nextGap mean lcg =
  let
    (eRand, newLcg) = expRand (toFloat mean) lcg
  in
    (max 1 (round eRand), newLcg)

next : Ping -> Ping
next ping =
  let
    (Ping internals) = ping
    (gap, newLcg) = nextGap internals.meanGap internals.lcg
    resultUnixTime = internals.lastPingUnixTime + gap
  in
    Ping { internals | lcg = newLcg , lastPingUnixTime = resultUnixTime }



-- MUST-HAVE UTILITIES FOR MANIPULATING PINGS

meanGap : Ping -> Int
meanGap (Ping internals) =
  internals.meanGap

toTime : Ping -> Time.Posix
toTime (Ping {lastPingUnixTime}) =
  Time.millisToPosix <| lastPingUnixTime * 1000

prev : Ping -> Ping
prev ping =
  let
    (Ping internals) = ping
    prevLcg = retreat internals.lcg
    (prevGap, _) = nextGap internals.meanGap prevLcg
    resultUnixTime = internals.lastPingUnixTime - prevGap
  in
    Ping { internals | lcg = prevLcg , lastPingUnixTime = resultUnixTime }

mostRecent : Time.Posix -> Ping
mostRecent target =
  let
    step : Ping -> Ping
    step =
      if target |> isAfter (toTime urPing) then
        next
      else
        prev

    tailRecurse : Ping -> Ping
    tailRecurse ping =
      if (toTime (next ping) |> isAfter target) && not (toTime ping |> isAfter target) then
        ping
      else
        tailRecurse (step ping)
  in
    tailRecurse (closestCachedPingTo target)


-- WAITING FOR PINGS

waitForPing : Ping -> Task x Ping
waitForPing prevPing =
  let
    nextPing = next prevPing
  in
    Time.now
    |> Task.andThen (\now ->
        Process.sleep (toFloat <| millisBetween now (toTime nextPing))
      )
    |> Task.map (always nextPing)


-- INTERNAL UTILITIES

iaInv = 1407677000  -- Multiplicative inverse of ia; used to step backward.

retreat : Lcg -> Lcg
retreat (Lcg seed) =
  Lcg <| mulModIm seed iaInv

-- Elm's Int arithmetic is quirky for numbers above 2^32.
-- This means we can't use normal multiplication for elements of the modular field:
--   we need a special algorithm that carefully avoids ever going outside the interval [-2^32, 2^32).
-- This implementation is adapted from Stack Overflow's inimitable "Matt":
-- https://stackoverflow.com/questions/21030153/modulo-of-multiplication-of-large-numbers/21032389#21032389
mulModIm : Int -> Int -> Int
mulModIm =
  let

    addModIm : Int -> Int -> Int
    addModIm x y =
      let
        z = x - im + y
      in
        if z < 0 then
          z + im
        else
          z

    tailRecurse : Int -> Int -> Int -> Int
    tailRecurse product x y =
      let
        smallX = x |> modBy im
        smallY = y |> modBy im
        a = min x y
        b = max x y
      in
        if a == 0 then
          product
        else
          tailRecurse
            ( if (a |> modBy 2) == 1 then
                addModIm product b
              else
                product
            )
            (a // 2)
            (addModIm b b)
  in
    tailRecurse 0


isAfter : Time.Posix -> Time.Posix -> Bool
isAfter t1 t2 =
  Time.posixToMillis t2 > Time.posixToMillis t1

millisBetween : Time.Posix -> Time.Posix -> Int
millisBetween t0 tf =
  Time.posixToMillis tf - Time.posixToMillis t0


closestCachedPingTo : Time.Posix -> Ping
closestCachedPingTo =
  let
    cachedPings : Dict Int Ping
    cachedPings =
      Dict.fromList
      <| List.map (\ping -> let (Ping {lastPingUnixTime}) = ping in (lastPingUnixTime, ping))
      <|[ Ping { lastPingUnixTime = -2866, lcg = Lcg 2099573786, meanGap = 2700 } -- epoch
        , urPing
        , Ping { lastPingUnixTime = 1210798482, lcg = Lcg 146250425, meanGap = 2700 }
        , Ping { lastPingUnixTime = 1265233028, lcg = Lcg 524132708, meanGap = 2700 }
        , Ping { lastPingUnixTime = 1319301223, lcg = Lcg 1594955303, meanGap = 2700 }
        , Ping { lastPingUnixTime = 1372711669, lcg = Lcg 1815482626, meanGap = 2700 }
        , Ping { lastPingUnixTime = 1426730819, lcg = Lcg 604164177, meanGap = 2700 }
        , Ping { lastPingUnixTime = 1480347960, lcg = Lcg 871999208, meanGap = 2700 }
        , Ping { lastPingUnixTime = 1534182392, lcg = Lcg 1043395948, meanGap = 2700 }
        , Ping { lastPingUnixTime = 1588379296, lcg = Lcg 1731503183, meanGap = 2700 } -- time of writing
        , Ping { lastPingUnixTime = 1642674203, lcg = Lcg 75660533, meanGap = 2700 }
        , Ping { lastPingUnixTime = 1696919026, lcg = Lcg 658414393, meanGap = 2700 }
        , Ping { lastPingUnixTime = 1750366433, lcg = Lcg 358401311, meanGap = 2700 }
        , Ping { lastPingUnixTime = 1804564790, lcg = Lcg 87431681, meanGap = 2700 }
        , Ping { lastPingUnixTime = 1858581381, lcg = Lcg 2049909891, meanGap = 2700 }
        , Ping { lastPingUnixTime = 1912691903, lcg = Lcg 1566488548, meanGap = 2700 }
        , Ping { lastPingUnixTime = 1966908261, lcg = Lcg 252853066, meanGap = 2700 }
        ]
  in
    (\target ->
      let
        targetUnixTime = Time.posixToMillis target // 1000

        distance : Ping -> Int
        distance ping =
          let (Ping {lastPingUnixTime}) = ping in abs (lastPingUnixTime - targetUnixTime)

        reduce : Int -> Ping -> (Int, Ping) -> (Int, Ping)
        reduce millis ping (minDistance, winner) =
          let
            newDistance = distance ping
          in
            if newDistance < minDistance then
              (newDistance, ping)
            else
              (minDistance, winner)
      in
        Dict.foldl reduce (targetUnixTime-1184097393, urPing) cachedPings
        |> Tuple.second
    )


-- DEBUGGING STUFF THAT I SHOULD PACKAGE INTO TESTS

firstFewPings : List Ping
firstFewPings =
  let
    p0 = urPing
    p1 = next p0
    p2 = next p1
    p3 = next p2
    p4 = next p3
  in
    [p0, p1, p2, p3, p4]

{-- TODO: tests

List.Extra.unfoldr
  (\p -> if (next p |> Time.posixToMillis) > 1184105815000 then Nothing else Just (next p))
  urPing
== List.map (\sec -> Time.millisToPosix (sec*1000))
    [ 1184098754
    , 1184102685
    , 1184104776
    , 1184105302
    , 1184105815
    ]

(next <| advanceUntil <| Time.millisToPosix 1184104776000) == Time.millisToPosix 1184105302000

--}
