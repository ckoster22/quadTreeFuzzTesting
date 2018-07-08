module QuadTreeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import QuadTree.QuadTree as QuadTree exposing (Point2D, QuadTree)
import Test exposing (..)


-- Fuzzers


pointFuzzer : Fuzzer Point2D
pointFuzzer =
    Fuzz.map2
        Point2D
        (Fuzz.intRange -20 20)
        (Fuzz.intRange -20 20)


pointsFuzzer : Fuzzer (List Point2D)
pointsFuzzer =
    Fuzz.list pointFuzzer
        |> Fuzz.map (List.take 1000)


dupePointsFuzzer : Fuzzer (List Point2D)
dupePointsFuzzer =
    Fuzz.map2
        (\point count -> List.repeat ((abs count % 100) + 2) point)
        pointFuzzer
        Fuzz.int



-- Tests


singlePointTest : Test
singlePointTest =
    fuzz pointFuzzer "Single point in, single point out" <|
        \point ->
            [ point ]
                |> QuadTree.fromList
                |> QuadTree.extract
                |> Expect.equal [ point ]


duplicatePointsTest : Test
duplicatePointsTest =
    fuzz dupePointsFuzzer "Duplicate points should get discarded" <|
        \dupePoints ->
            let
                count =
                    List.length dupePoints

                treeCount =
                    dupePoints |> QuadTree.fromList |> QuadTree.extract |> List.length
            in
                Expect.lessThan count treeCount


northwestPoints : QuadTree -> List Point2D
northwestPoints =
    QuadTree.northwest >> Maybe.map QuadTree.extract >> Maybe.withDefault []


northeastPoints : QuadTree -> List Point2D
northeastPoints =
    QuadTree.northeast >> Maybe.map QuadTree.extract >> Maybe.withDefault []


southwestPoints : QuadTree -> List Point2D
southwestPoints =
    QuadTree.southwest >> Maybe.map QuadTree.extract >> Maybe.withDefault []


southeastPoints : QuadTree -> List Point2D
southeastPoints =
    QuadTree.southeast >> Maybe.map QuadTree.extract >> Maybe.withDefault []


pointsSmaller : List a -> List a -> (a -> comparable) -> Bool
pointsSmaller list1 list2 accessor =
    let
        list2Vals =
            List.map accessor list2
    in
        list1
            |> List.map accessor
            |> List.all
                (\listItem1Val ->
                    List.all (\listItem2Val -> listItem1Val < listItem2Val) list2Vals
                )


pointsOrderTest : Test
pointsOrderTest =
    fuzz pointsFuzzer "QuadTree points should be in order" <|
        \points ->
            Expect.all
                (points |> QuadTree.fromList |> recursivePointsOrderTest)
                ()


recursivePointsOrderTest : QuadTree -> List (subject -> Expectation)
recursivePointsOrderTest quadTree =
    let
        nwPoints =
            northwestPoints quadTree

        nePoints =
            northeastPoints quadTree

        swPoints =
            southwestPoints quadTree

        sePoints =
            southeastPoints quadTree
    in
        [ \_ -> Expect.true "NW x vals less than NE x vals" <| pointsSmaller nwPoints nePoints .x
        , \_ -> Expect.true "NW x vals less than SE x vals" <| pointsSmaller nwPoints sePoints .x
        , \_ -> Expect.true "SW x vals less than SE x vals" <| pointsSmaller swPoints sePoints .x
        , \_ -> Expect.true "SW x vals less than NE x vals" <| pointsSmaller swPoints nePoints .x
        , \_ -> Expect.true "NW y vals less than SW y vals" <| pointsSmaller nwPoints swPoints .y
        , \_ -> Expect.true "NW y vals less than SE y vals" <| pointsSmaller nwPoints sePoints .y
        , \_ -> Expect.true "NE y vals less than SE y vals" <| pointsSmaller nePoints sePoints .y
        , \_ -> Expect.true "NE y vals less than SW y vals" <| pointsSmaller nePoints swPoints .y
        ]
            |> List.append (quadTree |> QuadTree.northwest |> Maybe.map recursivePointsOrderTest |> Maybe.withDefault [])
            |> List.append (quadTree |> QuadTree.northeast |> Maybe.map recursivePointsOrderTest |> Maybe.withDefault [])
            |> List.append (quadTree |> QuadTree.southwest |> Maybe.map recursivePointsOrderTest |> Maybe.withDefault [])
            |> List.append (quadTree |> QuadTree.southeast |> Maybe.map recursivePointsOrderTest |> Maybe.withDefault [])
