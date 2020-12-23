module Main exposing (main)

import Array exposing (fromList)
import Html exposing (node, text)
import Html.Attributes exposing (href, rel, style)


logic : String
logic =
    "If Price > 200 then C else if Sold < 100 then R else if Price > 500 && Sold < 20 then H else if RT && TY then BY else if TO && BO then KO else E"


parseCondition : String
parseCondition =
    String.slice (Maybe.withDefault 0 (List.head (String.indexes "If" logic)) + 2) (Maybe.withDefault 0 (List.head (String.indexes "then" logic))) logic


parseConditionTrue : String
parseConditionTrue =
    if String.contains "else if" logic then
        String.slice (Maybe.withDefault 0 (List.head (String.indexes "then" logic)) + 4) (Maybe.withDefault 0 (List.head (String.indexes "else if" logic))) logic

    else
        String.slice (Maybe.withDefault 0 (List.head (String.indexes "then" logic)) + 4) (Maybe.withDefault 0 (List.head (String.indexes "else" logic))) logic


parseConditionFalse : List String -> String -> Int -> List String
parseConditionFalse l str ptr =
    if String.length str > 0 && String.contains "else if" str && (Maybe.withDefault -1 (List.head (String.indexes "else if" (String.slice 8 (String.length str) str))) > -1) then
        parseConditionFalse (List.append (List.append l [ String.slice 8 (Maybe.withDefault 10 (List.head (String.indexes "then" (afterThat str "else if")))) (afterThat str "else if") ]) [ String.slice (Maybe.withDefault 0 (List.head (String.indexes "then" (afterThat (String.slice 1 (String.length str) (afterThat str "then")) "then"))) + 4) (Maybe.withDefault 0 (List.head (String.indexes "else" (afterThat (String.slice 1 (String.length str) (afterThat str "then")) "then")))) (afterThat (String.slice 1 (String.length str) (afterThat str "then")) "then") ]) (String.slice 1 (String.length str) (afterThat str "else if")) (ptr + 1)

    else if String.length str > 0 && String.contains "else if" str && (Maybe.withDefault -1 (List.head (String.indexes "else if" (String.slice 8 (String.length str) str))) == -1) then
        parseConditionFalse (List.append [ String.slice (Maybe.withDefault 0 (List.head (String.indexes "then" (String.slice (Maybe.withDefault 0 (List.head (String.indexes "else if" str))) (String.length str) str)))) (Maybe.withDefault 0 (List.head (String.indexes "else if" str)) + 8) str ++ ".m.m pizza" ] l) (afterThat str "else if") (ptr + 1)

    else
        List.append l [ String.slice (Maybe.withDefault 0 (List.head (String.indexes "else" str)) + 4) (String.length str) str ]


afterThat : String -> String -> String
afterThat str fin =
    String.slice (Maybe.withDefault 10 (List.head (String.indexes fin str))) (String.length str) (String.slice 0 (String.length str) str)


doOne : String -> String
doOne str =
    String.slice (Maybe.withDefault 0 (List.head (String.indexes "then" (String.slice (Maybe.withDefault 0 (List.head (String.indexes "else if" str))) (String.length str) str)))) (Maybe.withDefault 0 (List.head (String.indexes "else if" str)) + 8) str


arrayzTops =
    [ 140, 242, 240, 342, 340, 442, 440, 542, 540 ]


arrayzLeft =
    [ 200, 140, 280, 245, 400, 330, 490, 430, 600 ]


arrayzEdgesTops =
    [ 180, 280, 380, 480, 580, 680, 780 ]


arrayzEdgesLeft =
    [ 160, 255, 350, 445, 540, 635, 730 ]


arrayzTrueEdgesLeft =
    [ 255, 350, 445, 540, 635, 730, 825 ]


main =
    Html.div [ style "display" "flex" ]
        [ Html.div []
            [ Html.div
                [ style "border" "1px solid lime"
                , style "margin-bottom" "40px"
                , style "margin-left" "40px"
                , style "margin-top" "40px"
                , style "position" "absolute"
                , style "left" "40px"
                , style "width" "70px"
                , style "top" "75px"
                , style "height" "0px"
                , style "margin-left" "60px"
                , style "transform" "rotate(-35deg)"
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "flex-start"
                , style "font-family" "Montserrat"
                ]
                [ text "True" ]
            , Html.div
                [ style "border" "1px solid red"
                , style "margin-bottom" "40px"
                , style "margin-left" "40px"
                , style "margin-top" "40px"
                , style "position" "absolute"
                , style "left" "170px"
                , style "width" "70px"
                , style "top" "78px"
                , style "height" "0px"
                , style "margin-left" "60px"
                , style "transform" "rotate(35deg)"
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "flex-start"
                , style "font-family" "Montserrat"
                ]
                [ text "False" ]
            , Html.div
                [ style "border" "1px solid #009fdf"
                , style "margin-bottom" "40px"
                , style "margin-left" "40px"
                , style "margin-top" "40px"
                , style "border-radius" "50%"
                , style "padding" "20px 30px"
                , style "position" "absolute"
                , style "left" "100px"
                , style "font-family" "Montserrat"
                ]
                [ text parseCondition ]
            , Html.div
                [ style "border" "1px solid orange"
                , style "margin-bottom" "40px"
                , style "margin-left" "40px"
                , style "justify-content" "center"
                , style "display" "flex"
                , style "padding" "20px 30px"
                , style "position" "absolute"
                , style "left" "30px"
                , style "top" "140px"
                , style "font-family" "Montserrat"
                ]
                [ text parseConditionTrue ]
            , Html.div []
                (List.indexedMap
                    (\i elm ->
                        if modBy 2 i == 1 then
                            Html.div
                                [ style "border" "1px solid orange"
                                , style "margin-bottom" "40px"
                                , style "margin-left" "40px"
                                , style "justify-content" "center"
                                , style "display" "flex"
                                , style "padding" "20px 30px"
                                , style "position" "absolute"
                                , style "left" (String.append (String.fromInt (Maybe.withDefault 0 (Array.get i (fromList arrayzLeft)))) "px")
                                , style "top" (String.append (String.fromInt (Maybe.withDefault 0 (Array.get i (fromList arrayzTops)))) "px")
                                , style "font-family" "Montserrat"
                                ]
                                [ text elm ]

                        else
                            Html.div
                                [ style "border" "1px solid #009fdf"
                                , style "margin-bottom" "40px"
                                , style "margin-left" "40px"
                                , style "border-radius" "50%"
                                , style "padding" "20px 30px"
                                , style "position" "absolute"
                                , style "font-family" "Montserrat"
                                , style "left" (String.append (String.fromInt (Maybe.withDefault 0 (Array.get i (fromList arrayzLeft)))) "px")
                                , style "top" (String.append (String.fromInt (Maybe.withDefault 0 (Array.get i (fromList arrayzTops)))) "px")
                                , style "font-family" "Montserrat"
                                ]
                                [ text elm ]
                    )
                    (parseConditionFalse [] logic 0)
                )
            , Html.div []
                (List.indexedMap
                    (\i elm ->
                        if i < (List.length (parseConditionFalse [] logic 0) // 2) then
                            Html.div
                                [ style "border" "1px solid lime"
                                , style "margin-bottom" "40px"
                                , style "margin-left" "40px"
                                , style "margin-top" "40px"
                                , style "position" "absolute"
                                , style "width" "70px"
                                , style "height" "0px"
                                , style "margin-left" "60px"
                                , style "transform" "rotate(-35deg)"
                                , style "display" "flex"
                                , style "justify-content" "center"
                                , style "align-items" "flex-start"
                                , style "font-family" "Montserrat"
                                , style "left" (String.append (String.fromInt (Maybe.withDefault 0 (Array.get i (fromList arrayzEdgesLeft)))) "px")
                                , style "top" (String.append (String.fromInt (Maybe.withDefault 0 (Array.get i (fromList arrayzEdgesTops)))) "px")
                                , style "font-family" "Montserrat"
                                ]
                                [ text "True" ]

                        else
                            Html.div [] []
                    )
                    (parseConditionFalse [] logic 0)
                )
            , Html.div []
                (List.indexedMap
                    (\i elm ->
                        if i < (List.length (parseConditionFalse [] logic 0) // 2) then
                            Html.div
                                [ style "border" "1px solid red"
                                , style "margin-bottom" "40px"
                                , style "margin-left" "40px"
                                , style "margin-top" "40px"
                                , style "position" "absolute"
                                , style "width" "70px"
                                , style "height" "0px"
                                , style "margin-left" "60px"
                                , style "transform" "rotate(35deg)"
                                , style "display" "flex"
                                , style "justify-content" "center"
                                , style "align-items" "flex-start"
                                , style "font-family" "Montserrat"
                                , style "left" (String.append (String.fromInt (Maybe.withDefault 0 (Array.get i (fromList arrayzTrueEdgesLeft)))) "px")
                                , style "top" (String.append (String.fromInt (Maybe.withDefault 0 (Array.get i (fromList arrayzEdgesTops)))) "px")
                                , style "font-family" "Montserrat"
                                ]
                                [ text "False" ]

                        else
                            Html.div [] []
                    )
                    (parseConditionFalse [] logic 0)
                )
            , Html.div
                [ style "font-family" "Montserrat"
                , style "margin-top" "10px"
                , style "margin-left" "30px"
                ]
                [ text logic ]
            , node "link" [ href "https://fonts.googleapis.com/css?family=Montserrat", rel "stylesheet" ] []
            ]
        ]
