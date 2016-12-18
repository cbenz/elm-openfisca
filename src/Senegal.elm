module Senegal exposing (..)

import Scale exposing (..)
import Value exposing (Value(..))


nbParts : Bool -> Bool -> Int -> Float
nbParts estMarie conjointADesRevenus nbEnfants =
    let
        nbPartsIndividu =
            1

        nbPartsConjoint =
            (if estMarie then
                0.5
             else
                0
            )
                + (if conjointADesRevenus then
                    0
                   else
                    0.5
                  )

        nbPartsEnfants =
            (toFloat nbEnfants) * 0.5
    in
        List.sum [ nbPartsIndividu, nbPartsConjoint, nbPartsEnfants ]
            |> min 5


reductionImpotsPourChargeFamille : Float -> Float -> Result String Float
reductionImpotsPourChargeFamille impotProgressif nbParts =
    let
        data =
            [ ( 1, { taux = 0, minimum = 0, maximum = 0 } )
            , ( 1.5, { taux = 0.1, minimum = 100000, maximum = 300000 } )
            , ( 2, { taux = 0.15, minimum = 200000, maximum = 650000 } )
            , ( 2.5, { taux = 0.2, minimum = 300000, maximum = 1100000 } )
            , ( 3, { taux = 0.25, minimum = 400000, maximum = 1650000 } )
            , ( 3.5, { taux = 0.3, minimum = 500000, maximum = 2030000 } )
            , ( 4, { taux = 0.35, minimum = 600000, maximum = 2490000 } )
            , ( 4.5, { taux = 0.4, minimum = 700000, maximum = 2755000 } )
            , ( 5, { taux = 0.45, minimum = 800000, maximum = 3180000 } )
            ]

        findValue xs getter =
            case xs of
                [] ->
                    Err ("nbParts = " ++ (toString nbParts) ++ " not found")

                ( nbPartsOfItem, values ) :: tail ->
                    if nbParts == nbPartsOfItem then
                        Ok (getter values)
                    else
                        findValue tail getter
    in
        Result.map3
            (\taux minimum maximum ->
                clamp minimum maximum (impotProgressif * taux)
            )
            (findValue data .taux)
            (findValue data .minimum)
            (findValue data .maximum)


baremeImpotProgressif : ScaleWithDate
baremeImpotProgressif =
    let
        start =
            "2013-01-01"

        stop =
            "2013-12-31"
    in
        scaleWithDate
            (MonetaryAmount "CFA")
            [ { thresholds =
                    [ ( start, stop, 0 ) ]
              , rates =
                    [ ( start, stop, 0 ) ]
              }
            , { thresholds =
                    [ ( start, stop, 630000 ) ]
              , rates =
                    [ ( start, stop, 0.2 ) ]
              }
            , { thresholds =
                    [ ( start, stop, 1500000 ) ]
              , rates =
                    [ ( start, stop, 0.3 ) ]
              }
            , { thresholds =
                    [ ( start, stop, 4000000 ) ]
              , rates =
                    [ ( start, stop, 0.35 ) ]
              }
            , { thresholds =
                    [ ( start, stop, 8000000 ) ]
              , rates =
                    [ ( start, stop, 0.37 ) ]
              }
            , { thresholds =
                    [ ( start, stop, 13500000 ) ]
              , rates =
                    [ ( start, stop, 0.4 ) ]
              }
            ]



-- baremeImpotProgressif2013 : Scale
-- baremeImpotProgressif2013 =
--     scale
--         (MonetaryAmount "CFA")
--         [ ( 0, 0 )
--         , ( 630000, 0.2 )
--         , ( 1500000, 0.3 )
--         , ( 4000000, 0.35 )
--         , ( 8000000, 0.37 )
--         , ( 13500000, 0.4 )
--         ]


impotRevenus : Bool -> Bool -> Int -> Float -> Scale -> Result String Float
impotRevenus estMarie conjointADesRevenus nbEnfants salaire bareme =
    let
        impotProgressif =
            Scale.compute salaire bareme

        nbPartsOperation =
            nbParts estMarie conjointADesRevenus nbEnfants
    in
        reductionImpotsPourChargeFamille impotProgressif nbPartsOperation
            |> Result.map
                (\reductionImpotsPourChargeFamille ->
                    max 0 (impotProgressif - reductionImpotsPourChargeFamille)
                )
