module Senegal exposing (..)

import Operations exposing (..)
import Types exposing (..)


nbParts : BooleanOperation -> BooleanOperation -> ArithmeticOperation -> ArithmeticOperation
nbParts estMarie conjointADesRevenus nbEnfants =
    let
        nbPartsPersonne =
            Number 1

        nbPartsConjoint =
            Add
                (Condition estMarie (Number 0.5) (Number 0))
                (Condition conjointADesRevenus (Number 0) (Number 0.5))

        nbPartsEnfants =
            Mul nbEnfants (Number 0.5)
    in
        add3 nbPartsPersonne nbPartsConjoint nbPartsEnfants
            |> Min (Number 5)


reductionImpotsPourChargeFamille :
    ArithmeticOperation
    -> BooleanOperation
    -> BooleanOperation
    -> ArithmeticOperation
    -> ArithmeticOperation
reductionImpotsPourChargeFamille impotProgressif estMarie conjointADesRevenus nbEnfants =
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

        nbPartsValue =
            nbParts estMarie conjointADesRevenus nbEnfants

        valueHelp xs getter =
            case xs of
                [] ->
                    Number 0

                ( nbParts, values ) :: rest ->
                    Condition
                        (Equals nbPartsValue (Number nbParts))
                        (Number (getter values))
                        (valueHelp rest getter)

        taux =
            valueHelp data .taux

        minimum =
            valueHelp data .minimum

        maximum =
            valueHelp data .maximum
    in
        -- Mul (ScaleEvaluation baremeImpotProgressif salaire) taux
        Mul impotProgressif taux
            |> clip minimum maximum


baremeImpotProgressif2013 : Scale
baremeImpotProgressif2013 =
    scale
        (MonetaryAmount "CFA")
        [ ( 0, 0 )
        , ( 630000, 0.2 )
        , ( 1500000, 0.3 )
        , ( 4000000, 0.35 )
        , ( 8000000, 0.37 )
        , ( 13500000, 0.4 )
        ]


impotRevenus :
    ArithmeticOperation
    -> BooleanOperation
    -> BooleanOperation
    -> ArithmeticOperation
    -> ArithmeticOperation
impotRevenus salaire estMarie conjointADesRevenus nbEnfants =
    let
        impotProgressif =
            ScaleEvaluation baremeImpotProgressif2013 salaire
    in
        Add
            impotProgressif
            (Negate
                (reductionImpotsPourChargeFamille impotProgressif estMarie conjointADesRevenus nbEnfants)
            )
            |> Max (Number 0)
