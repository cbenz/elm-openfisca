module Formulas exposing (..)

import OpenFisca exposing (..)
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
        taux =
            Condition
                (Equals (nbParts estMarie conjointADesRevenus nbEnfants) (Number 1))
                (Number 0)
                (Condition
                    (Equals (nbParts estMarie conjointADesRevenus nbEnfants) (Number 1.5))
                    (Number 0.15)
                    (Number 999)
                )

        -- (nbParts == 1.5) * taux_2 + \
        -- (nbParts == 2) * taux_3 + \
        -- (nbParts == 2.5) * taux_4 + \
        -- (nbParts == 3) * taux_5 + \
        -- (nbParts == 3.5) * taux_6 + \
        -- (nbParts == 4) * taux_7 + \
        -- (nbParts == 4.5) * taux_8 + \
        -- (nbParts == 5) * taux_9
        -- minimum = (nbParts == 1) * min_1 + \
        --     (nbParts == 1.5) * min_2 + \
        --     (nbParts == 2) * min_3 + \
        --     (nbParts == 2.5) * min_4 + \
        --     (nbParts == 3) * min_5 + \
        --     (nbParts == 3.5) * min_6 + \
        --     (nbParts == 4) * min_7 + \
        --     (nbParts == 4.5) * min_8 + \
        --     (nbParts == 5) * min_9
        -- maximum = (nbParts == 1) * max_1 + \
        --     (nbParts == 1.5) * max_2 + \
        --     (nbParts == 2) * max_3 + \
        --     (nbParts == 2.5) * max_4 + \
        --     (nbParts == 3) * max_5 + \
        --     (nbParts == 3.5) * max_6 + \
        --     (nbParts == 4) * max_7 + \
        --     (nbParts == 4.5) * max_8 + \
        --     (nbParts == 5) * max_9
    in
        -- |>clip  minimum maximum
        -- Mul (ScaleEvaluation baremeImpotProgressif salaire) taux
        Mul impotProgressif taux


impotRevenus :
    ArithmeticOperation
    -> Scale
    -> BooleanOperation
    -> BooleanOperation
    -> ArithmeticOperation
    -> ArithmeticOperation
impotRevenus salaire baremeImpotProgressif estMarie conjointADesRevenus nbEnfants =
    let
        impotProgressif =
            ScaleEvaluation baremeImpotProgressif salaire
    in
        Add
            impotProgressif
            (Negate
                (reductionImpotsPourChargeFamille impotProgressif estMarie conjointADesRevenus nbEnfants)
            )
            |> Max (Number 0)
