module ScaleSamples exposing (..)

import Scale exposing (..)
import Senegal


baremeImpotFrance2014 =
    scale
        (MonetaryAmount "€")
        [ ( 0, 0 )
        , ( 6011, 0.055 )
        , ( 11991, 0.14 )
        , ( 26631, 0.3 )
        , ( 71397, 0.41 )
        , ( 151200, 0.45 )
        ]


baremeImpotFrance2015 =
    scale
        (MonetaryAmount "€")
        [ ( 0, 0 )
        , ( 9690, 0.14 )
        , ( 26764, 0.3 )
        , ( 71754, 0.41 )
        , ( 151956, 0.45 )
        ]


baremeImpotFrance =
    scaleWithDate
        (MonetaryAmount "€")
        [ { thresholds =
                [ ( "2014-01-01", "2015-12-31", 0 )
                ]
          , rates =
                [ ( "2014-01-01", "2015-12-31", 0 )
                ]
          }
        , { thresholds =
                [ ( "2014-01-01", "2014-12-31", 6011 )
                , ( "2015-01-01", "2015-12-31", 9690 )
                ]
          , rates =
                [ ( "2014-01-01", "2014-12-31", 0.055 )
                , ( "2015-01-01", "2015-12-31", 0.14 )
                ]
          }
        , { thresholds =
                [ ( "2014-01-01", "2014-12-31", 11991 )
                , ( "2015-01-01", "2015-12-31", 26764 )
                ]
          , rates =
                [ ( "2014-01-01", "2014-12-31", 0.14 )
                , ( "2015-01-01", "2015-12-31", 0.3 )
                ]
          }
        , { thresholds =
                [ ( "2014-01-01", "2014-12-31", 26631 )
                , ( "2015-01-01", "2015-12-31", 71754 )
                ]
          , rates =
                [ ( "2014-01-01", "2014-12-31", 0.3 )
                , ( "2015-01-01", "2015-12-31", 0.41 )
                ]
          }
        , { thresholds =
                [ ( "2014-01-01", "2014-12-31", 71397 )
                , ( "2015-01-01", "2015-12-31", 151956 )
                ]
          , rates =
                [ ( "2014-01-01", "2014-12-31", 0.41 )
                , ( "2015-01-01", "2015-12-31", 0.45 )
                ]
          }
        , { thresholds =
                [ ( "2014-01-01", "2014-12-31", 151200 )
                ]
          , rates =
                [ ( "2014-01-01", "2014-12-31", 0.45 )
                ]
          }
        ]


baremeReductionsPourChargeDeFamille =
    scale
        Amount
        [ ( 1, 0 )
        , ( 1.5, 0.1 )
        , ( 2, 0.15 )
        , ( 2.5, 0.2 )
        , ( 3, 0.25 )
        , ( 3.5, 0.3 )
        , ( 4, 0.35 )
        , ( 4.5, 0.4 )
        , ( 5, 0.45 )
        ]


scales =
    [ baremeImpotFrance2014
    , baremeImpotFrance2015
    , Senegal.baremeImpotProgressif2013
    , baremeReductionsPourChargeDeFamille
    ]


timeChangingScales =
    [ baremeImpotFrance
    , Senegal.baremeImpotProgressif
    ]
