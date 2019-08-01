module Json where

import Data.Aeson
import Data.Aeson.Types
import Lib
import Data.Monoid ((<>))

instance ToJSON PersonalityTestResult where
    toJSON p = object (fixedElements <> domains)
        where 
                fixedElements = [ 
                    "NAME"  .= _name p, 
                    "EMAIL" .= _email p
                    ]
                domains = fmap domainToPair (_domains p)
                domainToPair :: Domain -> Pair
                domainToPair dom = _dname dom .= dom


instance ToJSON Domain where
    toJSON d = object ["Overall Score" .= _dscore d, "Facets" .= object facets] where
        facets = fmap facetToPair (_facets d)
        facetToPair :: Facet -> Pair
        facetToPair f = _fname f .= _fscore f

instance ToJSON Score where
    toJSON (Score s) = toJSON s
        