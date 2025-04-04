{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module StatReport where


import           Colonnade
import           Data.Foldable                  ( maximumBy
                                                , minimumBy
                                                )
import           Data.Ord                       ( comparing )
import           Data.Time                      ( diffDays )
import           Fmt

import           QuoteData

decimalPlacesFloating :: Int
decimalPlacesFloating = 2

data StatValue = StatValue
  { decimalPlaces :: Int
  , value         :: Double
  }

data StatEntry = StatEntry
  { qfield            :: QField
  , meanVal           :: StatValue
  , minVal            :: StatValue
  , maxVal            :: StatValue
  , daysBetweenMinMax :: Int
  }

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / fromIntegral (length xs)

computeMinMaxDays
  :: (Ord a, Foldable t) => (QuoteData -> a) -> t QuoteData -> (a, a, Int)
computeMinMaxDays get quotes = (get minQuote, get maxQuote, days)
 where
  cmp      = comparing get
  minQuote = minimumBy cmp quotes
  maxQuote = maximumBy cmp quotes
  days     = fromIntegral $ abs $ diffDays (day minQuote) (day maxQuote)


statInfo :: (Functor t, Foldable t) => t QuoteData -> [StatEntry]
statInfo quotes = fmap qFieldStatInfo [minBound .. maxBound]
 where
  decimalPlacesByQField Volume = 0
  decimalPlacesByQField _      = decimalPlacesFloating

  qFieldStatInfo qfield =
    let get                         = fieldValue qfield
        (mn, mx, daysBetweenMinMax) = computeMinMaxDays get quotes
        decPlaces                   = decimalPlacesByQField qfield
        meanVal = StatValue decimalPlacesFloating (mean $ fmap get quotes)
        minVal                      = StatValue decPlaces mn
        maxVal                      = StatValue decPlaces mx
    in  StatEntry { .. }

instance Buildable StatValue where
  build sv = fixedF (decimalPlaces sv) (value sv)

instance Buildable StatEntry where
  build StatEntry {..} =
    "Stats for "
      +|| qfield
      ||+ ": "
      +|  meanVal
      |+  " (mean), "
      +|  minVal
      |+  " (min), "
      +|  maxVal
      |+  " (max), "
      +|  daysBetweenMinMax
      |+  " (days)"

showPrice :: Double -> Builder
showPrice = fixedF decimalPlacesFloating

textReport :: [StatEntry] -> String
textReport = ascii colStats
 where
  colStats = mconcat
    [ headed "Quote Field"          (show . qfield)
    , headed "Mean"                 (pretty . meanVal)
    , headed "Min"                  (pretty . minVal)
    , headed "Max"                  (pretty . maxVal)
    , headed "Days between Min/Max" (pretty . daysBetweenMinMax)
    ]
