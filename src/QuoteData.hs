{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
module QuoteData where

import           Data.ByteString.Char8          ( unpack )
import           Data.Csv                       ( FromField(..)
                                                , FromNamedRecord
                                                )
import           Data.Time                      ( Day
                                                , defaultTimeLocale
                                                , parseTimeM
                                                )
import           GHC.Generics                   ( Generic )

data QuoteData = QuoteData
  { day    :: Day
  , volume :: Int
  , open   :: Double
  , close  :: Double
  , high   :: Double
  , low    :: Double
  }
  deriving (Generic, FromNamedRecord)

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

data QField = Open | Close | High | Low | Volume
  deriving (Eq, Ord, Show, Enum, Bounded)

fieldValue :: QField -> QuoteData -> Double
fieldValue Open   = open
fieldValue Close  = close
fieldValue High   = high
fieldValue Low    = low
fieldValue Volume = fromIntegral . volume
