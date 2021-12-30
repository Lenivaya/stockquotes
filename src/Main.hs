{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                  ( unless
                                                , when
                                                )
import qualified Data.ByteString.Lazy          as BL
                                                ( readFile
                                                , writeFile
                                                )
import           Data.Csv                       ( decodeByName )
import           Data.Foldable                  ( toList )
import           Data.Text                      ( unpack )

import           Charts
import           HtmlReport
import           Params
import           QuoteData
import           StatReport


main :: IO ()
main = cmdLineParser >>= work

work :: Params -> IO ()
work params = do
  readQuotes fname' >>= generateReports params
  where fname' = fname params

readQuotes :: FilePath -> IO [QuoteData]
readQuotes fpath = do
  csvData <- BL.readFile fpath
  case decodeByName csvData of
    Left  err         -> error err
    Right (_, quotes) -> pure (toList quotes)

generateReports :: (Functor t, Foldable t) => Params -> t QuoteData -> IO ()
generateReports Params {..} quotes = do
  unless silent $ putStr textRpt
  when chart $ plotChart title quotes chartFname
  saveHtml htmlFile htmlRpt
 where
  statInfo' = statInfo quotes
  textRpt   = textReport statInfo'
  htmlRpt   = htmlReport title quotes statInfo' [ chartFname | chart ]

  withCompany prefix = maybe mempty (prefix <>) company
  chartFname = unpack $ "chart" <> withCompany "_" <> ".svg"
  title      = unpack $ "Historical Quotes" <> withCompany " for "

  saveHtml Nothing  _    = pure ()
  saveHtml (Just f) html = BL.writeFile f html
