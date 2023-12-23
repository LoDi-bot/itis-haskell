{-# LANGUAGE OverloadedStrings #-}

module MyLib where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

data CSV = CSV { csvHeader :: [String], csvRows :: [[String]] } deriving (Show, Eq)

type ParserError = ParseErrorBundle String Void

parseCSV :: Char -> Bool -> String -> Either ParserError CSV
parseCSV separator hasHeader input =
  parse csvParser "" input
  where
    eol = char '\n'
    cellContent = many (noneOf ['"', separator, '\n'])
    quotedCellContent = char '"' >> many (char '\"' <|> noneOf ['"']) <* char '"'
    cell = cellContent <|> quotedCellContent
    row = cell `sepBy` char separator
    headerRow = row <* eol
    bodyRow = row <* eol
    csvParser = do
      header <- if hasHeader then headerRow else pure []
      rows <- many bodyRow
      let numColumnsHeader = length header
      let invalidRow = filter (\r -> length r /= numColumnsHeader) rows
      if not (null invalidRow) && hasHeader
        then fail "Invalid number of columns in some rows."
        else pure (CSV header rows)
