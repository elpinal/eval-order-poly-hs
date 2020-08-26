module Lib
    ( fromFile
    , fromString
    ) where

import Data.Bifunctor

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec hiding (parse)

import Economical
import EvalOrder
import Parser
import Type

fromFile :: FilePath -> IO ()
fromFile file = do
  v <- parse file <$> TIO.readFile file
  case v of
    Left e -> putStrLn $ errorBundlePretty e
    Right t ->
      case fromSurface t of
        Left e -> putStrLn $ show e
        Right t -> case synth t of
          Left e -> print e
          Right (ty, vn) -> do
            putStrLn $ "valueness: " ++ show vn
            putStrLn $ "type: " ++ show ty

fromString :: String -> Either String (Type Economical, Valueness)
fromString input =
  case parse "<filename>" $ T.pack input of
    Left e -> Left $ errorBundlePretty e
    Right t ->
      case fromSurface t of
        Left e -> Left $ show e
        Right t -> first show $ synth t
