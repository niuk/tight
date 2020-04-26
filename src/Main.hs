module Main where

import Data.Text
import Data.Text.Encoding as E
import Data.ByteString as BS

import System.Environment

import Types
import Parser (parse)
import TypeChecker (typeCheck)
import Generator (generateC)

main :: IO ()
main = do
  [inputFile, outputFile] <- getArgs
  source <- E.decodeUtf8 <$> BS.readFile inputFile
  let target = compile source
  BS.writeFile outputFile (E.encodeUtf8 target)

compile :: Text -> Text
compile = generateC . typeCheck . parse
