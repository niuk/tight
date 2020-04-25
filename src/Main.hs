{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import System.Environment
import System.IO

import Data.String

import Prelude (error, undefined)

import Parse (parse)
import TypeCheck (typeCheck)
import Generator (generateC)

main :: IO ()
main = do
  [inputFile, outputFile] <- getArgs
  source <- readFile inputFile
  let target = compile source
  writeFile outputFile target

compile :: String -> String
compile = generateC . typeCheck . parse
