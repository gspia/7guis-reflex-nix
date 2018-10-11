{-# LANGUAGE OverloadedStrings #-}
module Main where

import MainW (mainW)

import Language.Javascript.JSaddle.WebKitGTK (run)

main :: IO ()
main = run mainW

