module Main where

import Language.Javascript.JSaddle (liftJSM)
import MainW (mainW)

main :: IO ()
main = liftJSM mainW

