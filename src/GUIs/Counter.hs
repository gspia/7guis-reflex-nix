{-# LANGUAGE OverloadedStrings #-}
module GUIs.Counter
    ( counter
    ) where

import           Reflex
import           Reflex.Dom.Core
import qualified Data.Text as T

import           Reflex.Dom.HTML5.Elements
-- import           Reflex.Dom.HTML5.Attrs

counter :: MonadWidget t m => m ()
counter = eDivN $ do

  click <- button "Click"
  c <- count click
  -- cStr <- mapDyn (show :: Int -> String) c
  -- cStr <- mapDyn (T.pack . (show :: Int -> String)) c
  cStr <- (return . fmap (T.pack . (show :: Int -> String))) c

  text "Clicks: "
  dynText cStr

  pure ()
