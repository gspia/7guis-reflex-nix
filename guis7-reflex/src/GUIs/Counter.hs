{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MonoLocalBinds #-}

module GUIs.Counter
    ( counter
    ) where

import           Reflex
import           Reflex.Dom.Core
import qualified Data.Text as T

import qualified Reflex.Dom.HTML5.Elements as E
-- import           Reflex.Dom.HTML5.Attrs

counter ∷ MonadWidget t m ⇒ m ()
counter = E.divN $ do
    click ← button "Click"
    c ← count click
    cStr ← (pure . fmap (T.pack . (show ∷ Int → String))) c
    text "Clicks: "
    dynText cStr
    pure ()

