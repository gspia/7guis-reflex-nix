{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MonoLocalBinds #-}

module GUIs.Timer
    ( timer
    ) where

import           Reflex
import           Reflex.Dom.Core

import           Data.Decimal
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)

import           Widgets

import           Reflex.Dom.HTML5.Elements (divN)
-- import           Reflex.Dom.HTML5.Attrs

data TimerEvent
  = TimerTick Decimal -- limit
  | TimerReset

timer ∷ MonadWidget t m ⇒ UTCTime → m ()
timer t0 = divN $ do
    tick ← tickLossy 0.1 t0
    text "Limit:"
    limit ← readableInput def
    limitDyn ← holdDyn 10.0 limit
    rec let events = leftmost
              [ (\(limit', _) → TimerTick limit') <$> attachPromptlyDyn limitDyn tick
              , const TimerReset <$> reset
              ]
        elapsed ← foldDyn (\ev curr → case ev of
                TimerTick limit' → if curr + 0.1 <= limit' then curr + 0.1 else curr
                TimerReset       → 0.0
            ) (0.0 ∷ Decimal) events
        elapsedText ← (pure . fmap (T.pack . show)) elapsed
        dynText elapsedText
        reset ← button "Reset"
    pure ()


