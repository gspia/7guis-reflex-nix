{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MonoLocalBinds #-}

module GUIs.TemperatureConverter
    ( temperatureConverter
    ) where

import           Reflex.Dom.Core
import qualified Data.Text as T
import           Widgets
import           Reflex.Dom.HTML5.Elements (divN)
-- import           Reflex.Dom.HTML5.Attrs

temperatureConverter ∷ MonadWidget t m ⇒ m ()
temperatureConverter = divN $ mdo
    celsius ← readableInput $ def & textInputConfig_setValue
        .~ ((\x → (T.pack . show) $ ((x ∷ Double) - 32) * 5/9) <$> fahrenheit)
    text "Celsius = "
    fahrenheit ← readableInput $ def & textInputConfig_setValue
        .~ ((\x → (T.pack . show) $ (x ∷ Double) * 9/5 + 32) <$> celsius)
    text "Fahrenheit"


