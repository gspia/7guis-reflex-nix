{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module GUIs.TemperatureConverter
    ( temperatureConverter
    ) where

import           Reflex.Dom.Core

-- import           Data.Text (Text)
import qualified Data.Text as T

import           Widgets

import           Reflex.Dom.HTML5.Elements (eDivN)
-- import           Reflex.Dom.HTML5.Attrs

temperatureConverter :: MonadWidget t m => m ()
temperatureConverter = eDivN $ mdo
    celsius <- readableInput $ def & textInputConfig_setValue
        .~ ((\x -> (T.pack . show) $ ((x :: Double) - 32) * 5/9) <$> fahrenheit)
    text "Celsius = "

    fahrenheit <- readableInput $ def & textInputConfig_setValue
        .~ ((\x -> (T.pack . show) $ (x :: Double) * 9/5 + 32) <$> celsius)
    text "Fahrenheit"
