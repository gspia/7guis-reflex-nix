{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Reflex.Dom.Core

import           Control.Monad.IO.Class    (liftIO)

import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Text.Encoding        (encodeUtf8)
import           Data.Time.Clock           (getCurrentTime)

import           GUIs.Cells
import           GUIs.CircleDrawer
import           GUIs.Counter
import           GUIs.CRUD
import           GUIs.FlightBooker
import           GUIs.TemperatureConverter
import           GUIs.Timer

import Language.Javascript.JSaddle (JSM, liftJSM)

#ifdef ghcjs_HOST_OS
#else
import Language.Javascript.JSaddle.WebKitGTK (run)
#endif


main :: IO ()
#ifdef ghcjs_HOST_OS
main = liftJSM mainW
#else
main = run mainW
#endif

-- mainW :: UTCTime -> JSM ()
-- mainW tStart = do
mainW :: JSM ()
mainW = do
  tStart <- liftIO getCurrentTime
  -- mainWidget $ do
  -- mainWidgetWithCss css $ do
  mainWidgetWithCss (encodeUtf8 css) $ do
    header "Counter"
    counter
    header "Temperature Converter"
    temperatureConverter
    header "Flight Booker"
    flightBooker
    header "Timer"
    timer tStart
    header "CRUD"
    crud
    header "Circle Drawer (wrapDomEvent)"
    circleDrawer
    header "Circle Drawer 2 (domEvent)"
    circleDrawer2
    header "Cells"
    cells
  -- where
  --   css = $(embedFile "index.css")

header :: MonadWidget t m => Text -> m ()
header = el "h1" . text

-- On vim:
-- :r index.cc
-- and with visual selection
-- :s/^\(\)$/  <> \"\1\"\\n/g
-- :g/<> \"\\n\"/d
-- and manually correcting font-family -line and the first line.
-- Somehow, the multiline-thing with \ at the end and start didn't work.
-- (Probably some minor thing for which I was blind.)
css :: Text
css = "body, input, button, select {\n"
  <> "  font-size: 12pt;\n"
  <> "  line-height: 1.6;\n"
  <> "  font-weight: 400;\n"
  <> "  font-family: \"Raleway\", \"HelveticaNeue\", \"Helvetica Neue\", Helvetica, Arial, sans-serif;\n"
  <> "  color: #222;\n"
  <> "}\n"
  <> "input, button, select {\n"
  <> "  margin: 5px;\n"
  <> "}\n"
  <> "input {\n"
  <> "  border:2px solid blue\n"
  <> "}\n"
  <> "button:disabled {\n"
  <> "  color: gray;\n"
  <> "}\n"
  <> "h1 {\n"
  <> "  margin-bottom: 0px;\n"
  <> "  font-size: 20pt;\n"
  <> "}\n"
  <> ".cellInput > input {\n"
  <> "  font-family: monospace;\n"
  <> "  margin: 0px;\n"
  <> "  width: 190px;\n"
  <> "  border:5px solid blue\n"
  <> "}\n"
  <> ".cellResult {\n"
  <> "  height: 33.33px;\n"
  <> "  overflow: hidden;\n"
  <> "}\n"
  <> ".cell {\n"
  <> "  position: absolute;\n"
  <> "  width: 190px;\n"
  <> "  height: 90px;\n"
  <> "}\n"
  <> ".sheet {\n"
  <> "  position: relative;\n"
  <> "}\n"


