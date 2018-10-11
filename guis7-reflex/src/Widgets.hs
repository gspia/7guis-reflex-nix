{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MonoLocalBinds #-}

module Widgets where

import           Reflex
import           Reflex.Dom.Core

import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (defaultTimeLocale, parseTimeM)

import           Text.Read        (readMaybe)

import           Utils

import qualified Reflex.Dom.HTML5.Elements as E (buttonD', defButton, defTextArea)
import qualified Reflex.Dom.HTML5.Attrs as A (disabled, style, attrMap)


readableInput ∷ (MonadWidget t m, Read a) ⇒ TextInputConfig t → m (Event t a)
readableInput conf = do
    c ← textInput conf
    pure $ fmapMaybe (readMaybe . T.unpack) $ _textInput_input c

maybeButton ∷ MonadWidget t m
            ⇒ Dynamic t Bool
            -- ^ Is the button enabled?
            → Text
            -- ^ Static button label
            → m (Event t ())
maybeButton enabled label = do
    attrs ← (pure . ffor enabled) $ \e →
      if not e
         then A.disabled E.defButton
         else E.defButton
    (btn, _) ← E.buttonD' attrs $ text label
    pure $ domEvent Click btn

datePicker ∷ MonadWidget t m
           ⇒ Dynamic t Bool
           -- ^ Widget enabled?
           → m (Dynamic t (Maybe UTCTime))
datePicker enabled = do
    rec raw ← textInput $ def & textInputConfig_attributes .~ attrs
        date ← (pure . fmap ((parseTimeM True defaultTimeLocale "%F") . T.unpack))
                $ _textInput_value raw
        attrs ← dynCombine date enabled $ \d e →
          A.attrMap $ -- we need this to get the map from E.TextArea
            monoidGuard (isNothing d) (A.style "color: red" $ E.defTextArea) <>
            monoidGuard (not e) (A.disabled $ E.defTextArea)
    pure date

selectableList ∷ (MonadWidget t m, Ord k)
               ⇒ Dynamic t (Maybe k)
               -- ^ Key of element that may be selected
               → Dynamic t (Map k v)
               -- ^ Map of elements to be shown in the list
               → (Dynamic t Bool → Dynamic t v → m (Event t a))
               -- ^ Action that renders a widget for an element. The element may fire events
               → m (Event t k)
               -- ^ List fires events whenever an element is selected
selectableList selection elems mkEntry = do
    selectEntry ← listWithKey elems $ \k v → do
        isSelected ← ( pure . ffor selection) $ \s → s == Just k
        fmap (const k) <$> mkEntry isSelected v
    switchPromptlyDyn <$> (pure . fmap (leftmost . Map.elems)) selectEntry


