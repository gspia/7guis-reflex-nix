{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GUIs.FlightBooker
    ( flightBooker
    ) where

import           Reflex
import           Reflex.Dom.Core

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

import           Utils
import           Widgets

import           Reflex.Dom.HTML5.Elements
-- import           Reflex.Dom.HTML5.Attrs

data FlightType
    = OneWay
    | Return
    deriving (Eq, Ord, Read, Show)

flightTypeMap :: Map FlightType Text
flightTypeMap = Map.fromList
    [ (OneWay, "one-way flight")
    , (Return, "return flight")
    ]

flightBooker :: MonadWidget t m => m ()
flightBooker = eDivN $ do

    flightType <- dropdown OneWay (constDyn flightTypeMap) def

    text "Depart:"
    mStart <- datePicker (constDyn True)

    -- endEnabled <- forDyn (_dropdown_value flightType) $ \case
    endEnabled <- (return . ffor (_dropdown_value flightType)) $ \case
        OneWay -> False
        Return -> True
    text "Return:"
    mEnd <- datePicker endEnabled

    bookMsg <- dynCombine3 (_dropdown_value flightType) mStart mEnd $ \ft ms me -> case (ft, ms, me) of
        (OneWay, Just s, _) ->
            Just $ "You have booked a one-way flight on " <> show s <> "."
        (Return, Just s, Just e) -> if s < e
            then Just $ "You have booked a return trip from " <> show s <> " to " <> show e <> "."
            else Nothing
        _ ->
            Nothing
    -- bookEnabled <- mapDyn isJust bookMsg
    bookEnabled <- (return . fmap isJust) bookMsg
    bookBtn <- maybeButton bookEnabled "Book!"

    -- resp <- holdDyn Nothing $ tagDyn bookMsg bookBtn
    resp <- holdDyn Nothing $ tagPromptlyDyn bookMsg bookBtn
    -- respStr <- mapDyn (T.pack . show) resp
    respStr <- (return . fmap (T.pack . show)) resp

    dynText respStr
