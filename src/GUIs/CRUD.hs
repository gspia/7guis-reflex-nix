{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module GUIs.CRUD
    ( crud
    ) where

import           Reflex
import           Reflex.Dom.Core

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

-- import           Utils
import           Widgets

import           Reflex.Dom.HTML5.Elements (eDivN, eLiD', eUlN)
import           Reflex.Dom.HTML5.Attrs (style)

data Person = Person Text Text -- name surname

instance Show Person where
  show (Person name surname) = T.unpack surname <> ", " <> T.unpack name

data DB = DB
    { dbPersons  :: Map Int Person
    , _dbSelected :: Int
    , _dbIndex    :: Int
    } deriving (Show)

initialDB :: DB
initialDB = DB Map.empty 0 0

data DBCommand
    = DBInsert Person
    | DBUpdate Int Person
    | DBDelete Int
    | DBSelect Int

updateDB :: DBCommand -> DB -> DB
updateDB cmd (DB persons sel ind) = case cmd of
    DBInsert p   -> DB (Map.insert ind p persons) sel (ind + 1)
    DBUpdate i p -> DB (Map.update (const $ Just p) i persons) sel ind
    DBDelete i   -> DB (Map.delete i persons) sel ind
    DBSelect i   -> DB persons i ind

selected :: DB -> Maybe Int
selected (DB persons sel _) = if Map.member sel persons
    then Just sel
    else Nothing

crud :: MonadWidget t m => m ()
crud = eDivN $ do
    text "Name:"
    name <- textInput def
    text "Surname:"
    surname <- textInput def
    -- person <- combineDyn Person (_textInput_value name) (_textInput_value surname)
    person <- return (zipDynWith Person (_textInput_value name) (_textInput_value surname))

    rec db <- foldDyn updateDB initialDB updates
        -- persons <- mapDyn dbPersons db
        -- selectedPerson <- mapDyn selected db
        -- isPersonSelected <- mapDyn isJust selectedPerson
        persons <- (return . fmap dbPersons) db
        selectedPerson <- (return . fmap selected) db
        isPersonSelected <- (return . fmap isJust) selectedPerson

        select <- eUlN $ selectableList selectedPerson persons $ \sel p -> do
          -- attrs <- (return . fmap (\s -> monoidGuard s $ "style" =: "font-weight: bold")) sel
          -- attrs <- (return . fmap (\s -> monoidGuard s $
          attrs <- (return . fmap (\s ->
            if s
               then style "font-weight: bold" $ def
               else def )) sel
          domEvent Click . fst <$> eLiD' attrs (display p)

        createClick <- button "Create"
        updateClick <- maybeButton isPersonSelected "Update"
        deleteClick <- maybeButton isPersonSelected "Delete"

        -- personToUpdate <- combineDyn (,) selectedPerson person
        personToUpdate <- return (zipDynWith (,) selectedPerson person)

        let updates = leftmost
              [ DBDelete <$> fmapMaybe id (tag (current selectedPerson) deleteClick)
              , fmapMaybe (\(mSel, p) -> case mSel of
                    Nothing -> Nothing
                    Just sel -> Just $ DBUpdate sel p
                  ) $ tag (current personToUpdate) updateClick
              , DBInsert <$> tag (current person) createClick
              , DBSelect <$> select
              ]

    pure ()
