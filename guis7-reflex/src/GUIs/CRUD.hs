{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MonoLocalBinds #-}

module GUIs.CRUD
    ( crud
    ) where

import           Reflex
import           Reflex.Dom.Core

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T

import           Widgets

import           Reflex.Dom.HTML5.Elements (divN, liD', ulN, defLi)
import           Reflex.Dom.HTML5.Attrs (style)

data Person = Person Text Text -- name surname

instance Show Person where
  show (Person name surname) = T.unpack surname <> ", " <> T.unpack name

data DB = DB
    { _dbPersons  ∷ Map Int Person
    , _dbSelected ∷ Int
    , _dbIndex    ∷ Int
    } deriving (Show)

initialDB ∷ DB
initialDB = DB Map.empty 0 0

data DBCommand
    = DBInsert Person
    | DBUpdate Int Person
    | DBDelete Int
    | DBSelect Int

updateDB ∷ DBCommand → DB → DB
updateDB cmd (DB persons sel ind) = case cmd of
    DBInsert p   → DB (Map.insert ind p persons) sel (ind + 1)
    DBUpdate i p → DB (Map.update (const $ Just p) i persons) sel ind
    DBDelete i   → DB (Map.delete i persons) sel ind
    DBSelect i   → DB persons i ind

queryDB ∷ DB → Int → Maybe Person
queryDB db i = Map.lookup i (_dbPersons db)

selected ∷ DB → Maybe Int
selected (DB persons sel _) = if Map.member sel persons
    then Just sel
    else Nothing

crud ∷ MonadWidget t m ⇒ m ()
crud = divN $ mdo
    -- Which one is better default: change the input box contents when user clicks on 
    -- names (e.g. if planning to change something, it might be easier to select the
    -- record first)
    -- or
    -- not change the input box contents, thus if willing to do "mass updates" for
    -- several records, it is easier if the input box contents remain???
    text "Name:"
    name ← textInput def {_textInputConfig_setValue = updateFN}
    text "Surname:"
    surname ← textInput def {_textInputConfig_setValue = updateSN}
    person ← pure (zipDynWith Person (_textInput_value name) (_textInput_value surname))
    db ← foldDyn updateDB initialDB updates
    persons ← (pure . fmap _dbPersons) db
    selectedPerson ← (pure . fmap selected) db
    isPersonSelected ← (pure . fmap isJust) selectedPerson
    select ∷ Event t Int ←
        ulN $ selectableList selectedPerson persons $ \sel p → do
            attrs ← (pure . fmap (\s →
              if s
                 then style "font-weight: bold" $ defLi
                 else defLi )) sel
            domEvent Click . fst <$> liD' attrs (display p)
    createClick ← button "Create"
    updateClick ← maybeButton isPersonSelected "Update"
    deleteClick ← maybeButton isPersonSelected "Delete"
    personToUpdate ∷ Dynamic t (Maybe Int, Person) ← pure (zipDynWith (,) selectedPerson person)
    let updates = leftmost
          [ DBDelete <$> fmapMaybe id (tag (current selectedPerson) deleteClick)
          , fmapMaybe (\(mSel, p) → case mSel of
                Nothing → Nothing
                Just sel → Just $ DBUpdate sel p
              ) $ tag (current personToUpdate) updateClick
          , DBInsert <$> tag (current person) createClick
          , DBSelect <$> select
          ]
    let updateNames = fmapMaybe (\(d,i) → queryDB d i) $ attach (current db) select
        updateFN = fmap (\(Person fn _sn) → fn) updateNames
        updateSN = fmap (\(Person _fn sn) → sn) updateNames
    pure ()


