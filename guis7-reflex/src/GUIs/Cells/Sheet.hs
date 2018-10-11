{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GUIs.Cells.Sheet
    ( SheetState
    , newSheetState
    , MonadSheet(..)
    , runSheet
    ) where

import           Control.Monad              (when)
import           Control.Monad.State.Lazy   (State, evalState, get, gets
                                            , modify)
import           Control.Monad.State.Class  (MonadState)
import           Control.Monad.Except (ExceptT(..), runExceptT)

import           Data.Decimal
import           Data.Monoid                ((<>))
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text (Text)
import qualified Data.Text as T

import           GUIs.Cells.References
import           GUIs.Cells.Types

data SheetState = SheetState
    { ssSize         ∷ Size
    , ssDependencies ∷ ReferenceGraph Coords
    , ssValues       ∷ Map Coords Decimal
    , ssExpressions  ∷ Map Coords Expr
    , ssUpdates      ∷ Map Coords CellResult
    }

newSheetState ∷ Size → SheetState
newSheetState size@(Size w h) = SheetState
    { ssSize = size
    , ssDependencies = mkEmpty (w * h - 1)
                               (\(Coords i j) → j * w + i)
                               (\v → Coords (v `mod` w) (v `div` w))
    , ssValues = Map.empty
    , ssExpressions = Map.empty
    , ssUpdates = Map.empty
    }

class Monad m ⇒ MonadSheet m where
    failEval ∷ Text → m a
    storeCellResult ∷ Coords → CellResult → m ()
    storeExpression ∷ Coords → Expr → m ()
    getValueLookup ∷ m (Coords → Maybe Decimal)
    getExpr ∷ Coords → m Expr
    updateDependencies ∷ Coords → [Coords] → m ()
    getLevels ∷ Coords → m [[Coords]]


newtype Sheet a = Sheet
    { unSheet ∷ ExceptT Text (State SheetState) a
    } deriving (Functor, Applicative, Monad, MonadState SheetState)

runSheet ∷ SheetState →  Sheet a → (SheetState, Either Text (Map Coords CellResult))
runSheet old actions = flip evalState old $ do
    failed ← runExceptT (unSheet actions)
    state ← get
    let cleanedState = state
            { ssUpdates = Map.empty
            }
    case failed of
        Left err → pure (cleanedState, Left err)
        Right _  → pure (cleanedState, Right $ ssUpdates state)

instance MonadSheet Sheet where
    failEval err = Sheet $ ExceptT . pure . Left $ err
    storeCellResult coords result = modify $ \st → st
        { ssValues = case result of
              Right (Number x) → Map.insert coords x (ssValues st)
              _ → Map.delete coords (ssValues st)
        , ssUpdates = Map.insert coords result (ssUpdates st)
        }
    storeExpression coords expr = modify $ \st → st
        { ssExpressions = Map.insert coords expr (ssExpressions st)
        }
    getValueLookup = do
        valueMap ← gets ssValues
        pure $ \c → Map.lookup c valueMap
    getExpr coords = do
        exprMap ← gets ssExpressions
        case Map.lookup coords exprMap of
            Just expr → pure expr
            Nothing → failEval $ "Could not find expression for " <> (T.pack . show) coords
    updateDependencies coords deps = do
        when (coords `elem` deps) $ failEval "Cannot reference self."
        depsGraph ← gets ssDependencies
        size ← gets ssSize
        let depsGraph' = addReferences coords (filter (inBounds size) deps) depsGraph
        when (hasCycle depsGraph') $ failEval "Cyclic references"
        modify $ \st → st { ssDependencies = depsGraph' }
    getLevels coords = do
        deps ← gets ssDependencies
        maybe (failEval "Expected one forest for levels") (pure . tail) $
            levelsAtVertex deps coords

