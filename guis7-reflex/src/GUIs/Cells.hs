{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MonoLocalBinds #-}

module GUIs.Cells
    ( cells
    ) where

import           Reflex
import           Reflex.Dom.Core

import           Control.Monad     (when)

import           Data.Decimal
import           Data.Foldable     (for_)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Monoid       ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable  (for)

import           GUIs.Cells.Parser
import           GUIs.Cells.Sheet
import           GUIs.Cells.Types

import           Utils

import qualified Reflex.Dom.HTML5.Elements as E
import qualified Reflex.Dom.HTML5.Attrs as A (addClass, style)

updateSheetState ∷ (Coords, Text) → SheetState
                 → (SheetState, Either Text (Map Coords CellResult))
updateSheetState (coords, expr) oldState = runSheet oldState (eval coords expr)

eval ∷ MonadSheet m ⇒ Coords → Text → m ()
eval coords expr = do
    if expr == ""
        then do
            storeCellResult coords $ Right Empty
            updateDependencies coords []
        else case parseExpression expr of
            Left err → do
              storeCellResult coords $ Left $ ParseError $ (T.pack . show) err
              updateDependencies coords []
            Right parsedExpr → do
                storeExpression coords parsedExpr
                updateDependencies coords $ getExprDeps parsedExpr
                valueLookup ← getValueLookup
                storeCellResult coords $ embedEvalResult $ evalCell valueLookup parsedExpr
    levels ← getLevels coords
    for_ levels $ \level → do
        valueLookup' ← getValueLookup
        for level $ \coords' → do
            expr' ← getExpr coords'
            storeCellResult coords' $ embedEvalResult $ evalCell valueLookup' expr'

evalCell ∷ (Coords → Maybe Decimal) → Expr → EvalResult
evalCell valueLookup ex = case ex of
    ERef coords → case valueLookup coords of
        Just val → pure val
        Nothing → Left $ RefNotFound coords
    EBinOp op left right → do
        leftRes ← evalCell valueLookup left
        rightRes ← evalCell valueLookup right
        case op of
            Plus  → pure $ leftRes + rightRes
            Minus → pure $ leftRes - rightRes
            Times → pure $ leftRes * rightRes
            Div → do
                when (rightRes == 0) $ Left DivByZero
                pure $ leftRes / rightRes
    EUnOp op ex' → case op of
        Negate → do
          res ← evalCell valueLookup ex'
          pure $ -1 * res
    ENumber x → pure x

-- Reflex stuff

cell ∷ MonadWidget t m
     ⇒ Coords
     → CellResult
     → Event t CellResult
     → m (Event t Text)
cell (Coords i j) initialResult resultUpdate =
    E.div aC $ do
        raw ← E.div (A.addClass "cellInput" E.defDiv) $ textInput def
        cellResult ← holdDyn initialResult resultUpdate
        cellResultText ← (pure . fmap showCellResult) cellResult
        E.div (A.addClass "cellResult" E.defDiv) $ dynText cellResultText
        pure $ _textInput_input raw
  where
    aC = A.addClass "cell" $ A.style styStr $ E.defDiv
    styStr = "left:"
        <> ((T.pack . show) (i * 210))
        <> "px;top:"
        <> ((T.pack . show) (j * 70) <> "px")


sheet ∷ MonadWidget t m
      ⇒ Map Coords CellResult
      → Event t (Map Coords CellResult)
      → m (Event t (Coords, Text))
sheet initialResults updateResults =
    E.div (A.addClass "sheet" E.defDiv) $ do
        dynEventMap ← listWithKeyShallowDiff initialResults
            (fmap Just <$> updateResults) $ \c v e → cell c v e
        dynEvent ← (pure . fmap (leftmost . map (\(k, e)
                    → (\ex → (k, ex)) <$> e) . Map.toList)) dynEventMap
        pure $ switchPromptlyDyn dynEvent


cells ∷ MonadWidget t m ⇒ m ()
cells = E.divN $ mdo
    text "Reference other cells with {i,j}, for example top-left is {0,0}. "
    E.br_
    text "(Available ops: +, -, * and / )"
    dynError ← holdDyn T.empty $ fmap (either ("Error: " <>) (const "")) eventMap
    dynText dynError

    let size = Size 2 4
        initial = Map.fromList
            [ (Coords i j, Right Empty) |
              i ← [0 .. ( width size - 1)]
            , j ← [0 .. (height size - 1)]
            ]
    (_, eventMap) ← foldDynWithEvent updateSheetState (newSheetState size, Right initial) updates
    updates ← sheet initial (fmapMaybe (either (const Nothing) Just) eventMap)
    pure ()


