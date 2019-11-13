{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.View.Presets where

import Reflex.Dom
import Data.Map.Strict
import Data.Text
import qualified Data.Text as T


import Estuary.Types.View

-- presetViewsWDefinedColumnsAndRows :: View
-- presetViewsWDefinedColumnsAndRows =  divClass "test" emptyView

-- presetViewsWDefinedColumnsAndRows' :: View
-- presetViewsWDefinedColumnsAndRows' =  elAttr "div" ("class" =: "subGrid-container" <> "style" =: (setColumnsAndRows 2 3) ) emptyView


presetViews :: Map Text View

-- note: if an ensemble publishes a view called 'default', that view rather than this one,
-- will effectively be the default view in that ensemble.

presetViews = fromList [

      ("default",  GridView 2 3  [
       (Views [LabelView 1, SequenceView 2]),
       (Views [LabelView 3, SequenceView 4]),
       (Views [LabelView 5, TextView 6 0]),
       (Views [LabelView 7, TextView 8 0]),
       (Views [LabelView 9, TextView 10 0]),
       (Views [LabelView 11, TextView 12 0])
      ]),

      ("1x1", GridView 1 1 [
      BorderDiv (Views [LabelView 0,TextView 1 0])
      ]),

      ("2x1", GridView 2 1  [
      BorderDiv (Views [LabelView 0,TextView 1 0]),
      BorderDiv (Views [LabelView 2,TextView 3 0])
      -- BorderDiv (TextView 1 30),
      -- BorderDiv (TextView 2 30)
      ]),

      ("2x2", GridView 2 2 [
      BorderDiv (Views [LabelView 0,TextView 1 0]),
      BorderDiv (Views [LabelView 2,TextView 3 0]),
      BorderDiv (Views [LabelView 4,TextView 5 0]),
      BorderDiv (Views [LabelView 6,TextView 7 0])
      ]),

      ("2x3", GridView 2 3 [
      BorderDiv (Views [LabelView 0,TextView 1 0]),
      BorderDiv (Views [LabelView 2,TextView 3 0]),
      BorderDiv (Views [LabelView 4,TextView 5 0]),
      BorderDiv (Views [LabelView 6,TextView 7 0]),
      BorderDiv (Views [LabelView 8,TextView 9 0]),
      BorderDiv (Views [LabelView 10,TextView 11 0])
      ]),

      ("2x4",  GridView 2 4 [
        BorderDiv (Views [LabelView 0,TextView 1 0]),
        BorderDiv (Views [LabelView 2,TextView 3 0]),
        BorderDiv (Views [LabelView 4,TextView 5 0]),
        BorderDiv (Views [LabelView 6,TextView 7 0]),
        BorderDiv (Views [LabelView 8,TextView 9 0]),
        BorderDiv (Views [LabelView 10,TextView 11 0]),
        BorderDiv (Views [LabelView 12,TextView 13 0]),
        BorderDiv (Views [LabelView 14,TextView 15 0])
      ]),

      ("2x5",  GridView 2 5 [
      BorderDiv (Views [LabelView 0,TextView 1 0]),
      BorderDiv (Views [LabelView 2,TextView 3 0]),
      BorderDiv (Views [LabelView 4,TextView 5 0]),
      BorderDiv (Views [LabelView 6,TextView 7 0]),
      BorderDiv (Views [LabelView 8,TextView 9 0]),
      BorderDiv (Views [LabelView 10,TextView 11 0]),
      BorderDiv (Views [LabelView 12,TextView 13 0]),
      BorderDiv (Views [LabelView 14,TextView 15 0]),
      BorderDiv (Views [LabelView 15,TextView 16 0]),
      BorderDiv (Views [LabelView 17,TextView 18 0])
      ]),

      ("3x6", GridView 3 6  [
      BorderDiv (Views [LabelView 0,TextView 1 0]),
      BorderDiv (Views [LabelView 2,TextView 3 0]),
      BorderDiv (Views [LabelView 4,TextView 5 0]),
      BorderDiv (Views [LabelView 6,TextView 7 0]),
      BorderDiv (Views [LabelView 8,TextView 9 0]),
      BorderDiv (Views [LabelView 10,TextView 11 0]),
      BorderDiv (Views [LabelView 12,TextView 13 0]),
      BorderDiv (Views [LabelView 14,TextView 15 0]),
      BorderDiv (Views [LabelView 16,TextView 17 0]),
      BorderDiv (Views [LabelView 18,TextView 19 0]),
      BorderDiv (Views [LabelView 20,TextView 21 0]),
      BorderDiv (Views [LabelView 22,TextView 23 0]),
      BorderDiv (Views [LabelView 24,TextView 25 0]),
      BorderDiv (Views [LabelView 26,TextView 27 0]),
      BorderDiv (Views [LabelView 28,TextView 29 0]),
      BorderDiv (Views [LabelView 30,TextView 31 0]),
      BorderDiv (Views [LabelView 32,TextView 33 0]),
      BorderDiv (Views [LabelView 34,TextView 35 0])
      ]),

      ("4x7", GridView 4 7  [
      BorderDiv (Views [LabelView 0,TextView 1 0]),
      BorderDiv (Views [LabelView 2,TextView 3 0]),
      BorderDiv (Views [LabelView 4,TextView 5 0]),
      BorderDiv (Views [LabelView 6,TextView 7 0]),
      BorderDiv (Views [LabelView 8,TextView 9 0]),
      BorderDiv (Views [LabelView 10,TextView 11 0]),
      BorderDiv (Views [LabelView 12,TextView 13 0]),
      BorderDiv (Views [LabelView 14,TextView 15 0]),
      BorderDiv (Views [LabelView 16,TextView 17 0]),
      BorderDiv (Views [LabelView 18,TextView 19 0]),
      BorderDiv (Views [LabelView 20,TextView 21 0]),
      BorderDiv (Views [LabelView 22,TextView 23 0]),
      BorderDiv (Views [LabelView 24,TextView 25 0]),
      BorderDiv (Views [LabelView 26,TextView 27 0]),
      BorderDiv (Views [LabelView 28,TextView 29 0]),
      BorderDiv (Views [LabelView 30,TextView 31 0]),
      BorderDiv (Views [LabelView 32,TextView 33 0]),
      BorderDiv (Views [LabelView 34,TextView 35 0]),
      BorderDiv (Views [LabelView 36,TextView 37 0]),
      BorderDiv (Views [LabelView 38,TextView 39 0]),
      BorderDiv (Views [LabelView 40,TextView 41 0]),
      BorderDiv (Views [LabelView 42,TextView 43 0]),
      BorderDiv (Views [LabelView 44,TextView 45 0]),
      BorderDiv (Views [LabelView 46,TextView 47 0]),
      BorderDiv (Views [LabelView 48,TextView 49 0]),
      BorderDiv (Views [LabelView 50,TextView 51 0]),
      BorderDiv (Views [LabelView 52,TextView 53 0]),
      BorderDiv (Views [LabelView 54,TextView 55 0])
      ]),

      ("cybernetic",GridView 2 2 [
      BorderDiv (Views [LabelView 1, SequenceView 2]),
      BorderDiv (Views [LabelView 3, TextView 4 0]),
      BorderDiv (Views [LabelView 5, TextView 6 0]),
      BorderDiv (Views [LabelView 7, TextView 8 0])
      ]),

      ("supercontinent", GridView 2 5 [
      BorderDiv (Views [LabelView 0,TextView 1 0]),
      BorderDiv (Views [LabelView 2,TextView 3 0]),
      BorderDiv (Views [LabelView 4,TextView 5 0]),
      BorderDiv (Views [LabelView 6,TextView 7 0]),
      BorderDiv (Views [LabelView 8,TextView 9 0]),
      BorderDiv (Views [LabelView 10,TextView 11 0]),
      BorderDiv (Views [LabelView 12,TextView 13 0]),
      BorderDiv (Views [LabelView 14,TextView 15 0]),
      BorderDiv (Views [LabelView 16,TextView 17 0]),
      BorderDiv EnsembleStatusView
      ]),

      ("iclc2019",GridView 2 2 [
      BorderDiv (Views [LabelView 1, SequenceView 2]),
      BorderDiv (Views [LabelView 3, TextView 4 0]),
      BorderDiv (Views [LabelView 5, TextView 6 0]),
      BorderDiv (Views [LabelView 7, TextView 8 0])
      ]),

      ("blackBox",GridView 1 1 [
         BorderDiv (Views [LabelView 0,TextView 1 0]),
         BorderDiv (Views [LabelView 2,TextView 3 0])
         ]),

      ("memorias",GridView 3 2 [
         BorderDiv (Views [LabelView 0,TextView 1 0]),
         BorderDiv (Views [LabelView 2,TextView 3 0]),
         BorderDiv (Views [LabelView 4,TextView 5 0]),
         BorderDiv (Views [LabelView 6,TextView 7 0]),
         BorderDiv (Views [LabelView 8,TextView 9 0]),
         BorderDiv (Views [LabelView 10,TextView 11 0])
         ])  
      ]