{-# LANGUAGE TemplateHaskell #-}

module DebugState where

import qualified Data.Map.Strict as Map
import Control.Lens
import DebugCmd

data DebugState = DebugState {
    _debugLevel :: !Int,
    -- _posbreak :: (Int, Int),
    _variables :: Map.Map String Value
}

start :: DebugState
start = DebugState {
          _variables = Map.empty,
          _debugLevel = -1
          -- _posbreak = (-1, -1)
        }

$(makeLenses '' DebugState)
