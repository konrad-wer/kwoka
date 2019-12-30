module ASTBuilder where

import AST
import qualified Data.Map as Map

buildEffectEnv :: [TopLevelDef p] -> Map.Map String [ActionDef p]
buildEffectEnv [] = Map.empty
buildEffectEnv (DefEff (EffectDef pos name actions) : effs) = Map.insert name actions $ buildEffectEnv effs