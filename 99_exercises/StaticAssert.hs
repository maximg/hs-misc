-- Stolen from http://stackoverflow.com/questions/6648764/compile-time-assertions-with-ghc-haskell

{-# LANGUAGE TemplateHaskell #-}
module StaticAssert (staticAssert) where

import Control.Monad (unless)
import Language.Haskell.TH (report)

-- TODO: replace report with reportError
staticAssert cond mesg = do
    unless cond $ report True $ "Compile time assertion failed: " ++ mesg
    return [] -- No need to make a dummy declaration
