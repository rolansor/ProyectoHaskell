-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.Internals.Utilities
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (requires concurrency)
--
-- A simple graphics library.
--
-----------------------------------------------------------------------------

-- #hide
module Graphics.HGL.Internals.Utilities(
	bracket, bracket_,
	safeTry,
	Exception,
        modMVar, modMVar_
	) where

import qualified Control.Exception as E (bracket, try, IOException, tryJust, ioErrors)
import Control.Concurrent( MVar, takeMVar, putMVar )

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket = E.bracket

-- Not exactly the same type as GHC's bracket_
bracket_ :: IO a -> (a -> IO b) -> IO c -> IO c
bracket_ left right m = bracket left right (const m)

type Exception = E.IOException
safeTry :: IO a -> IO (Either Exception a)

#if __GLASGOW_HASKELL >= 610 
-- ghc-6.10
safeTry = E.try
#else
-- ghc 6.8 (and below?)
safeTry = E.tryJust E.ioErrors
#endif


----------------------------------------------------------------
-- Utilities
----------------------------------------------------------------

modMVar :: MVar a -> (a -> a) -> IO a
modMVar mv f = do
  x <- takeMVar mv
  putMVar mv (f x)
  return x

modMVar_ :: MVar a -> (a -> a) -> IO ()
modMVar_ mv f = do
  x <- takeMVar mv
  putMVar mv (f x)
