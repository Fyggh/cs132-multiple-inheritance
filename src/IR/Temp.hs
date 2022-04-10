{-|
Module       : Temp
Description  : Support for assembly-level temporaries (pseudo-registers) and
               assembly-language labels.
Maintainer   : CS 132
-}

module IR.Temp where

import qualified Data.IORef

------------------------------------
-- Temporaries (pseudo-registers) --
------------------------------------

-- | A simulated collection of arbitrarily many hardware registers
data Temp = T Integer           -- ^ General Temporaries
          | NT String Integer   -- ^ Named Temporaries
          | R String            -- ^ Machine Registers
   deriving (Eq, Ord, Read)

instance Show Temp where
  show (T n   ) = "%t" ++ show n
  show (NT s n) = "%" ++ s ++ show n
  show (R s   ) = s

-- | Get the temporary corresponding to a specific machine registers.
--   We shouldn't need to call this often.
registerTemp :: String -> Temp
registerTemp = R

-- | Construct temporaries for other phrases of the translation.
--   We won't need to call this directly.
specifiedNamedTemp :: String -> Integer -> Temp
specifiedNamedTemp = NT

-- | Is the given temporary a machine register?
isRegister :: Temp -> Bool
isRegister (R _) = True
isRegister _     = False

---------------------------------------------
-- Generating fresh temporaries and labels --
---------------------------------------------

type Counter = Data.IORef.IORef Integer

-- | Generate a fresh, named temporary
mkFreshTemp :: String -> Counter -> IO Temp
mkFreshTemp prefix ct = do
  next <- Data.IORef.readIORef ct
  Data.IORef.writeIORef ct (next + 1)
  return (specifiedNamedTemp prefix next)

-- | A destination to jump to
type Label = String

-- | Generate a fresh label name
mkFreshLabel :: Counter -> IO Label
mkFreshLabel ct = do
  next <- Data.IORef.readIORef ct
  Data.IORef.writeIORef ct (next + 1)
  return (".L" ++ show next)
