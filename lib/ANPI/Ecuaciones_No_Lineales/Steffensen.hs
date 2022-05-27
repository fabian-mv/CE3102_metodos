{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, CApiFFI #-}

module ANPI.Ecuaciones_No_Lineales.Steffensen (Steffensen (..)) where 

import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr

import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base

foreign import capi "Steffensen.h err_steffensen" errSteffensen
  :: FunPtr (StablePtr Criterio -> CDouble -> CDouble)
  -> StablePtr Criterio -> CDouble -> CDouble

foreign import capi "Steffensen.h iter_steffensen" iterSteffensen
  :: FunPtr (StablePtr Criterio -> CDouble -> CDouble)
  -> StablePtr Criterio -> CDouble -> CDouble

data Steffensen = Steffensen deriving Show

instance Solucion Criterio Steffensen where
  error_k   criterio aprox = error "TODO"
  siguiente criterio aprox = Steffensen
