{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, CApiFFI #-}

module ANPI.Ecuaciones_No_Lineales.Steffensen
( Steffensen (..)
, withForeign
) where 

import Foreign.C.Types
import Foreign.Ptr

import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base

foreign import capi "steffensen.h err_steffensen" errSteffensen
  :: FunPtr Criterio -> CDouble -> CDouble

foreign import capi "steffensen.h iter_steffensen" iterSteffensen
  :: FunPtr Criterio -> CDouble -> CDouble

data Steffensen = Steffensen 
  { x_k :: Double
  } deriving Show

withForeign :: Criterio -> (FunPtr Criterio -> IO a) -> IO a
withForeign criterio operacion = do
  ptr <- wrapCriterio criterio
  salida <- operacion ptr
  freeHaskellFunPtr ptr
  return salida

instance Solucion (FunPtr Criterio) Steffensen where
  error_k criterio aprox =
    let CDouble valor = errSteffensen criterio (CDouble (x_k aprox))
    in  valor

  siguiente criterio aprox =
    let CDouble valor = iterSteffensen criterio (CDouble (x_k aprox))
    in  Steffensen { x_k = valor }

foreign import ccall "wrapper" wrapCriterio :: Criterio -> IO (FunPtr Criterio)
