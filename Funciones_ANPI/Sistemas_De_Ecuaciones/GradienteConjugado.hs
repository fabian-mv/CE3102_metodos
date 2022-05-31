{-# LANGUAGE CApiFFI #-}

module Funciones_ANPI.Sistemas_De_Ecuaciones.GradienteConjugado
( solucion
) where

import Foreign.Ptr

import Funciones_ANPI.Sistemas_De_Ecuaciones.FFI

foreign import capi "anpi/ffi.h gradiente_conjugado_sistema" cGradienteConjugado
  :: Ptr CSistema -> IO (Ptr Double)

solucion = solucionFFI cGradienteConjugado
