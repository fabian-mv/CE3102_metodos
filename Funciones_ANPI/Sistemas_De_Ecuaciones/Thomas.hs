{-# LANGUAGE CApiFFI #-}

module Funciones_ANPI.Sistemas_De_Ecuaciones.Thomas
( solucion
) where

import Foreign.Ptr

import Funciones_ANPI.Sistemas_De_Ecuaciones.FFI

foreign import capi "anpi/ffi.h thomas_sistema" cThomas
  :: Ptr CSistema -> IO (Ptr Double)

solucion = solucionFFI cThomas
