{-# LANGUAGE CApiFFI #-}

module Funciones_ANPI.Sistemas_De_Ecuaciones.FFI
( CSistema(..)
) where

import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr

import Numeric.LinearAlgebra

import Funciones_ANPI.Sistemas_De_Ecuaciones.Base

newtype {-# CTYPE "anpi/ffi.h" "struct sistema" #-} CSistema = CSistema (Ptr CSistema)

foreign import capi "anpi/ffi.h new_sistema" cNewSistema
  :: Ptr CDouble -> Int -> Int -> Ptr CDouble -> Int -> IO CSistema

foreign import capi "anpi/ffi.h free_sistema" cFreeSistema
  :: CSistema -> IO ()

withCSistema :: Sistema -> (CSistema -> IO a) -> IO a
withCSistema sistema operacion =
  (withArray . map CDouble . concat . toLists) a' $ \celdas ->
    (withArrayLen . map CDouble . toList . b) sistema $ \longitud vector -> do
      ptr <- cNewSistema celdas (rows a') (cols a') vector longitud
      salida <- operacion ptr
      cFreeSistema ptr
      pure salida
  where a' = a sistema
