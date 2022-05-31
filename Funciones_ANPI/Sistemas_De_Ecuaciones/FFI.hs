{-# LANGUAGE CApiFFI #-}

module Funciones_ANPI.Sistemas_De_Ecuaciones.FFI
( CSistema
, solucionFFI
) where

import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr

import Numeric.LinearAlgebra

import Funciones_ANPI.Sistemas_De_Ecuaciones.Base

data {-# CTYPE "anpi/ffi.h" "struct sistema" #-} CSistema = CSistema
  { ptr :: Ptr CSistema
  , len :: Int
  }

foreign import capi "anpi/ffi.h new_sistema" cNewSistema
  :: Ptr CDouble -> Int -> Int -> Ptr CDouble -> Int -> IO (Ptr CSistema)

foreign import capi "anpi/ffi.h free_sistema" cFreeSistema
  :: Ptr CSistema -> IO ()

withCSistema :: Sistema -> (CSistema -> IO a) -> IO a
withCSistema sistema operacion =
  (withArray . map CDouble . concat . toLists) a' $ \celdas ->
    (withArrayLen . map CDouble . toList . b) sistema $ \longitud vector -> do
      ptr <- cNewSistema celdas (rows a') (cols a') vector longitud
      salida <- operacion CSistema { ptr = ptr, len = longitud }
      cFreeSistema ptr
      pure salida
  where a' = a sistema

solucionFFI :: (Ptr CSistema -> IO (Ptr Double)) -> CSistema -> IO (Vector R)
solucionFFI metodo sistema = do
  base <- metodo (ptr sistema)
  vector <- peekArray (len sistema) base
  pure $ fromList vector
