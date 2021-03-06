{-# LANGUAGE TypeSynonymInstances, FlexibleInstances
  , MultiParamTypeClasses, CApiFFI, DeriveGeneric, DeriveAnyClass #-}


{-
  Este módulo contiene la funcionalidad del método iterativo de Steffensen.

  Utiliza los módulos Funciones_ANPI.Base y Funciones_ANPI.Ecuaciones_No_Lineales.Base en su
  funcionamiento.

  Este módulo es diferente a los demás porque la implementación del método
  no está implementada en Haskell. Más bien está implementada en C++. Por esa
  razón, este módulo debe verse únicamente como una interfaz de C++ al runtime
  de Haskell.

  Esto permite que se pueda ejecutar este método desde con Haskell,
  igual que todos los demás.
-}


{-
  Exportación del tipo Steffensen para que pueda ser utilizado afuera
  del módulo. Tamibén se exporta withForeign.
-}
module Funciones_ANPI.Ecuaciones_No_Lineales.Steffensen
( Steffensen (..)
, withForeign
) where 

import GHC.Generics (Generic)
import Foreign.C.Types
import Foreign.Ptr

import Control.DeepSeq

import Funciones_ANPI.Base
import Funciones_ANPI.Ecuaciones_No_Lineales.Base


{-
  Importar las funciones err_steffensen e iter_steffensen del
  archivo stephensen.h.

  Para esto, se usa la función `capi`, la cual hace que GHC construya un
  archivo de C en el cual se incluye la biblioteca estandar de C y una
  función stub que llama a la función original.

  Para más información acerca de foreign imports en Haskell,
  leer: https://www.haskell.org/ghc/blog/20210709-capi-usage.html
-}
foreign import capi "anpi/ffi.h err_steffensen" errSteffensen
  :: FunPtr Criterio -> CDouble -> CDouble

foreign import capi "anpi/ffi.h iter_steffensen" iterSteffensen
  :: FunPtr Criterio -> CDouble -> CDouble


{-
  Tipo que describe el estado de cada iteración del método iterarivo de
  la Steffensen.

  `deriving Show` hace que este ADT sea imprimible en la terminal.
  `deriving NFData` hace que este ADT pueda ser expandido de manera eager,
  lo cual hace que los cálculos de la benchmark sean precisos.
  `deriving Generic` es requerido para usar NFData

  Esta definición es así de sencilla gracias a que la mayoría del boiler plate
  necesario para la ejecución del método está definido en Funciones_ANPI.Base y en 
  Funciones_ANPI.Ecuaciones_No_Lineales.Base.

  x_k: Valor inicial de la aproximación.
-}
data Steffensen = Steffensen 
  { x_k :: Double
  } deriving (Show, Generic, NFData)


{-
  Esta función sirve como interfaz en tiempo real entre Haskell y C++.

  Esto es necesario ya que la arquitectura de Haskell y C++ son muy diferentes.

  En esenecia, esta función genera código máquina que C++ puede interpretar
  y ejecutar.

  Para más información acerca de IO Haskell,
  leer: https://wiki.haskell.org/Introduction_to_IO
-}
withForeign :: Criterio -> (FunPtr Criterio -> IO a) -> IO a
withForeign criterio operacion = do
  ptr <- wrapCriterio criterio
  salida <- operacion ptr
  freeHaskellFunPtr ptr
  return salida


{-
  Se instancia una Solución utilizando el Criterio del método Steffensen.

  Se calcula el error en la iteración actual usando el método definido en C++.
  
  Consultar la documentación de esos métodos en sus respectivos archivos.
-}
instance Solucion (FunPtr Criterio) Steffensen where
  error_k criterio aprox =
    let CDouble valor = errSteffensen criterio (CDouble (x_k aprox))
    in  valor

  siguiente criterio aprox =
    let CDouble valor = iterSteffensen criterio (CDouble (x_k aprox))
    in  Steffensen { x_k = valor }


{-
  Genera una acción IO autogenerada por el compilador. Toma una función en
  Haskell y produce un FunPtr.
-}
foreign import ccall "wrapper" wrapCriterio :: Criterio -> IO (FunPtr Criterio)
