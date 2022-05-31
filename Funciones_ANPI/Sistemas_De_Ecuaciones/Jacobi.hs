{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric, DeriveAnyClass #-}

{-
  Este módulo contiene la funcionalidad del método iterativo de Jacobi.

  Utiliza los módulos Funciones_ANPI.Base y Funciones_ANPI.Sistemas_De_Ecuaciones.Base en su
  funcionamiento.
-}


{-
  Exportación del tipo Jacobi para que pueda ser utilizado afuera del módulo.
-}
module Funciones_ANPI.Sistemas_De_Ecuaciones.Jacobi (Jacobi (..)) where

import GHC.Generics (Generic)

import Control.DeepSeq
import Numeric.LinearAlgebra

import Funciones_ANPI.Base
import Funciones_ANPI.Sistemas_De_Ecuaciones.Base


{-
  Tipo que describe el estado de cada iteración del método iterarivo de
  Jacobi.

  `deriving Show` hace que este ADT sea imprimible en la terminal.
  `deriving NFData` hace que este ADT pueda ser expandido de manera eager,
  lo cual hace que los cálculos de la benchmark sean precisos.
  `deriving Generic` es requerido para usar NFData

  Esta definición es así de sencilla gracias a que la mayoría del boiler plate
  necesario para la ejecución del método está definido en Funciones_ANPI.Base y en 
  Funciones_ANPI.Sistemas_De_Ecuaciones.Base.

  x_k:  Valor inicial de la aproximación.
-}
newtype Jacobi = Jacobi
  { x_k :: Vector R
  } deriving (Show, Generic, NFData)


{-
  Se instancia una Solución utilizando el Criterio del método de Jacobi.

  Se calcula el error en la iteración actual obtenendo la norma dos de la
  diferencia entre la multiplicación de A y x_k y b:

    `|| Ax^(k + 1) - b ||`

  Se calcula la siguiente iteración usando el método de Jacobi.
-}
instance Solucion Sistema Jacobi where
  error_k   sistema aprox = norm_2 (a sistema #> x_k aprox - b sistema)
  siguiente sistema aprox = Jacobi
    -- Para calcular la siguiente iteración, se aplica la definición del método
    -- de Jacobi visto en clase.
    -- Esto consiste en la aplicación del criterio de este método.
    { x_k = vector . map xi_km1 $ is
    } where

    (a', b', x_k') = (a sistema, b sistema, x_k aprox)
    is             = [0..n a' - 1]

    xi_km1 i =
      -- Cálculo del siguiente xi utilizando la expresión conocida:
      -- b1 - (1/ai,i) (Σ1,i-1  Ai,j * xj)
      -- Nótese que se utiliza la función d para evitar la división entre cero.
      (b' ! i - sum [a' ! i ! j * x_k' ! j | j <- is, j /= i]) / denom (a' ! i ! i)
