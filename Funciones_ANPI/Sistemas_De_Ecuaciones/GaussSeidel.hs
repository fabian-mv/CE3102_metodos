{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric, DeriveAnyClass #-}

{-
  Este módulo contiene la funcionalidad del método iterativo Gauss Seidel.

  Utiliza los módulos Funciones_ANPI.Base y Funciones_ANPI.Sistemas_De_Ecuaciones.Base en su
  funcionamiento.
-}


{-
  Exportación del tipo GaussSeidel para que pueda ser utilizado afuera del módulo.
-}
module Funciones_ANPI.Sistemas_De_Ecuaciones.GaussSeidel  (GaussSeidel (..))
where

import GHC.Generics (Generic)

import Control.DeepSeq
import Numeric.LinearAlgebra

import Funciones_ANPI.Sistemas_De_Ecuaciones.Base
import Funciones_ANPI.Base

{-
  Tipo que describe el estado de cada iteración del método iterarivo de
  Gauss Seidel.

  `deriving Show` hace que este ADT sea imprimible en la terminal.
  `deriving NFData` hace que este ADT pueda ser expandido de manera eager,
  lo cual hace que los cálculos de la benchmark sean precisos.
  `deriving Generic` es requerido para usar NFData

  Esta definición es así de sencilla gracias a que la mayoría del boiler plate
  necesario para la ejecución del método está definido en Funciones_ANPI.Base y en 
  Funciones_ANPI.Sistemas_De_Ecuaciones.Base.

  x_k:  Valor inicial de la aproximación.
-}
newtype GaussSeidel = GaussSeidel
  { x_k :: Vector R
  } deriving (Show, Generic, NFData)


{-
  Se instancia una Solución utilizando el Criterio de la función de Gauss Seidel.

  Se calcula el error en la iteración actual obtenendo la norma dos de la
  diferencia entre la multiplicación de A y x_k y b:

    `|| Ax^(k + 1) - b ||`

  Se calcula la siguiente iteración usando el método de GaussSeidel.
-}
instance Solucion Sistema GaussSeidel where
  error_k   sistema aprox = norm_2 $ a sistema #> x_k aprox - b sistema
  siguiente sistema aprox = GaussSeidel
    -- Para calcular la siguiente iteración, se aplica la definición del método
    -- de Gauss Seidel visto en clase.
    -- Esto consiste principalmente en la aplicación de sustiución hacia
    -- adelante a diversos resultados de operaciones de algebra matricial
    -- como se mostró en clase.
    { x_k = sol ld (b sistema) - sol ld (u #> x_k aprox) } where
      (ld, u) = (ldu . a) sistema
      sol a b = sustitucionAdelante Sistema { a = a, b = b }


{-
  Aplica sustiticón hacia adelante a una matriz.

  Para esto, se aplica una composición de funciones a un sistema:
    1. Suma desde 0 hasta n-1 de `Aij * xj`
    2. Resta del resultado de la operación anterior y el vector b
    3. Lista con el resultado de la operación anterior dividida entre `Aii`
    4. Vector a partir de la lista anterior
  
  sistema: Matriz aumentada a la cual se le aplica sustitución hacia adlenate.
-}
sustitucionAdelante :: Sistema -> Vector R
sustitucionAdelante sistema = fromList . reverse . foldl x [] $ [0..n a' - 1] where
  x acc i = (: acc) . (/ denom (a' ! i ! i)) . (b' ! i -) . sum . map (uncurry (*)) . zip (toList $ a' ! i) . reverse $ acc
  a'      = a sistema
  b'      = b sistema


{-
  Aplica sustiticón LDU a una matriz.

  Para esto, se construyen dos matrices del tamaño de la matriz original y se
  filtran los valores ambas de tal manera que se obtengan las matrices LD y U.
  
  sistema: Matriz a la cual se le aplica sustitución LDU.
-}
ldu :: Matrix R -> (Matrix R, Matrix R)
ldu matriz = (tri (>=), tri (<)) where
    -- El bloque `where` añade estructura al programa y permite evitar la
    -- repetición de código y aumenta claridad del programa. Aunque esta
    -- sintaxis puede parecer extraña, es análoga a la palabra "donde", la cual
    -- se usa comúnmente en el lenguaje matemático. Por ejemplo:
    --     x + 0 = 1,
    --       donde x = 1
    -- 
    -- Como se observa, la palabra clave `where` inicia un bloque en el cual
    -- se declaran ecuaciones que se reemplazan en el código que está arriba
    -- del `where`.
    
  tri filtro = build (size matriz) $ \i j ->
    if i `filtro` j then matriz ! truncate i ! truncate j else 0
