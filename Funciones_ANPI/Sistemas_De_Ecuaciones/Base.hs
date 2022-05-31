{-
  Este módulo contiene funciones y tipos que se utilizan en (casi) todos los
  métodos relacionados a sistemas de ecuaciones.
  Los métodos que lo usan, lo importan utilizando la palabra clave `import`
  seguida del nombre de este módulo: `Funciones_ANPI.Ecuaciones_No_Lineales.Base`.
-}


{-
  Exportación de las funciones y tipos para que puedan ser utilizadas
  afuera del módulo.
  
  Los tipos se escriben empezando con mayúscula y están seguidos
  de `(..)`, lo cual hace que también se exporten sus partes.
  
  Las funciones se escríben en minúscula y están precedidas por su firma,
  la cual describe su comportamiento.
-}
module Funciones_ANPI.Sistemas_De_Ecuaciones.Base
( Sistema (..)
, n
) where

import Numeric.LinearAlgebra

{-
  Algebraic Data Type que define una matriz aumentada.
  
  Este ADT se utiliza para pasarle a un método iterativo que lo requiera, el
  conjunto constituido por una matriz A y el vector b, comunmente utilizados
  de la siguiente manera:

  `Ax = b`

  Donde `A` es una matriz que representa el sistema de ecuaciones, `x` es
  un vector que contiene las variables del sistema y `b` es el vector solución
  del sistema.

  a: Matriz que representa un sistema de ecuaciones
  b: Vector solución del sistema
-}
data Sistema = Sistema
  -- Haskell no permite llamarle "A" mayúscula
  { a :: Matrix R
  , b :: Vector R
  }

{-
  Obtiene el tamaño de una matriz cuadrada. Si la matriz no es cuadrada,
  detiene el programa.
  
  Se usa principalmente para obtener el tamaño de una matriz en los métodos
  iterativos que requieren este valor.

  Revisa si el tamaño de las filas es igual al de las columnas. Si lo es,
  retorna el tamaño de las filas. Si no lo es, detiene el programa.

  n: Matriz a analizar
-}
n :: Matrix a -> Int
n matriz = 
  if   rows matriz == cols matriz 
  then rows matriz
  else error "Matriz no cuadrada."