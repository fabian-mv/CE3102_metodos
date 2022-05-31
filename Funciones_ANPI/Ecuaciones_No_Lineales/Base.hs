{-
  Este módulo contiene funciones y tipos que se utilizan en (casi) todos los
  métodos relacionados a ecuaciones no lineales.
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
module Funciones_ANPI.Ecuaciones_No_Lineales.Base
( Criterio
, Derivable (..)
) where

import Numeric.LinearAlgebra

{-
  Tipo que defino un a nivel genérico, el criterio de una función matemática.
-}
type Criterio = Double -> Double

{-
  Algebraic Data Type que contiene a una función y su derivada.
  
  Este ADT se utiliza para pasarle a un método iterativo que lo requiera, una
  función y su derivada.

  f: función sin derivar
  f': derivada de la función
-}
data Derivable = Derivable
  { f  :: Criterio
  , f' :: Criterio
  }