{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric, DeriveAnyClass #-}

{-
  Este módulo contiene funciones y tipos que se utilizan en (casi) todos los
  demás métodos. Los métodos que lo usan, lo importan utilizando la palabra
  clave `import` seguida del nombre de este módulo: `ANPI.Base`.
-}

{-
  Exportación de las funciones y tipos para que puedan ser utilizadas
  afuera del módulo.
  
  Los tipos se escriben empezando con mayúscula y están seguidos
  de `(..)`, lo cual hace que también se exporten sus partes.
  
  Las funciones se escríben en minúscula y están precedidas por su firma,
  la cual describe su comportamiento.
-}
module ANPI.Base
( Iteracion (..)
, Param (..)
, Solucion (..)
, denom
, solucion
) where

import GHC.Generics (Generic)

import Control.DeepSeq
import Numeric.LinearAlgebra

{-
  Algebraic Data Type que define un parámetro general que reciben todas las
  funciones iterativas.

  objetivo: función objetivo a analizar
  tol: tolerancia de la aproximación
  iterMax: cantidad máxima de iteraciones permitidas
-}
data Param o = Param
  { objetivo  :: o
  , tol       :: Double
  , iterMax   :: Int
  }


{-
  Algebraic Data Type que define la estructura general de cada iteración.
  Se puede entender como el "estado" actual de cada iteración.

  `deriving Show` hace que este ADT sea imprimible en la terminal.

  k: número de iteración
  aprox_k: solución o aproximación del método iterativo en la iteración k
  error: error del método iterativo
-}
data Iteracion s = Iteracion
  { k        :: Int
  , aprox_k  :: s
  , err      :: Double
  } deriving (Show, Generic, NFData)


{-
  Algebraic Data Type que define la estructura general de una solución.
  Este ADT se usa para calcular la siguiente iteración a partir de la actual.

  error_k: cálculo del error del método iterativo en la iteración k 
  siguiente: siguiente aproximación del método iterativo
-}
class Solucion o s | s -> o where
  error_k :: o -> s -> Double
  siguiente :: o -> s -> s


{-
  Revisa si un valor es menor que el cero de máquina.
  Se usa principalmente para determinar si algún denominador se anula.

  Si el valor es menor que el cero de máquina, detiene el programa.
  Si no lo es, retorna el valor.

  x: valor a comprar con el cero de máquina
-}
denom :: Double -> Double
denom x =
  if   abs x > peps
  then x
  else error "Denominador se anula."


{-
  Obtiene la siguiente iteración a partir de la actual.
  
  Esta es la función principal que usan todos los métodos iterativos, es decir,
  es el "motor" de todos los métodos iterativos y logra que, en módulo de cada
  método iterativo, solo sea necesario escribir su definición. Esta función
  toma esa definición y realiza las iteraciones necesarias hasta alcanzar una
  aproximación que cumpla con la condición de parada.

  Construye una iteración a la cual se le aplica una composición de funciones:
    1. Lista infinita con todos los sucesores de la iteración actual. Para
       obtener esta lista, se utuliza la función `sucesor`.
    2. Lista igual a la anterior, excepto que carece de los resultados que no
       han alcanzado la condición de parada.
    3. Primer valor de la lista anterior.

  Se detiene una vez que se aclanza una condición de parada descrita por la 
  función `parar`. Al detenerse, retorna el valor descrito en el tercer paso
  de la composición de funciones.
-}
solucion :: Solucion o s => Param o -> s -> Iteracion s
solucion param aprox_0 = head . dropWhile (not . parar) . iterate sucesor $
  Iteracion { k = 0, aprox_k = aprox_0, err = error_k objetivo' aprox_0 } where

  parar iter = k iter >= iterMax param || error_k objetivo' (aprox_k iter) < tol param
  
  sucesor iter =
    -- Obtiene la siguiente aproximación a partir de la actual. Para esto,
    -- toma la aproximación de la iteración actual y le aplica la función
    -- objetivo, a cuyo resultado se le obtiene la siguiente iteración.
    -- Además, calcula el siguiente error y aumenta el contador de
    -- iteraciones.
    let aprox_km1 = siguiente objetivo' (aprox_k iter)
    in  Iteracion
        { k       = k iter + 1
        , aprox_k = aprox_km1
        , err     = error_k objetivo' aprox_km1
        }

  objetivo' = objetivo param
