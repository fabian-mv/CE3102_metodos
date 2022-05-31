{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric, DeriveAnyClass #-}

{-
  Este módulo contiene la funcionalidad del método iterativo la Pseudoinversa.

  Utiliza los módulos Funciones_ANPI.Base y Funciones_ANPI.Sistemas_De_Ecuaciones.Base en su
  funcionamiento.
-}


{-
  Exportación del tipo Pseudoinversa para que pueda ser utilizado afuera del
  módulo. También se exporta la función x_0.
-}
module Funciones_ANPI.Sistemas_De_Ecuaciones.Pseudoinversa (Pseudoinversa (..), x_0) where

import GHC.Generics (Generic)

import Control.DeepSeq
import Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra as LA

import Funciones_ANPI.Sistemas_De_Ecuaciones.Base
import Funciones_ANPI.Base
import Text.Read (Lexeme(Ident))


{-
  Algebraic Data Type que describe el estado de cada iteración del método
  iterarivo de Pseudoinversa.

  `deriving Show` hace que este ADT sea imprimible en la terminal.
  `deriving NFData` hace que este ADT pueda ser expandido de manera eager,
  lo cual hace que los cálculos de la benchmark sean precisos.
  `deriving Generic` es requerido para usar NFData

  Esta definición es así de sencilla gracias a que la mayoría del boiler plate
  necesario para la ejecución del método está definido en Funciones_ANPI.Base y en 
  Funciones_ANPI.Sistemas_De_Ecuaciones.Base.

  pseudo_k: Valor inicial de la pseudoinversa
  x_k: Valor inicial de la aproximación
-}
data Pseudoinversa = Pseudoinversa
  -- pseudo_k es quien se utiliza para para obtener las siguientes iteraciones
  -- a partir del estado actual. Es útil hacer esto porque de esta forma x_k no
  -- se calcula en cada iteración, lo cual hace al algoritmo más eficiente.
  -- Esta eficiencia se logra gracias a la lazyness de Haskell, ya que logra que
  -- solamente se solicita el último x_k, los anteriores se ignoran y
  -- por tanto nunca se computan del todo.
  { pseudo_k :: Matrix R
  , x_k      :: Vector R
  } deriving (Show, Generic, NFData)


{-
  Se instancia una Solución utilizando el Criterio del método del Pseudoinversa.

  Se calcula el error en la iteración actual obtenendo la norma dos de la
  diferencia entre la multiplicación de A y x_k y b:

    `|| Ax^(k + 1) - b ||`

  Se calcula la siguiente iteración usando el método de Pseudoinversa.
-}
instance Solucion Sistema Pseudoinversa where
  error_k sistema aprox = norm_2 (pseudo_kmas1' - pseudo_k') / (denom . norm_2) pseudo_kmas1'
    where (pseudo_k', pseudo_kmas1') = (pseudo_k aprox, pseudo_k . siguiente sistema $ aprox)

  siguiente sistema aprox = sol sistema $
    -- Para calcular la siguiente iteración, se aplica la definición del método
    -- de Pseudoinversa visto en clase.
    -- Esto consiste en la aplicación del criterio de este método.
    pseudo_k aprox LA.<> (_2i - a sistema LA.<> pseudo_k aprox)
    where _2i = (2 *) . ident . n . a $ sistema


{-
  Genera la primera aproximación para iniciar la iteración.

  Para esto, se realiza la multiplicación escalar entre la transpuesta de
  matriz A y el reciproco de la norma 2 de la matriz A elevada al cuadrado
-}
x_0 :: Sistema -> Pseudoinversa
x_0 sistema = sol sistema $ (recip . denom . (^ 2) . norm_2 . a) sistema `scale` tr' (a sistema)


{-
  Genera la primera x_k a partir de pseudo_k y el vector b.
-}
sol sistema pseudo_k = Pseudoinversa
  { pseudo_k = pseudo_k
  , x_k      = pseudo_k #> b sistema
  }