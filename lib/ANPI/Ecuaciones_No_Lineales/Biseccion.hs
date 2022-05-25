module ANPI.Ecuaciones_No_Lineales.Biseccion (Biseccion (..))
where 

import ANPI.Base

data Biseccion = Biseccion
  { a :: Double
  , b :: Double
  }

instance Solucion Criterio Biseccion where
  error criterio aprox = abs . criterio . x_k $ aprox
  siguiente criterio aprox = Biseccion
    { a = if bolzano then a aprox else x
    , b = if bolzano then x       else b aprox
    } where
    x       = x_k aprox
    bolzano = signum (criterio a) /= signum (criterio x)

x_k aprox = (a aprox + b aprox)/2