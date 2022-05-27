import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base
import ANPI.Ecuaciones_No_Lineales.Steffensen

main :: IO ()
main = withForeign objetivo $ \f -> print $ solucion (param f) x_0 where
  x_0        = Steffensen { x_k = 0 }
  objetivo x = x^2 - 42
  param f    = Param
    { tol      = 1e-8
    , iterMax  = 1000
    , objetivo = f
    }
