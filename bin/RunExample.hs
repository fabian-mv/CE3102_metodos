import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base
import ANPI.Ecuaciones_No_Lineales.Steffensen

main :: IO ()
main = print $ solucion param Steffensen where
  param = Param
    { tol      = 1e-8
    , iterMax  = 1000
    , objetivo = \x -> x
    }
