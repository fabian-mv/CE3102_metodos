import Funciones_ANPI.Base
import Funciones_ANPI.Ecuaciones_No_Lineales.Base
import Funciones_ANPI.Ecuaciones_No_Lineales.Steffensen

-- ejemplo 6.8 Razavi
problemaEjemplo :: Double -> Double
problemaEjemplo vgs =
  vdd - (rd / 2) * un_cox * (w / l) * (vgs - vth)^2 - vgs + vth
  where
    rd  = 5e3
    vdd = 1.8
    w   = 2.0
    l   = 0.18

    -- Valores adicionales
    vth    = 0.4
    un_cox = 100e-6

main :: IO ()
main = withForeign problemaEjemplo $ \f -> print $ solucion (param f) x_0 where
  x_0     = Steffensen { x_k = 0.6 }
  param f = Param
    { tol      = 1e-5
    , iterMax  = 1000
    , objetivo = f
    }
