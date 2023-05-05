data Flor= Flor{nombreFlor :: String, aplicacion:: String, cantidadDeDemanda:: Int} deriving Show

rosa = Flor "rosa" "decorativo" 120
jazmin =  Flor "jazmin" "aromatizante" 100
violeta=  Flor "violeta" "infusión" 110
orquidea =  Flor "orquidea" "decorativo" 90

flores = [orquidea, rosa,violeta, jazmin]

maximaFlorSegun :: [Flor] -> (Flor -> Int) -> String
maximaFlorSegun flores f = (nombreFlor. maximaFlor f) flores


maximaFlor :: (Flor -> Int) -> [Flor] -> Flor
maximaFlor _   [flor]  = flor
maximaFlor f  (flor: flores)  | f flor >= (f.maximaFlor f) flores =  flor
                              | otherwise = maximaFlor f flores



{- a
ghci> maximaFlorSegun flores cantidadDeDemanda
"rosa"
-}

{-b
ghci> maximaFlorSegun flores (length.nombreFlor)
"orquidea"
-}

{- c
ghci> maximaFlorSegun flores ((`mod` 4).cantidadDeDemanda) 
"orquidea"
ghci>
-}

cantidadDeElementos :: [(Integer, Integer)] -> Integer
cantidadDeElementos lista = foldl contar  0 lista

contar :: Integer -> (Integer, Integer) -> Integer
contar sem _  = sem +1

cantidadDeElementos' :: [a] -> Integer
cantidadDeElementos' lista = foldl (\sem _ -> sem + 1)  0 lista

cantidadDeElementos''  ::  [a] -> Integer
cantidadDeElementos''  lista = foldr (\_ sem -> sem + 1) 0 lista

masGastador :: [(String, Integer) ] -> (String, Integer)
masGastador (cab: pares) = foldl tieneMayorGasto cab pares

tieneMayorGasto :: (String, Integer ) -> (String, Integer) -> (String, Integer)
tieneMayorGasto unPar otroPar | snd unPar > snd otroPar = unPar
                              | otherwise = otroPar

masGastador' :: [(String, Integer)] -> (String, Integer)
masGastador' (cab: pares) = foldr tieneMayorGasto cab  pares


monto :: [(String, Integer)] -> Integer
monto empleados = foldl (\sem (_, gasto) -> sem + gasto) 0 empleados

monto' :: [(String, Integer)] -> Integer
monto' empleados = foldr (\(_, gasto) sem -> sem + gasto) 0 empleados


{-
ghci> foldl (\sem fun -> fun sem) 2 [(3+), (*2), (5+)] 
15
ghci> foldl (flip ($)) 2 [(3+), (*2), (5+)]              
15
ghci> foldr (\fun sem -> fun sem)   2 [(3+), (*2), (5+)]
17
ghci> foldr ($)   2 [(3+), (*2), (5+)]                  
17
-}


type Nombre  = String
type InversionInicial = Int
type Profesionales = [String]

data  Proyecto = Proy {nombre:: Nombre, inversionInicial::  InversionInicial, profesionales:: Profesionales} deriving Show

proyectos = [Proy "red social de arte"  20000 ["ing. en sistemas", "contador"], Proy "restaurante" 5000 ["cocinero", "adm. de empresas", "contador"], Proy "ventaChurros" 1000 ["cocinero"] ]

maximoProyectoSegun :: [Proyecto] -> ( Proyecto -> Int ) -> Proyecto
maximoProyectoSegun (unProyecto: proyectos) f  = foldl  (maximoSegun f) unProyecto  proyectos


maximoSegun :: (Proyecto -> Int) -> Proyecto  -> Proyecto -> Proyecto
maximoSegun f unProyecto otroProyecto | f unProyecto > f otroProyecto = unProyecto
                                      | otherwise = otroProyecto


maximoProyectoSegun' :: [Proyecto] -> ( Proyecto -> Int ) -> Proyecto
maximoProyectoSegun' (unProyecto:proyectos) f = foldr (maximoSegun f)   unProyecto  proyectos

{-ghci> maximoProyectoSegun proyectos inversionInicial
Proy {nombre = "red social de arte", inversionInicial = 20000, profesionales = ["ing. en sistemas","contador"]}
-}


{-ghci> maximoProyectoSegun proyectos (length.profesionales)
Proy {nombre = "restaurante", inversionInicial = 5000, profesionales = ["cocinero","adm. de empresas","contador"]}
-}

{-ghci> maximoProyectoSegun proyectos (length.words.nombre) 
Proy {nombre = "red social de arte", inversionInicial = 20000, profesionales = ["ing. en sistemas","contador"]}
-}

