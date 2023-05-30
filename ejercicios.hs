data Animal= Raton {nombre :: String, edad :: Double, peso :: Double,
 enfermedades :: [String]} deriving Show
-- Ejemplo de raton
cerebro = Raton "Cerebro" 9.0 0.2 ["brucelosis", "sarampión", "tuberculosis"]

orejudo = Raton "Orejudo" 4.0 9.0 ["sinusitis"]

-- Estos son las enfermedades infecciosas
enfermedadesInfecciosas = [ "brucelosis", "tuberculosis"]

modificarEdad :: (Double -> Double) -> Animal -> Animal
modificarEdad f raton = raton { edad = (f.edad)raton}

modificarNombre :: (String -> String) -> Animal -> Animal
modificarNombre f raton = raton {nombre = (f.nombre) raton}

modificarPeso :: (Double -> Double) -> Animal -> Animal
modificarPeso f raton = raton { peso = (f.peso) raton}


modificarEnfermedades :: ([String]-> [String]) -> Animal -> Animal
modificarEnfermedades f raton = raton {enfermedades = (f.enfermedades) raton}


hierbaBuena :: Animal -> Animal
hierbaBuena raton = modificarEdad sqrt raton

hierbaVerde :: String -> Animal -> Animal
hierbaVerde enfermedad  raton =  modificarEnfermedades (filter ((/= enfermedad)))  raton

alcachofa :: Animal -> Animal
alcachofa raton = modificarPeso perderPeso raton


perderPeso :: Double -> Double
perderPeso peso | peso > 2 = peso * 0.9
                | otherwise = peso * 0.95

hierbaMagica :: Animal -> Animal
hierbaMagica raton = modificarEdad (*0) . modificarEnfermedades (const []) $ raton


medicamento :: [(Animal -> Animal)] -> Animal -> Animal
medicamento hierbas raton = foldl (\unRaton unaHierba  -> unaHierba unRaton) raton hierbas


medicamento' hierbas raton = foldl (flip ($))  raton hierbas

antiAge :: Animal -> Animal
antiAge raton = medicamento (replicate 3 hierbaBuena ++ [alcachofa]) raton


reduceFatFast :: Int -> Animal -> Animal
reduceFatFast potencia raton = medicamento ([hierbaVerde "obesidad"] ++ replicate potencia alcachofa )  raton

hierbaMilagrosa :: Animal -> Animal
hierbaMilagrosa raton = medicamento (map hierbaVerde enfermedadesInfecciosas) raton


cantidadIdeal :: (Int -> Bool) -> Int
cantidadIdeal criterio = head . filter criterio $ [1..]


estanMejoresQueNunca  :: [Animal] -> (Animal -> Animal) -> Bool
estanMejoresQueNunca ratones medicamento = all ((<1).peso.medicamento)  ratones 

{-
ghci> estanMejoresQueNunca [cerebro] antiAge
True
-}

experimento :: [Animal] -> Int
experimento ratones  = cantidadIdeal (estanMejoresQueNunca ratones.reduceFatFast)






