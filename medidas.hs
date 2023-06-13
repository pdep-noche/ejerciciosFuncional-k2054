type Bien = (String,Float)
data Ciudadano = UnCiudadano {profesion :: String, sueldo :: Float, 
cantidadDeHijos :: Float, bienes :: [Bien] } deriving Show

homero = UnCiudadano "SeguridadNuclear" 9000 3 [("casa",50000), ("deuda",-70000)]
frink = UnCiudadano "Profesor" 12000 1 []
krabappel = UnCiudadano "Profesor" 12000 0 [("casa",35000)]
burns = UnCiudadano "Empresario" 300000 1 [("empresa",1000000),("empresa",500000),("auto",200000)]

type Ciudad = [Ciudadano]
springfield = [homero, burns, frink, krabappel] 

diferenciaDePatrimonio :: Ciudad -> Float 
diferenciaDePatrimonio ciudad = (patrimonio.ciudadanoSegun maximoPatrimonio) ciudad - (patrimonio.ciudadanoSegun minimoPatrimonio) ciudad


patrimonio :: Ciudadano -> Float
patrimonio (UnCiudadano _ sueldo _ bienes) = foldl (\sem (_, valor) -> sem + valor ) sueldo bienes


ciudadanoSegun :: (Ciudadano -> Ciudadano -> Ciudadano) -> Ciudad -> Ciudadano
ciudadanoSegun f  ciudad = foldl1 f ciudad

maximoPatrimonio :: Ciudadano -> Ciudadano -> Ciudadano
maximoPatrimonio unCiudadano otroCiudadano | patrimonio unCiudadano > patrimonio otroCiudadano = unCiudadano
                                           | otherwise = otroCiudadano


minimoPatrimonio :: Ciudadano -> Ciudadano -> Ciudadano
minimoPatrimonio unCiudadano otroCiudadano | patrimonio unCiudadano < patrimonio otroCiudadano = unCiudadano
                                           | otherwise = otroCiudadano


--   ghci> diferenciaDePatrimonio springfield
--2011000.0

tieneAutoAltaGama :: Ciudadano -> Bool
tieneAutoAltaGama (UnCiudadano _ _ _ bienes) =  any autoAltaGama bienes

autoAltaGama :: Bien -> Bool
autoAltaGama ("auto", valor) = valor > 100000
autoAltaGama _ = False

{-
ghci> tieneAutoAltaGama burns
True
-}

type Medida = Ciudadano -> Ciudadano
auh :: Medida
auh ciudadano = aplicarMedidaSegun (patrimonio ciudadano < 0) (modificarSueldo ((incremento.cantidadDeHijos)ciudadano)) ciudadano

modificarSueldo :: Float -> Ciudadano -> Ciudadano
modificarSueldo cantidad ciudadano = ciudadano { sueldo = sueldo ciudadano + cantidad}

aplicarMedidaSegun :: Bool -> (Ciudadano -> Ciudadano) -> Ciudadano -> Ciudadano
aplicarMedidaSegun condicion f  ciudadano | condicion = f ciudadano
                                          | otherwise = ciudadano

incremento cantidad = cantidad * 1000

{-
ghci> auh homero
UnCiudadano {profesion = "SeguridadNuclear", sueldo = 12000.0, cantidadDeHijos = 3.0, bienes = [("casa",50000.0),("deuda",-70000.0)]}
-}


impuestoGanancias :: Float -> Medida
impuestoGanancias minimo ciudadano = aplicarMedidaSegun (sueldo ciudadano > minimo) (modificarSueldo (diferencia minimo (sueldo ciudadano))) ciudadano

diferencia :: Float -> Float -> Float
diferencia minimo sueldo = (minimo - sueldo) * 0.3

{-
impuestoGanancias 10000 burns
UnCiudadano {profesion = "Empresario", sueldo = 213000.0, cantidadDeHijos = 1.0, bienes = [("empresa",1000000.0),("empresa",500000.0),("auto",200000.0)]}
-}


impuestoAltaGama :: Medida
impuestoAltaGama ciudadano = aplicarMedidaSegun (tieneAutoAltaGama ciudadano) (modificarSueldo ((impuesto.bienes) ciudadano)) ciudadano


impuesto :: [Bien] -> Float
impuesto bienes =  ((*(-0.1)).snd.head.filter autoAltaGama) bienes
{-ghci> impuestoAltaGama burns
UnCiudadano {profesion = "Empresario", sueldo = 280000.0, cantidadDeHijos = 1.0, bienes = [("empresa",1000000.0),("empresa",500000.0),("auto",200000.0)]}
-}


negociarSueldoProfesion :: String -> Float -> Medida
negociarSueldoProfesion unaProfesion porcentaje ciudadano =  aplicarMedidaSegun (((== unaProfesion).profesion)ciudadano)  (modificarSueldo (aumento porcentaje (sueldo ciudadano))) ciudadano

aumento :: Float -> Float -> Float
aumento porcentaje sueldo = (sueldo * porcentaje)/100

{-
ghci> negociarSueldoProfesion "SeguridadNuclear" 20 homero
UnCiudadano {profesion = "SeguridadNuclear", sueldo = 10800.0, cantidadDeHijos = 3.0, bienes = [("casa",50000.0),("deuda",-70000.0)]}
-}


data Gobierno = UnGobierno {años :: [Float], medidas :: [Ciudadano->Ciudadano ]}

gobiernoA :: Gobierno
gobiernoA = UnGobierno [1999..2003] [impuestoGanancias 30000, negociarSueldoProfesion "Profesion" 10, negociarSueldoProfesion "Empresario" 40, impuestoAltaGama, auh ]


gobiernoB :: Gobierno
gobiernoB = UnGobierno [2004..2008] [impuestoGanancias 40000, negociarSueldoProfesion "Profesor" 30 , negociarSueldoProfesion "Camionero" 40]


gobernarUnAño :: Gobierno -> Ciudad -> Ciudad
gobernarUnAño gobierno ciudad = map (aplicarMedidas gobierno)  ciudad

aplicarMedidas :: Gobierno -> Ciudadano -> Ciudadano
aplicarMedidas gobierno ciudadano = foldl  (flip ($))  ciudadano (medidas gobierno)

{-
ghci> gobernarUnAño gobiernoA springfield
[UnCiudadano {profesion = "SeguridadNuclear", sueldo = 12000.0, cantidadDeHijos = 3.0, bienes = [("casa",50000.0),("deuda",-70000.0)]},UnCiudadano {profesion = "Empresario", sueldo = 286600.0, cantidadDeHijos = 1.0, bienes = [("empresa",1000000.0),("empresa",500000.0),("auto",200000.0)]},UnCiudadano {profesion = "Profesor", sueldo = 12000.0, cantidadDeHijos = 1.0, bienes = []},UnCiudadano {profesion = "Profesor", sueldo = 12000.0, cantidadDeHijos = 0.0, bienes = [("casa",35000.0)]}]
-}

gobernarPeriodoCompleto :: Gobierno -> Ciudad -> Ciudad
gobernarPeriodoCompleto unGobierno unaCiudad = foldl (flip ($)) unaCiudad (replicate ((length.años) unGobierno) (gobernarUnAño unGobierno))


gobernarPeriodoCompleto2 :: Gobierno -> Ciudad -> Ciudad
gobernarPeriodoCompleto2 unGobierno unaCiudad = foldl (\ciudad _ -> gobernarUnAño unGobierno ciudad) unaCiudad (años unGobierno)


distribuyoRiqueza :: Gobierno -> Ciudad -> Bool
distribuyoRiqueza unGobierno unaCiudad = diferenciaDePatrimonio unaCiudad > (diferenciaDePatrimonio.gobernarPeriodoCompleto unGobierno) unaCiudad

kane :: Ciudadano
kane = UnCiudadano "Empresario"  1000000 0 [("Rosebud", valor)| valor <- [5,10..]]

f1:: (Num a) =>  a  -> ( b ->  a -> Bool)   -> b    ->  [a] -> [a]
f1 x y z = map (*x) . filter (y z) 





