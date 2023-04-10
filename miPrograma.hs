import Data.Array.IArray (array)
-- siguiente :: Integer -> Integer
siguiente nro = nro + 1

calcular nro | even nro = siguiente nro
             | otherwise = doble nro

doble :: Integer -> Integer
doble nro = nro * 2


aproboAlumno :: Integer -> Bool
aproboAlumno nota = nota >= 6

calcular' :: (Integer, Integer) -> (Integer, Integer)
calcular' (nro1, nro2) = (duplicarPar nro1, suma1Impar nro2)

duplicarPar nro | even nro = doble nro
                | otherwise = nro

suma1Impar nro | odd nro = siguiente nro
                | otherwise = nro

and' :: Bool -> Bool -> Bool
and' unBool otroBool | unBool = otroBool
                     | otherwise = False 


and'' :: Bool -> Bool -> Bool
and'' True otroBool = otroBool
and'' _  _  = False 


or' :: Bool -> Bool -> Bool
or' unBool otroBool | unBool = True
                    | otherwise  = otroBool

or'' :: Bool -> Bool -> Bool
or'' True _ = True
or''  _ otroBool = otroBool


type Nota = Integer
type Alumno = (String, Nota, Nota, Nota)

notaMaxima :: Alumno -> Nota
notaMaxima (_, nota1,nota2, nota3) =  nota1  `max` (nota2 `max` nota3)