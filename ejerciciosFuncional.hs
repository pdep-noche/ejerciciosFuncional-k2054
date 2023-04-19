
data Politico = Politico {proyectosPresentados :: [String], sueldo :: Integer, edad :: Int } deriving Show

find' criterio lista =  (head . filter criterio) lista 


julio :: Politico
julio =  Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81


politicos = [ Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81, Politico ["tratar de reconquistar luchas sociales"] 10000 63, Politico ["tolerancia 100 para delitos"] 15500 49 ]


{-  a)

ghci> find' ((<50).edad) politicos
Politico {proyectosPresentados = ["tolerancia 100 para delitos"], sueldo = 15500, edad = 49}
-}  

{- b

ghci> find' ((>3). length. proyectosPresentados ) politicos
Politico {proyectosPresentados = ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 
19182"], sueldo = 20000, edad = 81}
-}

{- c

ghci> find' (any((>3).length.words).proyectosPresentados) politicos
Politico {proyectosPresentados = ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 19182"], sueldo = 20000, edad = 81}

-}


type Nombre = String
type Notas = [Int]
data Persona = Alumno {nombre :: Nombre, notas :: Notas}


promediosAlumnos :: [Persona] -> [(Nombre, Int)]
promediosAlumnos alumnos = map (\alumno -> (nombre alumno, (promedio.notas) alumno)) alumnos

promedio :: Notas -> Int
promedio notas = (sum notas) `div` (length notas)