import Text.Show.Functions

data Usuario = Usuario {nick :: String, indiceFelicidad :: Float, librosAdquiridos :: [Libro], librosLeidos :: [Libro]} deriving Show
type Genero = Usuario -> Usuario
data Libro = Libro {titulo :: String, autor :: String, cantPaginas :: Int, genero :: Genero} deriving Show

comedia :: String -> Genero
comedia "dramatica" persona = persona
comedia "absurdas"  persona = modificarFelicidad (+5) persona
comedia "satiricas" persona = modificarFelicidad (2*) persona
comedia _  persona = modificarFelicidad (+10) persona

modificarFelicidad :: (Float -> Float) -> Genero
modificarFelicidad f persona = persona {indiceFelicidad = (f.indiceFelicidad) persona}

cienciaFiccion :: Genero
cienciaFiccion persona = persona {nick = (reverse.nick) persona}


terror :: Genero
terror persona = persona {librosAdquiridos = []}

magoDeTerramar :: Libro
magoDeTerramar = Libro "Un Mago de Terramar" "Ursula Le Guin" 250  cienciaFiccion

unMundoFeliz :: Libro
unMundoFeliz = Libro "Un mundo feliz" "Adolf Huxley" 500 cienciaFiccion

ignacio = Usuario "nacho" 60 [magoDeTerramar, unMundoFeliz] [unMundoFeliz]




