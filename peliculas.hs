import Text.Show.Functions
import Data.Text.Unsafe (unsafeDupablePerformIO)

data Pelicula = Pelicula {nombreP :: String, genero :: String, duracion :: Int, origen :: String } deriving (Show, Eq)

data Usuario = Usuario {nombre :: String,  categoria :: String, edad :: Int, paisResidencia :: String,  peliculas :: [Pelicula], estadoSalud :: Int} deriving Show

psicosis = Pelicula "Psicosis" "Te:rror" 109 "Estados Unidos"
perfumeDeMujer= Pelicula "Perfume de Mujer" "Drama" 150  "Estados Unidos"
elSaborDeLasCervezas = Pelicula "El sabor de las cervezas"  "Drama" 95 "Iran"
lasTortugasTambienVuelan = Pelicula "Las tortugas también vuelan" "Drama" 103 "Iran"
juan = Usuario "juan" "estandar" 23  "Argentina" [perfumeDeMujer, elSaborDeLasCervezas] 60


ver :: Pelicula -> Usuario -> Usuario
ver unaPelicula unUsuario = unUsuario { peliculas = peliculas unUsuario ++ [unaPelicula]}


premiar :: [Usuario] -> [Usuario]
premiar usuarios = map premiarSiEsFiel usuarios

premiarSiEsFiel :: Usuario -> Usuario
premiarSiEsFiel unUsuario | cumpleCondiciones unUsuario = subirCategoria unUsuario
                          | otherwise = unUsuario


cumpleCondiciones :: Usuario -> Bool
cumpleCondiciones usuario = (>=1). length . peliculasQueNoSeanDe "Estados Unidos" . peliculas $ usuario

peliculasQueNoSeanDe :: String -> [Pelicula]  -> [Pelicula]
peliculasQueNoSeanDe pais peliculas = filter ((pais /=).origen)  peliculas


subirCategoria :: Usuario -> Usuario
subirCategoria usuario = usuario { categoria = (nuevaCategoria.categoria) usuario}

nuevaCategoria :: String -> String
nuevaCategoria "basica" = "estandar"
nuevaCategoria _ = "premium"

type Criterio = Pelicula -> Bool

teQuedasteCorto :: Criterio
teQuedasteCorto unaPelicula = (<35).duracion $ unaPelicula

cuestionDeGenero :: [String] -> Criterio
cuestionDeGenero generos pelicula = any (==(genero pelicula)) generos 

{- ghci> cuestionDeGenero ["Comedia", "Drama"] elSaborDeLasCervezas
True -}

deDondeSaliste :: String -> Criterio
deDondeSaliste unOrigen  pelicula = (== unOrigen).origen $ pelicula

{-}
ghci> deDondeSaliste "Iran" elSaborDeLasCervezas
True
-}

vaPorEseLado :: (Eq t) =>  Pelicula -> (Pelicula -> t) -> Criterio
vaPorEseLado pelicula caracteristica otraPelicula = caracteristica pelicula == caracteristica otraPelicula


busquedaDePeliculas :: Usuario -> [Criterio] -> [Pelicula] -> [Pelicula]
busquedaDePeliculas usuario criterios peliculas = take 3. filter (esRecomendablePara usuario criterios) $ peliculas

esRecomendablePara :: Usuario -> [Criterio] -> Pelicula -> Bool
esRecomendablePara usuario criterios pelicula = (not. vio pelicula) usuario && cumpleCriterios pelicula criterios 


vio:: Pelicula -> Usuario -> Bool
vio pelicula usuario = elem pelicula (peliculas usuario)


cumpleCriterios :: Pelicula -> [Criterio] -> Bool
cumpleCriterios  pelicula criterios = all ($ pelicula) criterios


{-
ghci> busquedaDePeliculas juan [deDondeSaliste "Iran", cuestionDeGenero [ "Drama", "Comedia"],(not.teQuedasteCorto)] [psicosis, perfumeDeMujer, elSaborDeLasCervezas] 
[]
-}


data Capitulo = Capitulo {nombreC :: String, generoC :: String,  duracionC :: Int, origenC :: String, afecta :: (Usuario -> Usuario)} deriving Show

consumenSeries :: Usuario -> Capitulo -> Usuario
consumenSeries usuario capitulo = (afecta capitulo) usuario


{-ghci> consumenSeries juan capitulo
Usuario {nombre = "juan", categoria = "estandar", edad = 23, paisResidencia = "Argentina", peliculas = [Pelicula {nombreP = "Perfume de Mujer", genero = "Drama", duracion = 150, origen = "Estados Unidos"},Pelicula {nombreP = "El sabor de las cervezas", genero = "Drama", duracion = 95, origen = "Iran"}], estadoSalud = 40}
-}

type Serie = [Capitulo]
maraton :: Usuario -> Serie -> Usuario
maraton usuario serie = foldl consumenSeries usuario serie

{-
ghci> maraton juan [capitulo]
Usuario {nombre = "juan", categoria = "estandar", edad = 23, paisResidencia = "Argentina", peliculas = [Pelicula {nombreP = "Perfume de Mujer", genero = "Drama", duracion = 150, origen = "Estados Unidos"},Pelicula {nombreP = "El sabor de las cervezas", genero = "Drama", duracion = 95, origen = "Iran"}], estadoSalud = 40}
-}

serieInfinita = repeat capitulo


{-ghci> maraton juan ( take 3 serieInfinita)
Usuario {nombre = "juan", categoria = "estandar", edad = 23, paisResidencia = "Argentina", peliculas = [Pelicula {nombreP = "Perfume de Mujer", genero = "Drama", duracion = 150, origen = "Estados Unidos"},Pelicula {nombreP = "El sabor de las cervezas", genero = "Drama", duracion = 95, origen = "Iran"}], estadoSalud = 0}
-}


capitulo = Capitulo "Screem" "Terror" 40 "Estados Unidos" (\usuario -> usuario { estadoSalud = (estadoSalud usuario) - 20})