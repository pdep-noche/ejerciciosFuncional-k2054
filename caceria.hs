import Text.Show.Functions
import Distribution.Backpack.PreModuleShape (PreModuleShape)

data Personaje = Personaje { nombre :: String, fuerza :: Float, experiencia :: Float, elemento :: Elemento} deriving Show

type Elemento = Float -> Float


julia :: Personaje
julia = Personaje "julia" 30 50 (sableLambdico 89)

espadaOxidada = (1.2*)
katanaFilosa = (10+).(0.9*)
sableLambdico cm = ((1+cm/100)*)
redParadigmatica = sqrt
baculoDuplicador x= x* 2
espadaMaldita = espadaOxidada.sableLambdico 89


nivel (Personaje _  _ experiencia _) = ceiling(experiencia^2/experiencia + 1)

capacidad :: Personaje -> Float 
capacidad (Personaje _ fuerza _ elemento) = elemento fuerza

type Alquimista = Personaje -> Personaje

aprendiz :: Alquimista
aprendiz personaje = alterarElemento (2*) personaje

alterarElemento :: Elemento -> Alquimista
alterarElemento f personaje = personaje { elemento = f.elemento personaje}

maestro :: Float -> Alquimista
maestro años personaje = (alterarElemento (coeficienteAntiguedad años).aprendiz) personaje

coeficienteAntiguedad 0 = id
coeficienteAntiguedad años = (*1.1).coeficienteAntiguedad (años - 1)


estafador :: Alquimista
estafador personaje = personaje {elemento = id}

inventado :: Alquimista
inventado personaje = personaje { elemento = (\x -> x * 7)}

capacidadesSuperioresA :: Float -> Personaje -> [Alquimista] -> [Alquimista]
capacidadesSuperioresA valor personaje alquimistas = filter (capacidadSuperiorA valor personaje) alquimistas

capacidadSuperiorA :: Float -> Personaje  -> Alquimista -> Bool
capacidadSuperiorA valor personaje alquimista = ((>valor).capacidad.alquimista ) personaje

convieneATodos :: [Alquimista] -> Personaje -> Bool
convieneATodos alquimistas personaje  = all (capacidadSuperiorA (capacidad personaje) personaje) alquimistas

data Monstruo = Monstruo {especie :: String, resistencia :: Float, habilidades :: [Habilidad]}
 
type Habilidad = (String, String)

esAgresivo :: Monstruo -> Bool
esAgresivo monstruo = (not.especieInofensiva.especie) monstruo && (mayoriaHabilidadesOfensiva.habilidades) monstruo &&  ((>0).resistencia) monstruo

especieInofensiva :: String -> Bool
especieInofensiva especie = elem especie ["animal", "chocobo"]

tipo  = snd

mayoriaHabilidadesOfensiva :: [Habilidad ] -> Bool
mayoriaHabilidadesOfensiva habilidades = (length.filter(esOfensiva.tipo)) habilidades > div (length habilidades) 2

esOfensiva :: String -> Bool
esOfensiva "magia" = True
esOfensiva "fisica"  = True
esOfensiva _ = False


troll = Monstruo "troll" 60 [("patada", "fisica")]

leGana :: Personaje -> Monstruo -> Bool
leGana personaje monstruo = capacidad personaje > resistencia monstruo


pelearConTodos :: Personaje -> [Monstruo] -> Personaje
pelearConTodos personaje monstruos = foldl pelearCon  personaje    monstruos

pelearCon :: Personaje -> Monstruo -> Personaje
pelearCon personaje monstruo | leGana personaje monstruo = modificarExperiencia 100 personaje
                             | otherwise =  alterarElemento (0.9*). (modificarExperiencia (-50)) $ personaje

modificarExperiencia :: Float -> Personaje -> Personaje
modificarExperiencia valor personaje = personaje { experiencia = experiencia personaje + valor}

hayInvensible :: Personaje -> [Monstruo] -> Alquimista -> Bool
hayInvensible personaje monstruos alquimista = any ( not. leGana (alquimista personaje)) monstruos