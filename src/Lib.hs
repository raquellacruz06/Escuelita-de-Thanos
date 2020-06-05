module Lib where
import Text.Show.Functions

laVerdad = True

data Personaje = UnPersonaje {
    edad :: Int, 
    energia :: Int,
    habilidad :: [String],
    nombre :: String,
    planeta :: String
} deriving (Show, Eq)

type Gema = Personaje -> Personaje 
data Material = Uru | Hierro | Otro deriving (Show, Eq)
type Universo = [Personaje]

data Guante = UnGuante {
    material :: Material,
    cantidadGemas :: Int,
    gemas :: [Gema]
}

chasquidoUniverso :: Guante -> Universo -> Universo
chasquidoUniverso (UnGuante Uru 6 gemas) universo = eliminarPersonajes universo 
chasquidoUniverso guante universo = universo

eliminarPersonajes :: Universo -> Universo
eliminarPersonajes universo = take (mitadUniverso universo) universo

mitadUniverso :: Universo -> Int
mitadUniverso  = (`div` 2).length 


------------Punto 2: (3 puntos) Resolver utilizando únicamente orden superior.


aptoParaPendex :: Universo -> Bool 
aptoParaPendex  universo = any (( < 45 ).edad) universo 

energiaTotal :: Universo -> Int 

energiaTotal  universo = foldl1 (+) (energias universo)

energias :: Universo -> [Int]
energias = map energia . personajesMasDeUnaHabilidad

personajesMasDeUnaHabilidad :: Universo -> Universo
personajesMasDeUnaHabilidad  universo = filter ((>1).length.habilidad) universo

-------------PUNTO 3
laMente :: Int -> Gema
laMente valor personaje = restarEnergia valor personaje

restarEnergia :: Int -> Gema
restarEnergia valor personaje  = personaje {energia = energia personaje - valor}

elAlma :: String -> Gema
elAlma habilidadAQuitar = restarEnergia 10.quitarHabilidad habilidadAQuitar

quitarHabilidad :: String -> Gema
quitarHabilidad habilidadAQuitar personaje = personaje { habilidad = filter (/= habilidadAQuitar) (habilidad personaje)}

elEspacio :: String -> Gema
elEspacio planeta  = restarEnergia 20.cambiarPlaneta planeta

cambiarPlaneta :: String -> Gema
cambiarPlaneta planeta personaje = personaje {planeta = planeta}

elPoder :: Gema
elPoder personaje = (restarEnergia (energia personaje).quitarHabilidades) personaje
--elPoder = quitarEnergia.quitarHabilidades

--quitarEnergia :: Gema
--quitarEnergia personaje = personaje {energia = 0}

quitarHabilidades :: Gema
quitarHabilidades personaje 
    | ((<2).length.habilidad) personaje = sacarHabilidades personaje
    | otherwise = personaje                        

sacarHabilidades :: Gema
sacarHabilidades personaje = personaje {habilidad = []}

--Se podría mejorar evitando la repetición de lógica en quitarHabilidades

elTiempo :: Gema
elTiempo = (restarEnergia 50).nuevaEdad

nuevaEdad :: Gema
nuevaEdad personaje = personaje {edad = cambiarEdad personaje}

cambiarEdad :: Personaje -> Int
cambiarEdad   = max 18. (`div` 2).edad 
 
gemaLoca :: Gema -> Gema
gemaLoca gema  = gema.gema 

-----PUNTO 4: (1 punto) 
{-Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad de “usar Mjolnir” y 
la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell”.-}

--guanteEjemplo = UnGuante Otro 3 [tiempo, (alma (“usar Mjolnir”) ), (gemaLoca (alma “programación en Haskell”)) ]
 

-------PUNTO 5
utilizar :: [Gema] -> Gema
utilizar listaGemas enemigo = foldl aplicarGema enemigo listaGemas

aplicarGema :: Personaje -> Gema -> Personaje
aplicarGema personaje gema  = gema personaje

--Punto 6: 

gemaMasPoderosa :: Guante -> Personaje-> Gema
gemaMasPoderosa guante personaje = (gemasPoderosas personaje) $ gemas guante 

gemasPoderosas :: Personaje ->[Gema]-> Gema
gemasPoderosas _ [gema1]  = gema1
gemasPoderosas personaje listaGemas = hallarMasPoderosa personaje listaGemas 

hallarMasPoderosa :: Personaje -> [Gema]->  Gema
hallarMasPoderosa  personaje (gema1: gema2: masGemas)
 | (energia.gema1) personaje <= (energia.gema2) personaje  = gemasPoderosas personaje (gema1:masGemas)
 | otherwise = gemasPoderosas personaje (gema2:masGemas)


--PUNTO 7 respondido
{-Punto 7: (1 punto) Dada la función generadora de gemas y un guantelete de locos:
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

Y la función 
usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
gemaMasPoderosa punisher guanteleteDeLocos
No se puede ejecutar porque la gemaMasPoderosa necesita ver todos los elementos de la lista antes de poder arrojar 
un resultado (Eager evaluation), y como guanteLete tiene infinitas gemas nunca llegará a ese resultado

usoLasTresPrimerasGemas guanteleteDeLocos punisher
Esta sí se puede implementar gracias a lazy evaluation ya que, aunque gemas devuelva una lista infinita, take 3 no necesita 
conocer toda la lista para tomar los primeros 3. Por lo tanto aplica las 3 primeras gemas al personajesMasDeUnaHabilidad

-}