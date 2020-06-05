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


--Punto 2: (3 puntos) Resolver utilizando únicamente orden superior.
--Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años.

aptoParaPendex :: Universo -> Bool 
aptoParaPendex  universo = any (( < 45 ).edad) universo 

--Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad.

energiaTotal :: Universo -> Int 

energiaTotal  universo = foldl1 (+) (energias universo)

energias :: Universo -> [Int]
energias = map energia . personajesMasDeUnaHabilidad

personajesMasDeUnaHabilidad :: Universo -> Universo
personajesMasDeUnaHabilidad  universo = filter ((>1).length.habilidad) universo


----------------PARTE DOS
{-A su vez, aunque el guantelete no se encuentre completo con las 6 gemas, el poseedor puede utilizar el poder del mismo contra un enemigo, 
es decir que puede aplicar el poder de cada gema sobre el enemigo. Las gemas del infinito fueron originalmente parte de la entidad primordial 
llamada Némesis, un ser todopoderoso del universo anterior quién prefirió terminar su existencia en lugar de vivir como la única conciencia en 
el universo. Al morir, dio paso al universo actual, y el núcleo de su ser reencarnó en las seis gemas: -}

-----------PUNTO 3: (3 puntos) 
--Implementar las gemas del infinito, evitando lógica duplicada. 

--1) La mente que tiene la habilidad de debilitar la energía de un usuario en un valor dado.

laMente :: Int -> Gema
laMente valor personaje = restarEnergia valor personaje

restarEnergia :: Int -> Gema
restarEnergia valor personaje  = personaje {energia = energia personaje - valor}

--2) El alma puede controlar el alma de nuestro oponente permitiéndole eliminar una habilidad en particular si es que la posee. 
--Además le quita 10 puntos de energía. 

elAlma :: String -> Gema
elAlma habilidadAQuitar = restarEnergia 10.quitarHabilidad habilidadAQuitar

quitarHabilidad :: String -> Gema
quitarHabilidad habilidadAQuitar personaje = personaje { habilidad = filter (/= habilidadAQuitar) (habilidad personaje)}

--3) El espacio que permite transportar al rival al planeta x (el que usted decida) y resta 20 puntos de energía.
elEspacio :: String -> Gema
elEspacio planeta  = restarEnergia 20.cambiarPlaneta planeta

cambiarPlaneta :: String -> Gema
cambiarPlaneta planeta personaje = personaje {planeta = planeta}

--4) El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita (en caso contrario no le saca ninguna habilidad).



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

{-5) El tiempo que reduce a la mitad la edad de su oponente pero como no está permitido pelear con menores, no puede dejar la edad del 
oponente con menos de 18 años. Considerar la mitad entera, por ej: si el oponente tiene 50 años, le quedarán 25. Si tiene 45, le quedarán 22 
(por división entera). Si tiene 30 años, le deben quedar 18 en lugar de 15. También resta 50 puntos de energía.-}

elTiempo :: Gema
elTiempo = (restarEnergia 50).nuevaEdad

nuevaEdad :: Gema
nuevaEdad personaje = personaje {edad = cambiarEdad personaje}

cambiarEdad :: Personaje -> Int
cambiarEdad   = max 18. (`div` 2).edad 

--6) La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival.
 
gemaLoca :: Gema -> Gema
gemaLoca gema  = gema.gema 

-----PUNTO 4: (1 punto) 
{-Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad de “usar Mjolnir” y 
la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell”.-}

--guanteEjemplo = UnGuante Otro 3 [tiempo, (alma (“usar Mjolnir”) ), (gemaLoca (alma “programación en Haskell”)) ]
 

{-Punto 5: (2 puntos). No se puede utilizar recursividad. Generar la función utilizar  
que dado una lista de gemas y un enemigo ejecuta el poder de cada una de las gemas que lo componen 
contra el personaje dado. Indicar cómo se produce el “efecto de lado” sobre la víctima.-}


utilizar :: [Gema] -> Gema
utilizar listaGemas enemigo = foldl aplicarGema enemigo listaGemas

aplicarGema :: Personaje -> Gema -> Personaje
aplicarGema personaje gema  = gema personaje

{-Punto 6: (2 puntos). Resolver utilizando recursividad. Definir la función gemaMasPoderosa que dado un 
guantelete y una persona obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima. -}

{-gemaMasPoderosa :: Guante -> Personaje -> Gema
gemaMasPoderosa 
 | energia (gema1 personaje) < energia (gema2 personaje ) = -}


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