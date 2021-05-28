module Vacaciones where
import PdePreludat

-- 1 Crear un modelo para los turistas y crear los siguientes tres ejemplos:

--De cada turista nos interesa:
--Sus niveles de cansancio y stress
--Si está viajando solo
--Los idiomas que habla

type Idioma = String

data Turista = UnTurista {
cansancio :: Number,
stress :: Number,
viajaSolo :: Bool,
idiomas :: [Idioma]

} deriving (Show)

ana = UnTurista 0 21 False ["Español"]
beto = UnTurista 15 15 True ["Alemán"]
cathi = UnTurista 15 15 True ["Alemán", "Catalán"]

-- 2 Modelar las excursiones anteriores de forma tal que para agregar una excursión
-- al sistema no haga falta modificar las funciones existentes

--La isla contiene varias excursiones para los turistas, por ahora nos pidieron modelar estas:

--Ir a la playa: si está viajando solo baja el cansancio en 5 unidades, si no baja el stress 1 unidad.
irALaPlaya :: Turista->Turista
irALaPlaya turista
 | viajaSolo turista = turista {cansancio=(cansancio turista-5)}
 | otherwise = turista {stress=(stress turista-1)}

--Apreciar algún elemento del paisaje: reduce el stress en la cantidad de letras de lo que se aprecia.
type Elemento = String
apreciarElemento :: Elemento -> Turista -> Turista 
apreciarElemento elemento turista = turista {stress=(stress turista - length elemento)}

--Salir a hablar un idioma específico: el turista termina aprendiendo dicho idioma y continúa el viaje acompañado.
salirAHablar :: Idioma -> Turista -> Turista
salirAHablar idioma turista
 | elem idioma (idiomas turista) = turista {viajaSolo = False}
 | otherwise = turista {viajaSolo = False, idiomas=(idioma:idiomas turista)} 

--Caminar ciertos minutos: aumenta el cansancio pero reduce el stress según la intensidad de la caminad, ambos en la misma cantidad. El nivel de intensidad se calcula en 1 unidad cada 4 minutos que se caminen.
intensidadCaminata :: Number -> Number
intensidadCaminata minutos = minutos/4
caminarCiertosMinutos :: Number -> Turista -> Turista
caminarCiertosMinutos minutos turista = turista {cansancio=(cansancio turista+intensidadCaminata minutos), stress=(stress turista-intensidadCaminata minutos) }

--Paseo en barco: depende de cómo esté la marea

--si está fuerte, aumenta el stress en 6 unidades y el cansancio en 10.
--si está moderada, no pasa nada.
--si está tranquila, el turista camina 10’ por la cubierta, aprecia la vista del “mar”, y sale a hablar con los tripulantes alemanes.
paseoEnBarco :: String -> Turista -> Turista
paseoEnBarco marea turista
 | marea == "fuerte" = turista {stress = stress turista+6, cansancio=cansancio turista+10} 
 | marea == "moderada" = turista
 | marea == "tranquila" = salirAHablar "Alemán" (apreciarElemento "mar" (caminarCiertosMinutos 10 turista))
 | otherwise = turista

--2. a) Hacer que un turista haga una excursión. Al hacer una excursión, el turista además de sufrir los efectos propios de la excursión, reduce en un 10% su stress.

type Excursion = Turista->Turista


hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion turista= (excursion turista) {stress= (stress (excursion turista))-(0.1*stress (excursion turista))}

--Dada la función
deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

--2. b) Definir la función deltaExcursionSegun que a partir de un índice, un turista y una excursión determine cuánto varió dicho índice después de que el turista 
--haya hecho la excursión. Llamamos índice a cualquier función que devuelva un número a partir de un turista.

deltaExcursionSegun :: (Turista -> Number) -> Turista -> Excursion -> Number
deltaExcursionSegun indice turista excursion = deltaSegun indice (hacerExcursion excursion turista) (turista)

--2. c) Usar la función anterior para resolver cada uno de estos puntos:
--Saber si una excursión es educativa para un turista, que implica que termina aprendiendo algún idioma.
esEducativa :: Excursion -> Turista -> Bool
esEducativa excursion turista = deltaExcursionSegun (length.idiomas) turista excursion > 0
--Conocer las excursiones desestresantes para un turista. Estas son aquellas que le reducen al menos 3 unidades de stress al turista.
esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista excursion = deltaExcursionSegun (stress) turista excursion < (-3)

type Tour = [Excursion]

--Completo: Comienza con una caminata de 20 minutos para apreciar una "cascada", luego se camina 40 minutos hasta una playa, y finaliza con una salida con gente local que habla "melmacquiano".
completo :: Tour
completo = [salirAHablar "melmacquiano", irALaPlaya, caminarCiertosMinutos 40, apreciarElemento "cascada", caminarCiertosMinutos 20]
--Lado B: Este tour consiste en ir al otro lado de la isla a hacer alguna excursión (de las existentes) que elija el turista. 
--Primero se hace un paseo en barco por aguas tranquilas (cercanas a la costa) hasta la otra punta de la isla, luego realiza la excursión elegida y finalmente vuelve caminando hasta la otra punta, tardando 2 horas.
ladoB :: Excursion->Tour
ladoB excursion = [caminarCiertosMinutos 120,hacerExcursion excursion,paseoEnBarco "tranquila"]

--Isla Vecina: Se navega hacia una isla vecina para hacer una excursión. 
--Esta excursión depende de cómo esté la marea al llegar a la otra isla: si 
--está fuerte se aprecia un "lago", sino se va a una playa. 
--En resumen, este tour implica hacer un paseo en barco hasta la isla vecina, 
--luego llevar a cabo dicha excursión, 
--y finalmente volver a hacer un paseo en barco de regreso. La marea es la misma en todo el camino.
islaVecina :: String->Tour
islaVecina marea
 | marea == "fuerte" = [paseoEnBarco marea, hacerExcursion (apreciarElemento "lago"),paseoEnBarco marea]
 | otherwise = [paseoEnBarco marea , hacerExcursion irALaPlaya, paseoEnBarco marea]

--3. a)
--foldr porque la primer excursión es la última de la lista
hacerTour :: Turista -> Tour -> Turista
hacerTour turista tour = foldr (hacerExcursion) turista {stress=stress turista+length tour} tour

--3. b)
--Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista. 
--Esto significa que el tour tiene alguna excursión desestresante la cual, 
-- además, deja al turista acompañado luego de realizarla.

--para test
noParaBeto :: Tour
noParaBeto = [irALaPlaya]

--alguna excursion del tour es desestresante y lo deja acompañado
esConvincente :: Turista -> Tour -> Bool 
esConvincente turista = any (\excursion -> (esDesestresante turista excursion) && (not (viajaSolo (hacerExcursion excursion turista))))

algunoEsConvincente :: Turista -> [Tour] -> Bool
algunoEsConvincente turista tours = any (esConvincente turista) tours

-- 3. c)
-- Saber la efectividad de un tour para un conjunto de turistas.
-- Esto se calcula como la sumatoria de la espiritualidad recibida de cada turista a quienes les resultó  convincente el tour. 
-- La espiritualidad que recibe un turista es la suma de las pérdidas de stress y cansancio tras el tour.
turistasConvencidos :: [Turista] -> Tour -> [Turista]
turistasConvencidos turistas tour = filter (flip esConvincente tour) turistas

espiritualidad :: Tour -> Turista -> Number 
espiritualidad tour turista = deltaSegun stress (hacerTour turista tour) turista + deltaSegun cansancio (hacerTour turista tour) turista

efectividad :: [Turista] -> Tour -> Number
efectividad turistas tour = sum(map (espiritualidad tour) (turistasConvencidos turistas tour))

-- 4. Implementar y contestar en modo de comentarios o pruebas por consola
-- Construir un tour donde se visiten infinitas playas.

tourInfinitasPlayas :: Tour
tourInfinitasPlayas = [irALaPlaya]++tourInfinitasPlayas

-- ¿Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.
--No, ya que sólo se puede evaluar acotando evaluando una parte del tour ej. tomando las 10 primeras excursiones
-- lo cual no reflejaría la completitud
-- esConvincente ana (take 10 (tourInfinitasPlayas))
-- Si se evalúa el tour completo no otorga respuesta por quedarse evaluando infinitamente.

-- ¿Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar.
-- No ya que el tour de playas no modifica si viajaSolo y en caso de que viaje acompañado (que es una de las condiciones) 
-- solo disminuye el cansancio.
