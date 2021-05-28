module Influencers where
import PdePreludat

--Los influenciables
--Existen personas, y de cada una se conocen varias cosas:

-- Qué temas le gusta.
-- Qué cosas le dan miedo y en qué medida (expresado numéricamente).
-- Su estabilidad actual, representada en una escala de 0 a 100.

type Gusto=String
type ListaGustos = [Gusto]

type Algo = String
type Medida = Number

type Miedo = (Algo,Medida)
type ListaMiedos = [Miedo]

data Influenciable = UnInfluenciable {
 gustos :: ListaGustos,
 miedos :: ListaMiedos,
 estabilidad :: Number
} deriving (Eq, Show)

maria = UnInfluenciable ["mecánica"] [("extraterrestres", 600),("quedarse sin trabajo", 300)] 85
juanCarlos = UnInfluenciable ["maquillaje","trenes"] [("insectos", 100), ("coronavirus", 10),("vacunas",20)] 50

agregarMedida algoNuevo medidaNueva (algo,medida)
 | algoNuevo==algo = (algo,medida+medidaNueva)
 | otherwise = (algo,medida)

-- 1 Que una persona se vuelva miedosa a algo en cierta medida, agregándole el nuevo miedo o aumentando su medida, en caso de ya tener ese miedo previamente.
modificarMiedo algoNuevo medidaNueva influenciable
 | any ((==algoNuevo).fst) (miedos influenciable) = influenciable {miedos=map (agregarMedida algoNuevo medidaNueva) (miedos influenciable)}
 | otherwise = influenciable {miedos= (algoNuevo,medidaNueva):(miedos influenciable)}

-- 2 Que una persona pierda pierda el miedo a algo, lo que independiente de en qué medida lo tuviera, lo deja de tener. (En caso de no tener ese miedo, no cambia nada)
pierdaElMiedo algo influenciable =influenciable {miedos= filter ((/=algo).fst) (miedos influenciable)}

-- 3 Que una persona pueda variar su estabilidad en una cierta proporción dada, pero sin salirse de los límites de la escala.

nuevaEstabilidad proporcion influenciable = ((proporcion*estabilidad influenciable/100)+estabilidad influenciable)

variarEstabilidad proporcion influenciable
 | (-1) < (nuevaEstabilidad proporcion influenciable) &&  (nuevaEstabilidad proporcion influenciable)< 101 = influenciable {estabilidad = (nuevaEstabilidad proporcion influenciable) }
 | (nuevaEstabilidad proporcion influenciable) > 100 = influenciable {estabilidad = 100}
 | otherwise = influenciable {estabilidad = 0}

-- 4 Que una persona se vuelva fan de otra persona, en ese caso asume como propios todos los gustos de la otra persona (si ya tenía previamente alguno de esos gustos, lo va a tener repetido)
volverseFan influenciable idolo = influenciable {gustos = (gustos influenciable)++(gustos idolo)}

-- 5 Averiguar si una persona es fanática de un gusto dado, que es cuando tiene más de tres veces dicho gusto.
esFanaticaDeUnGusto gusto= (>2).length.(filter (==gusto)).gustos

--6 Averiguar si una persona es miedosa, cuando el total de la medida de todos sus miedos sumados supera 1000.
esMiedosa = (>1000).(foldl1 ((+))).(map (snd)).miedos