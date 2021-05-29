module Influencers where
import PdePreludat

-- Parte 1 Los influenciables
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

agregarMedida :: Algo -> Medida -> Miedo -> Miedo
agregarMedida algoNuevo medidaNueva (algo,medida)
 | algoNuevo==algo = (algo,medida+medidaNueva)
 | otherwise = (algo,medida)

-- 1 Que una persona se vuelva miedosa a algo en cierta medida, agregándole el nuevo miedo o aumentando su medida, en caso de ya tener ese miedo previamente.
modificarMiedo :: Algo -> Medida -> Influenciable -> Influenciable
modificarMiedo algoNuevo medidaNueva influenciable
 | any ((==algoNuevo).fst) (miedos influenciable) = influenciable {miedos=map (agregarMedida algoNuevo medidaNueva) (miedos influenciable)}
 | otherwise = influenciable {miedos= (algoNuevo,medidaNueva):(miedos influenciable)}

-- 2 Que una persona pierda pierda el miedo a algo, lo que independiente de en qué medida lo tuviera, lo deja de tener. (En caso de no tener ese miedo, no cambia nada)
pierdaElMiedo :: Algo -> Influenciable -> Influenciable
pierdaElMiedo algo influenciable =influenciable {miedos= filter ((/=algo).fst) (miedos influenciable)}

-- 3 Que una persona pueda variar su estabilidad en una cierta proporción dada, pero sin salirse de los límites de la escala.

--Versión en caso de que sea tomado en % ej. aumentar la estabilidad un 10%
--nuevaEstabilidad proporcion influenciable = ((proporcion*estabilidad influenciable/100)+estabilidad influenciable)
nuevaEstabilidad :: Number -> Influenciable -> Number
nuevaEstabilidad proporcion influenciable = proporcion+(estabilidad influenciable)

variarEstabilidad :: Number -> Influenciable -> Influenciable
variarEstabilidad proporcion influenciable
 | ((-1) < (nuevaEstabilidad proporcion influenciable)) &&  ((nuevaEstabilidad proporcion influenciable)< 101) = influenciable {estabilidad = (nuevaEstabilidad proporcion influenciable) }
 | (nuevaEstabilidad proporcion influenciable) > 100 = influenciable {estabilidad = 100}
 | otherwise = influenciable {estabilidad = 0}

-- 4 Que una persona se vuelva fan de otra persona, en ese caso asume como propios todos los gustos de la otra persona (si ya tenía previamente alguno de esos gustos, lo va a tener repetido)
volverseFan :: Influenciable -> Influenciable -> Influenciable
volverseFan influenciable idolo = influenciable {gustos = (gustos influenciable)++(gustos idolo)}

-- 5 Averiguar si una persona es fanática de un gusto dado, que es cuando tiene más de tres veces dicho gusto.
esFanaticaDeUnGusto :: Gusto -> Influenciable -> Bool
esFanaticaDeUnGusto gusto= (>2).length.(filter (==gusto)).gustos

--6 Averiguar si una persona es miedosa, cuando el total de la medida de todos sus miedos sumados supera 1000.
esMiedosa :: Influenciable -> Bool
esMiedosa = (>1000).(foldl1 ((+))).(map (snd)).miedos

-- Parte 2 Los influenciadores

-- Hay uno, llamado <coloque aquí su nombre>, que podría intervenirle la televisión a María para hacerle creer que los extraterrestres están instalando una falsa pandemia. 
--El impacto sería que se disminuya su estabilidad en 20 unidades, que tenga miedo a los extraterrestres en 100 y al coronavirus en 50.
interviniendoTV :: Influenciable -> Influenciable
interviniendoTV  = (modificarMiedo "coronavirus" 50).(modificarMiedo "extraterrestres" 100).(variarEstabilidad 20)

agregarGusto :: Gusto -> Influenciable -> Influenciable
agregarGusto gusto influenciable = influenciable {gustos= gusto:(gustos influenciable)}

-- Hay otro que hace que una persona le de miedo a la corrupción en 10, le pierda el miedo a convertirse en Venezuela y que agrega el gusto por escuchar.
multiMiedo :: Influenciable -> Influenciable
multiMiedo = (agregarGusto "escuchar").(pierdaElMiedo "convertirse en Venezuela").(modificarMiedo "corrupción" 10)

-- El community manager de un artista es un influencer que hace que la gente se haga fan de dicho artista.
communityManager :: Influenciable -> Influenciable-> Influenciable
communityManager artista = volverseFan artista

-- Está el influencer inutil, que no lograr alterar nada.
inutil :: Influenciable -> Influenciable
inutil = id

-- Agregá uno a tu elección, pero que tambien realice uno o más cambios en una persona.
-- El influencer gamer que hace que la gente pierda el piedo a perder y le guste el trap, aunque hace que su estabilidad mental disminuya en 20.
-- tambien que le gusten los extraterrestres para que sirva para test
gamer :: Influenciable -> Influenciable
gamer = (agregarGusto "extraterrestres").(agregarGusto "trap").(variarEstabilidad (-20) ).(pierdaElMiedo "perder")

-- 1 Hacer una campaña de marketing básica, que dado un conjunto de personas hace que todas ellas sean influenciadas por un influencer dado.

campaniaDeMarketing influencer = map influencer


miedosInfluenciables listaInfluenciables = map miedos listaInfluenciables


cantidadMiedos = map length

-- 2 Saber qué influencer es más generador de miedo: dada una lista de personas y dos influencer, saber cuál de ellos provoca que más personas se vuelvan miedosas.
cualEsPeor influencer1 influencer2 influenciables
 | ((sum.cantidadMiedos.miedosInfluenciables) (campaniaDeMarketing influencer1 influenciables)) > ((sum.cantidadMiedos.miedosInfluenciables) (campaniaDeMarketing influencer1 influenciables)) = influencer1
 | otherwise = influencer2

 -- Test
 -- campaniaDeMarketing (cualEsPeor inutil gamer [maria, juanCarlos]) [maria]
 -- Devuelve a maría que le guste el trap (ya que el gamer es peor que el inútil)

-- Parte 3 La influenciación

-- De cada producto se saben dos cosas: 
-- el gusto que se necesita que tenga la persona para comprarlo 
-- y una condición adicional específica de ese producto.

data Producto = UnProducto {
 gusto :: Gusto,
 condicion :: Influenciable->Bool
}

-- El desodorante Acks necesita que a la gente le guste el chocolate pero además que la estabilidad de la persona sea menor a 50.
acks = UnProducto "chocolate" ((<50).estabilidad)

-- El llavero de plato volador necesita que a la persona le gusten los extraterrestres pero que no sea miedosa.
llaveroPlatoV = UnProducto "extraterrestres" (not.esMiedosa)

-- El pollo frito Ca Efe Se necesita que a la persona le guste lo frito y que sea fanática del pollo.
caEfe = UnProducto "frito" (esFanaticaDeUnGusto "pollo")

-- Calcular la eficiencia de un campaña de marketing con un influencer para un producto en particular. 
-- Es el porcentaje de variación de la cantidad de gente que antes de la campaña no hubiera comprado el producto, pero luego sí.
leGusta :: Producto -> Influenciable -> Bool
leGusta producto influenciable = any (gusto producto==) (gustos influenciable)

cumplePerfil :: Producto -> Influenciable -> Bool
cumplePerfil producto influenciable = ((leGusta producto influenciable) && ((condicion producto) influenciable))

cantidadCompras :: Producto -> [Influenciable] -> Number
cantidadCompras producto gente = length(filter (cumplePerfil producto) gente)

-- Otorga el % de diferencia entre el segundo y el primer valor ingresado
variacion :: Number -> Number -> Number
variacion c1 c2 = (c2-c1)*100/c1


eficiencia influencer producto gente = variacion (cantidadCompras producto gente) (cantidadCompras producto (campaniaDeMarketing influencer gente))

-- Test eficiencia
 
pepe = UnInfluenciable ["extraterrestres"] [("arañas",100)] 65
elHermanoDePepe = UnInfluenciable ["Turismo Carretera"] [("volar",60)] 40
laCasaDePepe = [pepe,elHermanoDePepe]

--eficiencia gamer llaveroPlatoV laCasaDePepe devuelve 100 ya que antes compraba sólo pepe y luego su hermano también

-- Analizar qué sucede en caso de utilizar una lista infinita. 
-- Mostrar formas de utilizar algunas de las funciones hechas de manera que:

-- Se quede iterando infinitamente sin arrojar un resultado.
-- Se obtenga una respuesta finita.
-- La respuesta que se obtiene sea a su vez infinita.
-- Explicar la utilidad del concepto de aplicación parcial en la resolución realizada dando un ejemplo concreto donde se lo usó