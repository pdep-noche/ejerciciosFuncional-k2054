humano(platon).
humano(socrates).
mortal(Persona):-humano(Persona).


hablaIdioma(juan, espaniol).
hablaIdioma(juan, ingles).
hablaIdioma(juan, italiano).
hablaIdioma(marcela, espaniol).
hablaIdioma(hernan , aleman).

seComunican(Persona,OtraPersona):- hablaIdioma(Persona,Idioma), hablaIdioma(OtraPersona,Idioma), 
                  Persona \= OtraPersona.


viveEn(nora , almagro).
viveEn(luis ,caballito).
viveEn(ana ,lugano).
estaEn(lugano,campus).
estaEn(almagro, medrano).
viajaEnAuto(nora).
viajaEnAuto(matias).

llegaRapido(Persona,Lugar):- viveEn(Persona, Barrio),  estaEn(Barrio, Lugar).
llegaRapido(Persona,Lugar):- viajaEnAuto(Persona), estaEn(_, Lugar).



curso(julia,fisicaI).
curso(emilio , inglesII).
curso(elizabeth , quimica).
curso(pedro,economia).

aprobo(emilio , inglesII).
aprobo(elizabeth , quimica).

desaprobo(Persona,Materia):-  curso(Persona, Materia), not(aprobo(Persona,Materia)).


programaEn(nahuel, javascript).
programaEn(juan, haskell).
programaEn(juan, ruby).
programaEn(caro, haskell).
programaEn(caro, scala).
programaEn(caro, javascript).
programaEn(_, c).

programaEn(Persona, c):-persona(Persona).

persona(nahuel).
persona(juan).
persona(caro).


siguiente(Numero, Siguiente):- numero(Numero), Siguiente is Numero + 1.

numero(Numero):- between(1,10,Numero).

mayor(Mayor, Menor):- numero(Mayor), numero(Menor), Mayor > Menor.


irremplazable(Persona):- programaEn(Persona, Lenguaje), 
    not((programaEn(Alguien, Lenguaje), Alguien \= Persona)).


quiere(juan , playa).
quiere(juan , wifi).
quiere(juan , teatro).
quiere(ana , playa).
quiere(ana ,sierra).


lugar(mardel , playa).
lugar(mardel , wifi).
lugar(mardel , teatro).
lugar(mardel ,casino).
lugar(tandil ,sierra).
lugar(tandil , teatro).


tieneTodoParaVeranear(Lugar, Persona):- lugar(Lugar, _), quiere(Persona, _),
    forall(quiere(Persona, Algo), lugar(Lugar, Algo)).


contador(roque).
joven(roque).
trabajoEn(jose, acme).
trabajoEn(roque,acme).
trabajoEn(ana,omni).
trabajoEn(lucia,omni).
trabajoEn(alicia, fiat).
honesto(roque).
ingeniero(ana).
ingeniero(alicia).
habla(roque,frances).
habla(ana,ingles).
habla(ana,frances).
habla(lucia,ingles).
habla(lucia,frances).
habla(cecilia,frances).
abogado(cecilia).
ambicioso(cecilia).
ambicioso(jose).
ambicioso(Persona):-contador(Persona),joven(Persona).
tieneExperiencia(Persona):-trabajoEn(Persona, _).
profesional(Persona):-abogado(Persona).
profesional(Persona):-contador(Persona).
profesional(Persona):-ingeniero(Persona).


puedeAndar(comercioExterior, Persona):-ambicioso(Persona).
puedeAndar(contaduria, Persona):-honesto(Persona),contador(Persona).
puedeAndar(ventas, Persona):-ambicioso(Persona), tieneExperiencia(Persona).
puedeAndar(ventas, lucia).
puedeAndar(proyectos, Persona):-ingeniero(Persona), tieneExperiencia(Persona).
puedeAndar(proyectos, Persona):-abogado(Persona), joven(Persona).
puedeAndar(logistica, Persona):-profesional(Persona), cumpleCondiciones(Persona).

cumpleCondiciones(Persona):-joven(Persona).
cumpleCondiciones(Persona):-trabajoEn(Persona, omni).




madre(mona, homero).
madre(jaqueline, marge).
madre(marge, maggie).
madre(marge, bart).
madre(marge, lisa).
padre(abraham, herbert).
padre(abraham, homero).
padre(clancy, jaqueline).
padre(homero, maggie).
padre(homero, bart).
padre(homero, lisa).


hermano(Persona, OtraPersona):-mismaMadre(Persona, OtraPersona), mismoPadre(Persona, OtraPersona).

mismaMadre(Persona, OtraPersona):-madre(Madre, Persona), madre(Madre, OtraPersona), Persona \= OtraPersona.
mismoPadre(Persona, OtraPersona):- padre(Padre, Persona), padre(Padre, OtraPersona), Persona \= OtraPersona.

medioHermano(Persona, OtraPersona):- mismaMadre(Persona, OtraPersona), not(mismoPadre(Persona, OtraPersona)).
medioHermano(Persona, OtraPersona):- mismoPadre(Persona, OtraPersona), not(mismaMadre(Persona, OtraPersona)).

hijoUnico(Persona):-hijo(Persona,_), not(hermano(Persona, _)).

hijo(Persona, Progenitor):-madre(Progenitor, Persona).
hijo(Persona, Progenitor):-padre(Progenitor, Persona).