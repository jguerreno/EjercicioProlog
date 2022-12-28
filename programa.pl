% 1
% canal(Usuario, RedSocial, Seguidores).
canal(ana, youtube, 3000000).
canal(ana, instagram, 2700000).
canal(ana, tiktok, 1000000).
canal(ana, twitch, 2).

canal(beto, youtube, 6000000).
canal(beto, instagram, 1100000).
canal(beto, twitch, 120000).

canal(cami, tiktok, 2000).

canal(dani, youtube, 1000000).

canal(evelyn, instagram, 1).

% 2
% a
% influencer(Usuario).
influencer(Usuario) :-
    usuario(Usuario),
    cantidadTotalDeSeguidores(Usuario, Cantidad),
    Cantidad > 10000.

% usuario(Usuario).
usuario(Usuario) :-
    canal(Usuario,_,_).

% cantidadTotalDeSeguidores(Usuario, CantidadTotalDeSeguidores).
cantidadTotalDeSeguidores(Usuario, CantidadTotalDeSeguidores) :-
    findall(Seguidores, canal(Usuario, _,Seguidores), SeguidoresDeCadaRed),
    sum_list(SeguidoresDeCadaRed, CantidadTotalDeSeguidores).


 % b
% omnipresente(Influencer).
omnipresente(Influencer) :-
    influencer(Influencer),
    forall(red(Red), canal(Influencer, Red, _)).

% red(Red).
red(Red) :-
    canal(_,Red,_).


% c 
% exclusivo(Influencer).
exclusivo(Influencer) :-
    influencer(Influencer),
    not(tieneVariasRedes(Influencer)).

% tieneVariasRedes(Influencer).
tieneVariasRedes(Influencer) :-
    canal(Influencer, UnaRedSocial, _),
    canal(Influencer, OtraRedSocial, _),
    UnaRedSocial\=OtraRedSocial.


% 3
% tipos de contenidos
% video(quienesAparecen, duracion)
% foto(quienesAparecen)
% stream(tematica)

% a
% contenido(Usuario, RedSocial, Contenido).
contenido(ana, tiktok, video([beto,evelyn], 1)).
contenido(ana, tiktok, video([ana], 1)).
contenido(ana, instagram, foto([ana])).

contenido(beto, instagram, foto([])).

contenido(cami, twitch, stream(leagueOfLegends)).
contenido(cami, youtube, video([cami], 5)).

contenido(evelyn, instagram, foto([evelyn,cami])).


% b
% relacionadoConJuegos(Tematica).
relacionadoConJuegos(leagueOfLegends).
relacionadoConJuegos(minecraft).
relacionadoConJuegos(aoe).


% 4
% adictiva(Red).
adictiva(Red) :-
    red(Red),
    forall(contenido(_, Red, Contenido), contenidoAdictivo(Contenido)).

% contenidoAdictivo(Contenido).
contenidoAdictivo(video(_, Duracion)):-
    Duracion < 3.

contenidoAdictivo(stream(Tematica)):-
    relacionadoConJuegos(Tematica).

contenidoAdictivo(foto(Personas)) :-
    length(Personas, Cantidad),
    Cantidad < 4.


% 5
% colaboran(UnUsuario, OtroUsuario).
colaboran(UnUsuario, OtroUsuario) :-
    colaboraCon(UnUsuario, OtroUsuario).
colaboran(UnUsuario, OtroUsuario) :-
    colaboraCon(OtroUsuario, UnUsuario).

% colaboraCon(UnUsuario, OtroUsuario).
colaboraCon(UnUsuario, OtroUsuario) :-
    contenido(UnUsuario, _, Contenido),
    personasPresentes(UnUsuario, Contenido, PersonasPresentes),
    aprece(PersonasPresentes, OtroUsuario).

% personasPresentes(UsuarioDelContenido, Contenido, PersonasPresentes).
personasPresentes(_, video(PersonasPresentes,_), PersonasPresentes).
personasPresentes(_, foto(PersonasPresentes), PersonasPresentes).
personasPresentes(Usuario, stream(_), [Usuario]).

% aprece(PersonasPresentes, Persona).
aprece(PersonasPresentes, Persona) :-
    member(Persona, PersonasPresentes).
    

% 6
% caminoALaFama(Usuario).
caminoALaFama(Usuario) :-
    usuarioNoInfluencer(Usuario),
    influencer(Influencer),
    influencerPublicoContenidoCon(Influencer, Usuario).

% usuarioNoInfluencer(Usuario).
usuarioNoInfluencer(Usuario):-
    usuario(Usuario),
    not(influencer(Usuario)).

% influencerPublicoContenidoCon(Influencer, Usuario).
influencerPublicoContenidoCon(Influencer, Usuario) :-
    colaboraCon(Influencer, Usuario).

influencerPublicoContenidoCon(Influencer, Usuario) :-
    colaboraCon(Influencer, Colaborador),
    Colaborador \= Influencer,
    influencerPublicoContenidoCon(Colaborador, Usuario).


% 7
% a
:- begin_tests(influencer).
test(influencers, set(Influencer == [ana,beto,dani])) :-
	influencer(Influencer).
test(existe_algun_influencer, nondet) :-
	influencer(_).
:- end_tests(influencer).


% b
/*
Para modelar que beto no tiene tiktok no se tuvo que hacer nada.
Ya que, por universo cerrado, al no poner que beto tiene esa red
social (es decir se desconoce) se toma por falso. Ya que todo 
lo que no esta en mi base de conocimientos se toma como falso.
*/