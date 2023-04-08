%first line stations
line1(new_elmarg).
line1(elmarg).
line1(ezbet_elnakhl).
line1(ain_shams).
line1(elmatareyya).
line1(helmeyet_elzaitoun).
line1(hadayeq_elzaitoun).
line1(saray_elqobba).
line1(hammamat_elqobba).
line1(kobri_elqobba).
line1(manshiet_elsadr).
line1(eldemerdash).
line1(ghamra).
line1(alshohadaa).
line1(urabi).
line1(nasser).
line1(sadat).
line1(saad_zaghloul).
line1(alsayyeda_zeinab).
line1(elmalek_elsaleh).
line1(margirgis).
line1(elzahraa).
line1(dar_elsalam).
line1(hadayeq_elmaadi).
line1(maadi).
line1(thakanat_elmaadi).
line1(tora_elbalad).
line1(kozzika).
line1(tora_elasmant).
line1(elmaasara).
line1(hadayeq_helwan).
line1(wadi_hof).
line1(helwan_university).
line1(ain_helwan).
line1(helwan).

%second line stations
line2(shobra_elkheima).
line2(koliet_elzeraa).
line2(mezallat).
line2(khalafawy).
line2(sainte_teresa).
line2(road_elfarag).
line2(massara).
line2(alshohadaa).
line2(ataba).
line2(naguib).
line2(sadat).
line2(opera).
line2(dokki).
line2(bohooth).
line2(cairo_university).
line2(faisal).
line2(giza).
line2(omm_elmisryeen).
line2(sakiat_mekki).
line2(elmounib).

%first line connection
connected(new_elmarg,elmarg).
connected(elmarg,ezbet_elnakhl).
connected(ezbet_elnakhl,ain_shams).
connected(ain_shams,elmatareyya).
connected(elmatareyya,helmeyet_elzaitoun).
connected(helmeyet_elzaitoun,hadayeq_elzaitoun).
connected(hadayeq_elzaitoun,saray_elqobba).
connected(saray_elqobba,hammamat_elqobba).
connected(hammamat_elqobba,kobri_elqobba).
connected(kobri_elqobba,manshiet_elsadr).
connected(manshiet_elsadr,eldemerdash).
connected(eldemerdash,ghamra).
connected(ghamra,alshohadaa).
connected(alshohadaa,urabi).
connected(urabi,nasser).
connected(nasser,sadat).
connected(sadat,saad_zaghloul).
connected(saad_zaghloul, alsayyeda_zeinab).
connected(alsayyeda_zeinab,elmalek_elsaleh).
connected(elmalek_elsaleh,margirgis).
connected(margirgis,elzahraa).
connected(elzahraa,dar_elsalam).
connected(dar_elsalam,hadayeq_elmaadi).
connected(hadayeq_elmaadi,maadi).
connected(maadi,thakanat_elmaadi).
connected(thakanat_elmaadi,tora_elbalad).
connected(tora_elbalad,kozzika).
connected(kozzika,tora_elasmant).
connected(tora_elasmant,elmaasara).
connected(elmaasara,hadayeq_helwan).
connected(hadayeq_helwan,wadi_hof).
connected(wadi_hof,helwan_university).
connected(helwan_university,ain_helwan).
connected(ain_helwan,helwan).

%second line connection
connected(shobra_elkheima,koliet_elzeraa).
connected(koliet_elzeraa,mezallat).
connected(mezallat,khalafawy).
connected(khalafawy,sainte_teresa).
connected(sainte_teresa,road_elfarag).
connected(road_elfarag,massara).
connected(massara,alshohadaa).
connected(alshohadaa,ataba).
connected(ataba,naguib).
connected(naguib,sadat).
connected(sadat,opera).
connected(opera,dokki).
connected(dokki,bohooth).
connected(bohooth,cairo_university).
connected(cairo_university,faisal).
connected(faisal,giza).
connected(giza,omm_elmisryeen).
connected(omm_elmisryeen,sakiat_mekki).
connected(sakiat_mekki,elmounib).

%task1
%________


check(S,D):-
    (
      (S==sadat,line2(D))-> line2(S) ;
      (S==sadat,line1(D))-> line1(S) ;
      (S==alshohadaa,line2(D))-> line2(S) ;
      (S==alshohadaa,line1(D))-> line1(S)
      /*(line1(D), S\=sadat , s\=alshohadaa )-> line1(S) , write("5");
      (line2(D) , not(S=sadat) ,not(s=alshohadaa))-> line2(S) ,  write("6")*/
    ).

/*my_reverse(L,R) :- rev(L,[],R).

rev([],A,A).
rev([H|T],A,R) :-
    ( is_list(H) ->
      rev(H,[],X),
      rev(T,[X|A],R);
      rev(T,[H|A],R)
    ).
*/

path(Source, Destination, any, [[Source, Destination]]) :-
    connected(Source, Destination).
   % write("xx").

path(Source, Destination, any, [[Source, H]|T]):-
   connected(Source, H),
    path(H, Destination, any, T),!.

path(Destination, Source, any, [[Source, H]|T]):-
   connected(Source, H),

  %  check(Source ,Destination),
  check(Destination,Source),
     write(Source),
   % write(H),
    write(Destination),
   % check(Destination,Source),
    path(H, Destination, any, T),!.

path(Source, Destination, N, [[Source,Destination]]) :-
  N\=any, N>0, connected(Source, Destination).

path(Source, Destination, N, [[Source,H]|T]):-
  N\=any, N>0, connected(Source, H),
  NN is N-1, path(H, Destination, NN, T),!.

path(Destination, Source, N, [[Source,H]|T]):-
  N\=any, N>0, connected(Source, H),check(Destination,Source),
  NN is N-1, path(H, Destination, NN, T),!.

%______________________________________________________

%Task2

neighbors(Station, StationList) :-
    neighbors(Station, [], StationList).

neighbors(Station, L, StationList) :-
    connected(Neighbor,Station),
    \+ member(Neighbor, L), !,
	neighbors(Station, [Neighbor|L], StationList);
    connected(Station,Neighbor),
    \+ member(Neighbor, L), !,
	neighbors(Station, [Neighbor|L], StationList).

neighbors(_, L, L).
listCount([], 0).
listCount([_|Tail], Count) :-
    listCount(Tail, TailCount),
    Count is TailCount + 1.

nstations(Station, Count) :-
    neighbors(Station, StationList),
    listCount(StationList, Count).
%______________________________________________________


/*cost(Source,Destination,Cost):-
    (
      (line1(Source),line1(Destination),
       path(Source, Destination,any,L),
       listCount(L,N), N@=<7)->

           Cost = '3 EGP ';

      (line2(Source),line2(Destination),
       % path(Source, Destination,any,L),
       listCount(_,N), N@=<7 , (Source == sadat))->
     Cost = '3 EGP ';

     (path(Source, Destination,any,L),
      listCount(L,N), N<16,N>7)->

           Cost = '5 EGP ';

    (line1(Source),line2(Destination))->

           Cost = '5 EGP';

    (line2(Source),line1(Destination))->
     write(N),
           Cost = '5 EGP ';

    (path(Source, Destination,any,L),
     listCount(L,N), N@>=16)->
     write(N),

           Cost = '7 EGP'
    ).*/

%Task3

cost(Source,Destination,Cost):-
    (
      ((line1(Source),line1(Destination),
       path(Source, Destination,any,L),
       listCount(L,N), N@=<7);

      (line2(Source),line2(Destination),
      (Source == alshohadaa),
       path(Destination,Source,any,L),
     listCount(L,N), N@=<7);

      (line2(Source),line2(Destination),
      (Source == sadat),
       path(Destination,Source,any,L),
        listCount(L,N), N@=<7);

    (line2(Source),line2(Destination),
      (Source \= alshohadaa) , (Source \=sadat),
       path(Source, Destination,any,L),
       listCount(L,N), N@=<7)) ->
         % write(N),
          Cost = '3 EGP ';

    ((path(Source, Destination,any,L),
      listCount(L,N), N<16,N>7);
    (line1(Source),line2(Destination));
    (line2(Source),line1(Destination))) ->
         %write(N),
           Cost = '5 EGP';

    (path(Source, Destination,any,L),
     listCount(L,N), N@>=16)->

           Cost = '7 EGP'
    ).


%______________________________________________________


%Task4

checkline([[P1,P2],[P3,P4]]):-
[[P1,P2],[P3,P4]]=[R1,R2],
R1=[H1,T1],
R2=[H2,T2],
   (
 ( line1(H1),line1(T1),line1(H2),line1(T2) )
;
%OR
(line2(H1),line2(T1),line2(H2),line2(T2))
),!.

checkconnected([[ _,sadat],[_,_]]):- !.
checkconnected([[_,alshohadaa],[_,_]]):- !.
checkconnected([[,],[sadat,_]]):- !.
checkconnected([[,],[alshohadaa,_]]):- !.

checkconnected([[P1,P2],[P3,P4]]):-
[[P1,P2],[P3,P4]]=[R1,R2],
R1=[S1,S2],
R2=[S3,S4],
 connected(S1,S2),  S1\==S2,
connected(S2,S3), S2\==S3,
connected(S3,S4), S3\==S4.

checkPath([[],[]]):- fail.
checkPath([[S1,S2],[S3,S4]]):-
checkline([[S1,S2],[S3,S4]]) ;
checkconnected([[S1,S2],[S3,S4]]).

%______________________

%checkPath([[helmeyet_elzaitoun,hadayeq_elzaitoun],[hadayeq_elzaitoun,saray_elqobba]]).
%checkPath([[hadayeq_elmaadi,maadi],[omm_elmisryeen,sakiat_mekki]]).
%checkPath([[ghamra,alshohadaa],[opera,dokki]]).
%checkPath([[sadat,saad_zaghloul],[saad_zaghloul,alsayyeda_zeinab]]).
%checkPath([[sadat,saad_zaghloul],[opera,dokki]]).
