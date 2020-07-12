/*
 *
 * Eingabe ist:
 * 1. Weißer König
 * 2. Weißer Turm
 * 3. Schwarzer König
 *
 * Eingabeform ist in einer 2-elementigen Liste bestehend aus zwei
 * Ziffern,
 * die erste beschreibt die X-Koordinate,
 * die Zweite die Y-Koordinate.
 * Beispiel: KTKEndspiel([1,1],[1,8],[4,4])
 *
 * WICHTIG: Eingabe muss zwischen 1 und 8 sein
 *
 *
 *
 * Vorgehen:
 * 1. Orientierung so drehen, dass weißer König unten und schwarzer
 * König oben ist
 * 2a. Turm in Y3-1.ten Linie bewegen.
 * 2b. SK Richtung Turm
 * 3a. WK Richtung Springer-Position zu König, verfolgt SK
 * 3b. SK Richtung Turm
 * 3Fall1. SK kann WT einnehmen: ST Richtung anderes Ende mit Abstand
 * 3Fall2. WK geht in Opposition: Warteschritt mit WT anstelle
 * 3Fall3. SK geht in opposition: in andere Richtung wenn
 *                                möglich, ansonsten Opposition
 * 3Fall4. SK in Opposition:
 *                           1. nicht am oberen Rand
 *                           a. WT geht auf SK Ebene
 *                           b. SK in Richtung Turm mit Y+1
 *                           c. wiederhole 3.
 *                           2. am oberen Rand
 *                           a. WT geht auf SK Ebene
 *                              -> Schachmatt
 *
 *
 *
 *
 */
 %dynamischer Fakt, wird genutzt, um zu merken, wie sehr das Brett gedreht wurde. Dies wird bei der Ausgabe genutzt, um die Position der Steine zurückzudrehen
:-dynamic(gedreht/1).
gedreht(0).

 %Gleichheitsprädikat
gleich(X,X).
gleicherplatz([A,B],[C,D]):- gleich(A,C),gleich(B,D).
 %A ist gegenüber B, wenn B zwei Felder über A ist. (eine Lücke zwischen A und B)
gegenueber(X,Y):-Y is X + 2.
 %Opposition in Schach ist, wenn die Könige sich gegenüber stehen und somit die drei Felder zwischeneinander blocken. hier: Gleiche X-Koordinaten, stehen gegenüber.
opposition([X1,Y1],[X2,Y2]):-gleich(X1,X2),gegenueber(Y1,Y2).
 %prüft, ob die Könige direkt nebeneinanderstehen. Schauen WK an und gucken in seine 8 umliegenden Felder
imkoenigfeld([XWK,YWK],[XSK,YSK]):-
    XWK =:= XSK,YSK is YWK +1,!;   %oben
    XSK is XWK+1, YSK is YWK +1,!; %oben rechts
    XSK is XWK+1, YWK =:= YSK,!;   %rechts
    XSK is XWK+1, YSK is YWK-1,!;  %unten rechts
    XWK =:= XSK, YSK is YWK +1,!;  %unten
    XSK is XWK-1, YSK is YWK -1,!; %unten links
    XSK is XWK-1, YWK =:= YSK,!;   %links
    XSK is XWK-1, YSK is YWK+1,!.  %oben links


 %prüft, ob der weiße turm schräg neben könig ist
wtnebensk([XWT,YWT],[XSK,YSK]):-YSK is YWT+1,XSK is XWT-1; YSK is YWT+1,XSK is XWT+1.

 % Prüft, ob der WK in Springerposition zum SK ist. Wenn der Turm links
 % vom SK ist, dann ist die SP rechts vom SK und andersrum
wkspringerpositionsk([XWK,YWK],[XWT],[XSK,YSK]):-
    Y is YSK-2,X is XSK-1,XWK is X, YWK is Y, XSK=<XWT;
    Y is YSK-2,X is XSK+1,XWK is X, YWK is Y, XSK>=XWT.% YSK is YWK+2,XSK is XWK+1, XSK>=XWT.

 %prüft, obdie zwei Figuren genau übereinander sind
aunterb(Y1,Y2):- Y2 is Y1+1.
istwkuebersk([B],[D]):-B>=D.

ktk([A,B],[C,D],[E,F]):- retractall(gedreht(_)),assertz(gedreht(0)), ktkanfang([A,B],[C,D],[E,F]).

ktkanfang([A,B],[C,D],[E,F]):-
    imkoenigfeld([B],[F]),write('Fehlerhafte Eingabe, die Könige befinden sich unmittelbar in deren Nähe'),!;
    A is C, B is D,write('Fehlerhafte Eingabe, w Turm und w König haben den gleichen Platz'),!;
    E is C, F is D,write('Fehlerhafte Eingabe, w Turm und s König haben den gleichen Platz'),!;
    A is E, B is F,write('Fehlerhafte Eingabe, w König und s König haben den gleichen Platz'),!;

    B is 1, F is 2, A<E, drehe270([A,B],[H,I]),drehe270([C,D],[M,N]),drehe270([E,F],[O,P]),retractall(gedreht(_)), assertz(gedreht(3)), setupw([H,I],[M,N],[O,P]),!;
    B is 1, F is 2, A>E, drehe90([A,B],[H,I]),drehe90([C,D],[M,N]),drehe90([E,F],[O,P]),retractall(gedreht(_)), assertz(gedreht(3)), setupw([H,I],[M,N],[O,P]),!;

    istwkuebersk([B],[F]),A<E,drehe270([A,B],[H,I]),drehe270([C,D],[M,N]),drehe270([E,F],[O,P]),retractall(gedreht(_)), assertz(gedreht(3)), setupw([H,I],[M,N],[O,P]),!;
    istwkuebersk([B],[F]),A=:=E,drehe180([A,B],[H,I]),drehe180([C,D],[M,N]),drehe180([E,F],[O,P]),retractall(gedreht(_)), assertz(gedreht(2)), setupw([H,I],[M,N],[O,P]),!;
    istwkuebersk([B],[F]),A>E,drehe90([A,B],[H,I]),drehe90([C,D],[M,N]),drehe90([E,F],[O,P]),retractall(gedreht(_)), assertz(gedreht(1)), setupw([H,I],[M,N],[O,P]),!;

    setupw([A,B],[C,D],[E,F]).


setupw([A,B],[C,D],[E,F]):-

    %Wenn bereits in perfekter Lage, dann gehe in Hauptfunktion
    C=:=1,aunterb(D,F),not(imkoenigfeld([C,D],[E,F])),ktkw([A,B],[C,D],[E,F]),!;
    C=:=8,aunterb(D,F),not(imkoenigfeld([C,D],[E,F])),ktkw([A,B],[C,D],[E,F]),!;

    %Wenn Turm und König auf gleicher X-Spalte, dann bewege den turm zur entsprechend anderen Seite.
    C is E,B=\=D,E>=5 , X is 1,ausgabe('Weiss','Turm',[X,D]), setups([A,B],[X,D],[E,F]),!;
    C is E,B=\=D,E<5 , X is 8,ausgabe('Weiss','Turm',[X,D]), setups([A,B],[X,D],[E,F]),!;

    %Wenn Turm mindestens 2 Zeilen unterhalb SK, WK und WT in einer Reihe direkt nebeneinander, dann bewege den Turm um ein nach oben
    B=:=D,C=\=E,A is C-1,X is F-D,X>=2,Y is D+1,ausgabe('Weiss','Turm',[C,Y]),setups([A,B],[C,Y],[E,F]),!;
    B=:=D,C=\=E,A is C+1,X is F-D,X>=2,Y is D+1,ausgabe('Weiss','Turm',[C,Y]),setups([A,B],[C,Y],[E,F]),!;


    %Wenn König und Turm in einer Spalte, aber WK und WT in einer Reihe, dann bewege den Turm neben den König
    C is E,B=:=D,A<C,X is A+1,ausgabe('Weiss','Turm',[X,D]),setups([A,B],[X,D],[E,F]),!;
    C is E,B=:=D,A>C,X is A-1,ausgabe('Weiss','Turm',[X,D]),setups([A,B],[X,D],[E,F]),!;

    D=:=F,  Y is F-1,not(imkoenigfeld([C,Y],[E,F])),gleicherplatz([A,B],[C,Y]), Z is F+1,ausgabe('Weiss','Turm',[C,Z]), setups([A,B],[C,Z],[E,F]),!;
    D=:=F,  Y is F-1,not(imkoenigfeld([C,Y],[E,F])),not(gleicherplatz([A,B],[C,Y])),ausgabe('Weiss','Turm',[C,Y]), ktks([A,B],[C,Y],[E,F]),!;

    B>D,gleich(A,C),F>B,E>=5 , X is 1,gleich(A,X), Z is 2,ausgabe('Weiss','Turm',[Z,D]),setups([A,B],[Z,D],[E,F]),!;
    B>D,gleich(A,C),F>B,E<5 , X is 8,gleich(A,X), Z is 7,ausgabe('Weiss','Turm',[Z,D]),setups([A,B],[Z,D],[E,F]),!;

    Y is F-1,not(imkoenigfeld([C,Y],[E,F])),ausgabe('Weiss','Turm',[C,Y]), ktkw([A,B],[C,Y],[E,F]),!;

    M is F-1,imkoenigfeld([C,M],[E,F]),Y is F-2,ausgabe('Weiss','Turm',[C,Y]),  setups([A,B],[C,Y],[E,F]),!.

setups([A,B],[C,D],[E,F]):-
    C<E, not(C is E-1),X is E-1,D<F, Y is F-1,Y is D+1,ausgabe('Schwarz','König',[X,Y]), ktkw([A,B],[C,D],[X,Y]),!;
    C<E, not(C is E-1),X is E-1,D>F, Y is F+1,Y is D+1,ausgabe('Schwarz','König',[X,Y]), ktkw([A,B],[C,D],[X,Y]),!;

    C<E, not(C is E-1),X is E-1,D<F, Y is F-1,Y =\= D+1,ausgabe('Schwarz','König',[X,Y]), setupw([A,B],[C,D],[X,Y]),!;
    C<E, not(C is E-1),X is E-1,D>F, Y is F+1,Y =\= D+1,ausgabe('Schwarz','König',[X,Y]), setupw([A,B],[C,D],[X,Y]),!;

    C<E, C is E-1,D<F, Y is F-1,Y =\= D+1,ausgabe('Schwarz','König',[E,Y]), setupw([A,B],[C,D],[E,Y]),!;
    C<E, C is E-1,D>F, Y is F+1,Y =\= D+1,ausgabe('Schwarz','König',[E,Y]), setupw([A,B],[C,D],[E,Y]),!;

    C<E, C is E-1,D<F, Y is F-1,Y is D+1,ausgabe('Schwarz','König',[E,Y]), ktkw([A,B],[C,D],[E,Y]),!;
    C<E, C is E-1,D>F, Y is F+1,Y is D+1,ausgabe('Schwarz','König',[E,Y]), ktkw([A,B],[C,D],[E,Y]),!;


    C>E, not(C is E+1),X is E+1,D<F, Y is F-1,Y is D+1,ausgabe('Schwarz','König',[X,Y]), ktkw([A,B],[C,D],[X,Y]),!;
    C>E, not(C is E+1),X is E+1,D>F, Y is F+1,Y is D+1,ausgabe('Schwarz','König',[X,Y]), ktkw([A,B],[C,D],[X,Y]),!;

    C>E, not(C is E+1),X is E+1,D<F, Y is F-1,Y =\= D+1,ausgabe('Schwarz','König',[X,Y]), setupw([A,B],[C,D],[X,Y]),!;
    C>E, not(C is E+1),X is E+1,D>F, Y is F+1,Y =\= D+1,ausgabe('Schwarz','König',[X,Y]), setupw([A,B],[C,D],[X,Y]),!;

    C>E, C is E+1,D<F, Y is F-1,Y is D+1,ausgabe('Schwarz','König',[E,Y]), ktkw([A,B],[C,D],[E,Y]),!;
    C>E, C is E+1,D>F, Y is F+1,Y is D+1,ausgabe('Schwarz','König',[E,Y]), ktkw([A,B],[C,D],[E,Y]),!;

    C>E, C is E+1,D<F, Y is F-1,Y =\= D+1,setupw([A,B],[C,D],[E,Y]),!;
    C>E, C is E+1,D>F, Y is F+1,Y =\= D+1,setupw([A,B],[C,D],[E,Y]),!.


% gedreht(X),format('WK ~2f ~2f, WT ~2f ~2f, SK ~2f ~2f, gedreht:
% ~2f',[A,B,C,D,E,F,X]),nl.
drehe90([A,B],[X,Y]):-
    A<5,B<5,M is 5-A, N is 5-B, X is 5-N, Y is 4+M;
    A<5,B>=5,M is 5-A, N is B-4, X is 4+N, Y is 4+M;
    A>=5,B>=5,M is A-4,N is B-4,X is 4+N, Y is 5-M; %M= Abstand X zur Mitte, N=Abstand Y zur Mitte
    A>=5,B<5,M is A-4, N is 5-B, X is 5-N, Y is 5-M.

drehe180([A,B],[X,Y]):- X is 9-A,Y is 9-B.
drehe270([A,B],[X,Y]):-
    A<5,B<5,M is 5-A, N is 5-B, X is 4+N, Y is 5-M;
    A<5,B>=5,M is 5-A, N is B-4, X is 5-N, Y is 5-M;
    A>=5,B>=5,M is A-4,N is B-4,X is 5-N, Y is 4+M; %M= Abstand X zur Mitte, N=Abstand Y zur Mitte
    A>=5,B<5,M is A-4, N is 5-B, X is 4+N, Y is 4+M.

/*
drehe(X,Y,Z,[A,B],[C,D],[E,F]):-
     kleinste(X,Y,Z,K),K is X,drehe180([A,B],[H,I]),drehe180([C,D],[M,N]),drehe180([E,F],[O,P]), assertz(gedreht(2)), setup([H,I],[M,N],[O,P]);
*/

ktkw([2,6],[8,8],[2,8]).
ktkw([7,6],[1,8],[7,8]).
ktkw([2,6],[7,8],[2,8]).
ktkw([7,6],[2,8],[7,8]).

ktkw([A,B],[C,D],[E,F]):-

    %wenn der SK den WT angreift, dann wird der WT an das andere Ende des Brettes gespielt. Wichtig: mit Abstand 1, um später einen leichten Wartezug zu erlauben.
    wtnebensk([C,D],[E,F]),C<E,M is 7,ausgabe('Weiss','Turm',[M,D]),ktks([A,B],[M,D],[E,F]),!;
    wtnebensk([C,D],[E,F]),C>E,M is 2,ausgabe('Weiss','Turm',[M,D]),ktks([A,B],[M,D],[E,F]),!;

    %Wenn beide Könige in Opposition stehen, muss der Turm um ein erhöht werden um mit Schachmatt zu drohen
    %Opposition,            Turm noch unterhalb SK, YWT um eins erhöhen, Ausgabe
    opposition([A,B],[E,F]),aunterb(D,F),G is D+1,ausgabe('Weiss','Turm(Schach)',[C,G]),ktks([A,B],[C,G],[E,F]),!;

    %Sonderfall, es wird ein Wartezug benötigt, somit wird der Turm um ein Feld bewegt
    not(wtnebensk([C,D],[E,F])), not(wkspringerpositionsk([A,B],[C],[E,F])),wkspringerpositionsk([X,Y],[C],[E,F]),A is X-2 ,B=:=Y  ,A<E,C<E, C is 2, M is 1,ausgabe('Weiss','Turm',[M,D]),ktks([A,B],[M,D],[E,F]),!;
    not(wtnebensk([C,D],[E,F])), not(wkspringerpositionsk([A,B],[C],[E,F])),wkspringerpositionsk([X,Y],[C],[E,F]),A is X+2 ,B=:=Y  ,C is 7, M is 8,ausgabe('Weiss','Turm',[M,D]),ktks([A,B],[M,D],[E,F]),!;

    %wenn der SK den WT nicht angreift & WK nicht in Springerposition ist , dann hol diese , vergleiche die aktuelle Position mit dieser und bewege WK je nach dem. und die resultierende Könige sind nicht im Königfeld
    not(wtnebensk([C,D],[E,F])), not(wkspringerpositionsk([A,B],[C],[E,F])),wkspringerpositionsk([X,Y],[C],[E,F]) ,A<X ,B<Y , M is A+1,N is B+1,ausgabe('Weiss','König',[M,N]),ktks([M,N],[C,D],[E,F]),!;
    not(wtnebensk([C,D],[E,F])), not(wkspringerpositionsk([A,B],[C],[E,F])),wkspringerpositionsk([X,Y],[C],[E,F]) ,A=:=X ,B<Y ,N is B+1,ausgabe('Weiss','König',[A,N]),ktks([A,N],[C,D],[E,F]),!;
    not(wtnebensk([C,D],[E,F])), not(wkspringerpositionsk([A,B],[C],[E,F])),wkspringerpositionsk([X,Y],[C],[E,F]) ,A>X ,B<Y , M is A-1,N is B+1,ausgabe('Weiss','König',[M,N]),ktks([M,N],[C,D],[E,F]),!;

    not(wtnebensk([C,D],[E,F])), not(wkspringerpositionsk([A,B],[C],[E,F])),wkspringerpositionsk([X,Y],[C],[E,F]) ,A<X ,B=:=Y , M is A+1,ausgabe('Weiss','König',[M,B]),ktks([M,B],[C,D],[E,F]),!;
    not(wtnebensk([C,D],[E,F])), not(wkspringerpositionsk([A,B],[C],[E,F])),wkspringerpositionsk([X,Y],[C],[E,F]) ,A>X ,B=:=Y , M is A-1,ausgabe('Weiss','König',[M,B]),ktks([M,B],[C,D],[E,F]),!;

    not(wtnebensk([C,D],[E,F])), not(wkspringerpositionsk([A,B],[C],[E,F])),wkspringerpositionsk([X,Y],[C],[E,F]) ,A<X ,B>Y , M is A+1,N is B-1,not(imkoenigfeld([M,N],[E,F])),ausgabe('Weiss','König',[M,N]),ktks([M,N],[C,D],[E,F]),!;
    not(wtnebensk([C,D],[E,F])), not(wkspringerpositionsk([A,B],[C],[E,F])),wkspringerpositionsk([X,Y],[C],[E,F]) ,A>X ,B>Y, M is A-1,N is B-1,not(imkoenigfeld([M,N],[E,F])),ausgabe('Weiss','König',[M,N]),ktks([M,N],[C,D],[E,F]),!;

    %Sonderfall: Springerposition bevor SK Angriff auf WT -> WK muss ich um ein Feld bewegen
    not(wtnebensk([C,D],[E,F])), wkspringerpositionsk([A,B],[C],[E,F]),C>E,A<E,M is A-1,ausgabe('Weiss','König',[M,B]),ktks([M,B],[C,D],[E,F]),!;
    not(wtnebensk([C,D],[E,F])), wkspringerpositionsk([A,B],[C],[E,F]),C<E,A>E,M is A+1,ausgabe('Weiss','König',[M,B]),ktks([M,B],[C,D],[E,F]),!.


ktks([2,6],[8,8],[2,8]).
ktks([7,6],[1,8],[7,8]).
ktks([2,6],[7,8],[2,8]).
ktks([7,6],[2,8],[7,8]).

ktks([A,B],[C,D],[E,F]):-

    %Wenn in Opposition und Turm auf gleicher Ebene, dann Schach ausweichen durch nach obene gehen
    opposition([A,B],[E,F]),D=:=F,C>E,M is E+1,N is F+1,ausgabe('Schwarz','König',[M,N]),ktkw([A,B],[C,D],[M,N]),!;
    opposition([A,B],[E,F]),D=:=F,C<E,M is E-1,N is F+1,ausgabe('Schwarz','König',[M,N]),ktkw([A,B],[C,D],[M,N]),!;

    %Wenn in Springerposition, dann hängt die Bewegung von den Positionen der weißen Figuren ab, sowie, ob der SK am Rand ist
    %Springerposition,                      Turm rechts, WK links, bewege SK nach rechts Richtung WT
    wkspringerpositionsk([A,B],[C],[E,F]),C>E,A<E,M is E+1,ausgabe('Schwarz','König',[M,F]),ktkw([A,B],[C,D],[M,F]),!;
    wkspringerpositionsk([A,B],[C],[E,F]),C<E,A>E,M is E-1,ausgabe('Schwarz','König',[M,F]),ktkw([A,B],[C,D],[M,F]),!;

    %nicht am Rand, weglaufen von der Opposition -> vom WK weglaufen
    not(wkspringerpositionsk([A,B],[C],[E,F])),wkspringerpositionsk([X,Y],[C],[E,F]),A is X-2 ,B=:=Y,C<E,A<E,E<8,M is E+1,ausgabe('Schwarz','König',[M,F]),ktkw([A,B],[C,D],[M,F]),!;
    not(wkspringerpositionsk([A,B],[C],[E,F])),wkspringerpositionsk([X,Y],[C],[E,F]),A is X+2 ,B=:=Y,C>E,A>E,E>1,M is E-1,ausgabe('Schwarz','König',[M,F]),ktkw([A,B],[C,D],[M,F]),!;

    %am Rand, nur eine Möglichkeit: in die Opposition gehen
    not(wkspringerpositionsk([A,B],[C],[E,F])),wkspringerpositionsk([X,Y],[C],[E,F]),A is X-2 ,B=:=Y,C<E,A<E,E=:=8,M is E-1,ausgabe('Schwarz','König',[M,F]),ktkw([A,B],[C,D],[M,F]),!;
    not(wkspringerpositionsk([A,B],[C],[E,F])),wkspringerpositionsk([X,Y],[C],[E,F]),A is X+2 ,B=:=Y,C>E,A>E,E=:=1,M is E+1,ausgabe('Schwarz','König',[M,F]),ktkw([A,B],[C,D],[M,F]),!;



    %Wenn die Könige nicht in Springerposition sind, dann bewege den SK Richtung Turm zum angreifen
    not(wkspringerpositionsk([A,B],[C],[E,F])),C>E,M is E+1,imkoenigfeld([A,B],[M,F]),N is E-1,ausgabe('Schwarz','König',[N,F]),ktkw([A,B],[C,D],[N,F]),!;
    not(wkspringerpositionsk([A,B],[C],[E,F])),C<E,M is E-1,imkoenigfeld([A,B],[M,F]),N is E+1,ausgabe('Schwarz','König',[N,F]),ktkw([A,B],[C,D],[N,F]),!;


    not(wkspringerpositionsk([A,B],[C],[E,F])),C>E,M is E+1,ausgabe('Schwarz','König',[M,F]),ktkw([A,B],[C,D],[M,F]),!;
    not(wkspringerpositionsk([A,B],[C],[E,F])),C<E,M is E-1,ausgabe('Schwarz','König',[M,F]),ktkw([A,B],[C,D],[M,F]),!.


ausgabe(S1,S2,[A,B]):-
    gedreht(G), G is 0, format('~s, ~s:~t ~1f, ~1f',[S1,S2,A,B]),nl,!;
    gedreht(G), G is 1,drehe270([A,B],[X,Y]), format('~s ~s:~t ~1f, ~1f',[S1,S2,X,Y]),nl,!;
    gedreht(G), G is 2,drehe180([A,B],[X,Y]), format('~s ~s:~t ~1f, ~1f',[S1,S2,X,Y]),nl,!;
    gedreht(G), G is 3,drehe90([A,B],[X,Y]), format('~s ~s:~t ~1f, ~1f',[S1,S2,X,Y]),nl,!.


%ktk([7,6],[1,7],[7,8]).
%ktk([2,6],[8,7],[2,8]).
%ktk([3,1],[8,4],[4,5]).
%ktk([7,1],[8,4],[4,5]).
%ktk([3,7],[8,4],[4,5]).     Noch zu fixen
%ktk([7,6],[8,7],[5,8]).
%ktk([5,8],[2,3],[4,4]).

