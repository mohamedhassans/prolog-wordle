%% the engine 
main:-
   write('Welcome to Pro-Wordle!'),nl,
  write('----------------------'),nl,
  build_kb,nl,
  write('Done building the words database...'),nl,
  write('The available categories are:  '),categories(L), write(L),nl,
  play
     .
  
  
  
choose_category(L):-
   write('Choose a category:'),nl,
   read(C),categories(List),(member(C,List),L=C,!;write('This category does not exist.')
   ,nl,choose_category(L))  .
   
choose_length(Length,Category):-
  write('Choose a length:'),nl,read(L),(pick_word(_,L,Category)
    ,Length=L ,!;
  write('There are no words of this length.'),nl,choose_length(Length,Category)).
  
trytoguess(W,Category,Length,Trials):-
        write('Enter a word composed of '),write(Length), write(' letters:'),nl,
		read(Trial),(
		     
		     Trial==W,write('You Won!'),!
		     ;
			 
			 Trials==1,write('You lost!'),!
			 ;
			 
			 \+ string_length(Trial,Length),
		     write('Word is not composed of '),write(Length), write(' letters. Try again.'),nl,
			 write('Remaining Guesses are '),write(Trials),nl,write(''),nl,
			 trytoguess(W,Category,Length,Trials),!
			 ;
			 
			 atom_chars(W,WW),atom_chars(Trial,Triall),
			 correct_letters(Triall,WW,L1),remove(L1, L11),correct_positions(Triall,WW,L22),
			 write('Correct letters are: '),write(L11),nl,
			 write('Correct letters in correct positions are: '),write(L22),nl,
			 Trials2 is Trials-1,
			 write('Remaining Guesses are '),write(Trials2),nl,write(''),nl,
			 trytoguess(W,Category,Length,Trials2),!
			  ).
  

  
play:-
 choose_category(Category),nl,choose_length(Length,Category),
 Guesses is Length+1,
  write('Game started. You have '),write(Guesses), write(' guesses.'),nl,
  pick_word(W,Length,Category),!,
  trytoguess(W,Category,Length,Guesses).
  



build_kb:-
  write('Please enter a word and its category on separate lines:'),nl,
  read(W),
  (W==done;read(C),assert(word(W,C)),build_kb).
  
  
  
%%predicts
 
%is_category
is_category(C):-
 word(_,C).
 
%categories

categories(L):-
        setof(C,is_category(C),L).
  
%%avilablelenght
available_length(L):-
   word(W,_),string_length(W,L),!.
   
%%pickword
pick_word(W,L,C):-
   word(W,C),string_length(W,L).

%%correct_letters
correct_letters(L1,L2,CL):-
    intersect(L1,L2,CL).

intersect([X|Y],M,[X|Z]) :- member(X,M), intersect(Y,M,Z),!.
intersect([X|Y],M,Z) :- \+ member(X,M), intersect(Y,M,Z),!.
intersect([],_,[]):-!.

%%correct_positions
correct_positions(L1,L2,CP):-
  intersect1(L1,L2,CP).

intersect1([X|Y],[X|T],[X|Z]) :-  intersect1(Y,T,Z),!.
intersect1([X|Y],[X1|T1],Z) :-  X\==X1,intersect1(Y,T1,Z),!.
intersect1([],[],[]):-!.

remove([],[]):-!.
remove([H|T],[H|T1]):- \+member(H,T),remove(T,T1),!.
remove([H|T],TL):- member(H,T),remove(T,TL),!.