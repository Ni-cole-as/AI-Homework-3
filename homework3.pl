/* Generate a random list */
randomList(N, List) :-
    length(List, N),
    maplist(random(0, 100), List).
    
/* Code for saving the list in the knowledge base */
    
/*swap the first two elements if they are not in order*/ 
 swap([X, Y|T], [Y, X | T]):- 
            Y =< X. 
/*swap elements in the tail*/ 
 swap([H|T], [H|T1]):- 
              swap(T, T1). 
/* Comment describing bubbleSort */
bubbleSort(L,SL):- 
            swap(L, L1), % at least one swap is needed 
             !, 
             bubbleSort(L1, FILLINHERE). 
bubbleSort(L, L). % here, the list is already sorted
/* Comment describing ordered */
ordered([]).
ordered([_X]).
ordered([H1, H2|T]):-
    H1 =< H2, 
    ordered([H2|T]).
/*Comment describing insert(E, SL, SLE) ...*/
/*Comment describing the 1st clause of insert ...*/
insert(X, [],[X]). 
insert(E, [H|T], [E,H|T]):- 
      ordered(T),
FILLINHERE(E, H), 
                        !. 
/*Comment describing the 2nd clause of insert ...*/
insert(E, [H|T], [H|T1]):- 
      ordered(T),
      insert(E, T, FILLINHERE). 
/* Comment describing insertionSort */
insertionSort([], []). 
insertionSort([H|T], SORTED) :- 
          insertionSort(T, T1), 
          insert(H, T1, FILLINHERE). 
/* Comment to describe meregeSort... */
mergeSort([], []).    %the empty list is sorted 
mergeSort([X], [X]):-!.
mergeSort(L, SL):- 
             split_in_half(L, L1, L2), 
             mergeSort(L1, FILLINHERE), 
             mergeSort(L2, S2),
             merge(S1, FILLINHERE, SL). 
/* Comment to describe split_in_half...*/
intDiv(N,N1, R):- R is div(N,N1).
split_in_half([], _, _):-!, fail.
split_in_half([X],[],[X]). 
split_in_half(L, L1, L2):- 
             length(L,N), 
             intDiv(N,2,N1),
             length(L1, FILLINHERE), 
             append(L1, L2, L). 
/* Comment describing merge(S1, S2, S) */
merge([], L, L). % comment
merge(L, [],L).  % comment 
merge([H1|T1],[H2|T2],[H1| FILLINHERE]):-
 H1 FILLINHERE  H2,
merge(T1,[H2|T2],T).
merge([H1|T1], [H2|T2], [H2|T]):-
FILLINHERE =< FILLINHERE
merge([H1|T1], T2, FILLINHERE).
   
/* Splits list [H|T] into two lists SMALL and BIG
   where SMALL is every element in the list smaller
   than the H or the Head of the list and BIG is
   every element larger than the Head. 
   Base case is empty*/
split(_, [],[],[]). 
 split(X, [H|T], [H|SMALL], BIG):- 
H =< X, 
    split(X, T, SMALL, BIG).    
 split(X, [H|T], SMALL, [H|BIG]):-
    X =< H,
    split(X, T, SMALL, BIG). 
/* Sorts list into ascending order list "LS"
   "LS" is a sorted [H|T] where SMALL and BIG
   are the resultinng splits of [H|T]
   Base case is empty list                  */
quickSort([], []).
quickSort([H|T], LS):-
        split(H, T, SMALL, BIG), 
        quickSort(SMALL, S), 
        quickSort(BIG, B), 
        append(S, [H|B], LS). 
/* Comment describing hybridSort */
hybridSort(LIST, bubbleSort, BIGALG, THRESHOLD, SLIST):-
length(LIST, N), N=< THRESHOLD,      
      bubbleSort(LIST, FILLINHERE).
hybridSort(LIST, insertionSort, BIGALG, THRESHOLD, SLIST):-
length(LIST, N), N=<T,
      insertionSort(LIST, SLIST).
=======
        append(S, [H|B], FILLINHERE). 

/* hybridSort dynamically uses a chosen recursive or
 * iterative sorting algorithm on the LIST depending
 * on the number of remaining unsorted elements. */

/* If the length of LIST is <= THRESHOLD, the choice of
 *  'small' algorithm is used to sort LIST.
 * Valid options for SMALL are
 *  'bubbleSort' and 'insertionSort'.
 * Note that once LIST is shorter than THRESHOLD, we
 *  no longer need to recursively call hybridSort
 *  because the list will never get any longer
 *  and BIGALG will no longer be necessary. */
hybridSort(LIST, bubbleSort, _BIGALG, THRESHOLD, SLIST):-
	length(LIST, N), N=< THRESHOLD,      
    bubbleSort(LIST, SLIST).

hybridSort(LIST, insertionSort, _BIGALG, THRESHOLD, SLIST):-
	length(LIST, N), N=<THRESHOLD,
    insertionSort(LIST, SLIST).

/* If the length of LIST is > THRESHOLD, the choice of
 * 'large' algorithm is used to sort LIST.
 * Valid options for BIGALG are 'bubbleSort' and
 * 'insertionSort'. */

hybridSort(LIST, SMALL, mergeSort, THRESHOLD, SLIST):-
	length(LIST, N), N> THRESHOLD,      
	split_in_half(LIST, L1, L2),
    hybridSort(L1, SMALL, mergeSort, THRESHOLD, S1),
    hybridSort(L2, SMALL, mergeSort, THRESHOLD, S2),
    merge(S1,S2, SLIST).
    
hybridSort([H|T], SMALL, quickSort, THRESHOLD, SLIST):-
length(LIST, N), N > THRESHOLD,      
split(H, T, L1, L2),
    FILLINHERE several lines in the body of this clause
    append(S1, [H|S2], SLIST).
    
hybridSort([H|T], SMALL, quickSort, THRESHOLD, SLIST):-
FILLINHERE the full body of this clause
    
