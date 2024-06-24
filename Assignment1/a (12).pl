/* membership of X in S */
/* no element is a member of the empty set */
mem(X,[]) :- fail.
/* X is a member of a set containing X, of course! */
mem(X,[X|_]).
/* if X is not the first chosen element of S,
then it may be a later element */
mem(X,[_|R]) :- mem(X,R).

/*  del(X,L1,L2) -- delete element X from a list L1 to obtain L2 */ 
del(X, [ ] , [ ]) :- !.
del(X, [X|R], Z) :- del(X, R, Z), !.
del(X, [Y|R], [Y|Z]) :- del(X, R, Z), !.

/*  remdups(L, L1) remove duplicates from a list L to get L1 */
remdups([ ], [ ]) :- !.
remdups([X|R], [X|Z]) :- del(X, R, L), remdups(L, Z).

/* Assuming no duplicates in S1, S2
here is an implementation of union of S1, S2 */
unionI([ ], S2, S2) :- !.
unionI(S1, [ ], S1) :- !.
unionI([X|R], S2, [X|Z]) :- del(X, S2, S3),  unionI(R, S3, Z).

/* Test Cases

1. Input- S1-[1,2,3]  S2-[2,3,4] 
   Output- S-[1,2,3,4]
   This Test Case shows that Union does not have duplicates.
2. Input- S1-[1]  S2-[2,3,4] 
   Output- S-[1,2,3,4]
3. Input- S1-[]  S2-[1,2,3,4] 
   Output- S-[1,2,3,4]
4. Input- S1-[1,2]  S2-[2,1] 
   Output- S-[1,2]
5. Input- S1-[1,2,3]  S2-[2,1,4,3] 
   Output- S-[1,2,3,4]

*/

/* append(L1, L2, L3) -- append lis  L1 to list L2 to get list  L3 */
append( [ ], L, L).
append( [X|R], L, [X|Z]) :- append(R, L, Z).

/* mapcons(X,L1, L2) --  cons the element X to each list in L1 to get L2 */
mapcons(X, [ ], [ ]) :- !.
mapcons(X, [Y|R], [ [X|Y] | Z ]) :- mapcons(X, R, Z).

/* powerI( S, P1): Here is an implementation of powerset of S */
powerI([ ], [ [ ] ]) :- !.
powerI([X|R], P) :- powerI(R, P1),  mapcons(X, P1, P2), append(P2, P1, P).

/* Test Cases

1. Input- I-[] 
   Output- O-[[]]
2. Input- I-[1] 
   Output- O-[[1], []]
3. Input- I-[1,2] 
   Output- O- [[1, 2], [1], [2], []]
4. Input- I-[1,1] 
   Output- O- [[1, 1], [1], [1], []]
5. Input- I-[1,2,3] 
   Output- O-[[1, 2, 3], [1, 2], [1, 3], [1], [2, 3], [2], [3], []]

*/


/*-----------------------------------------------PART-A begins------------------------------------------------*/

/* loop */
loop((X,Z),[Y|S], R) :- is_mem((X,Y),R,S), is_mem((Y,Z),transclos(R),S) ;loop((X,Z),S,R).

/* Membership check in Relations */
is_mem((X,Y),[],S) :- fail.
is_mem((X,Y),[(X,Y)|T],S).
is_mem((X,Y),[_|T],S) :- is_mem((X,Y),T,S).

/* Transitive Closure */
is_mem((X,Y), transclos(R), S) :- is_mem((X,Y), R, S), !.
is_mem((X,Z), transclos(R), S) :- loop((X,Z), S, R).

/* Test Cases

1. Input- R-[(1,2),(2,3),(3,4)], S-[1,2,3,4], Query-(1,4) 
   Output- True
2. Input- R-[(1,2),(2,3),(3,1)], S-[1,2,3,4], Query-(1,4) 
   Output- False
3. Input- R-[(1,2),(2,1)], S-[1,2], Query-(1,1) 
   Output- True
4. Input- R-[(1,2),(2,3),(3,4)], S-[1,2,3,4], Query-(2,1) 
   Output- False
5. Input- R-[(1,1),(2,2)], S-[1,2], Query-(1,2) 
   Output- False

*/

/* Reflexive Closure */
is_mem((X,X), refclos(R),S) :- mem(X,S).
is_mem((X,Y), refclos(R),S) :- is_mem((X,Y),R,S).

/* Test Cases

1. Input- R-[(1,2),(2,3),(3,4)], S-[1,2,3,4], Query-(1,1) 
   Output- True
2. Input- R-[(1,2),(2,3)], S-[1,2], Query-(1,3) 
   Output- False
3. Input- R-[(1,2),(2,3)], S-[1,2], Query-(1,1) 
   Output- True
4. Input- R-[(1,2),(2,3)], S-[1,2], Query-(2,2) 
   Output- True
5. Input- R-[(1,2),(2,3)], S-[1,2], Query-(2,1) 
   Output- False

*/

/* Symmetric Closure */
is_mem((X,Y),symclos(R), S) :- is_mem((X,Y),R, S).
is_mem((X,Y),symclos(R), S) :- is_mem((Y,X), R,  S).

/* Test Cases

1. Input- R-[(1,2),(2,3),(3,4)], S-[1,2,3,4], Query-(1,1) 
   Output- False
2. Input- R-[(1,2),(2,3)], S-[1,2], Query-(2,1) 
   Output- True
3. Input- R-[(1,2),(2,3)], S-[1,2], Query-(1,1) 
   Output- False
4. Input- R-[(1,2),(2,3)], S-[1,2], Query-(3,2) 
   Output- True
5. Input- R-[(1,2),(2,3)], S-[1,2], Query-(1,3) 
   Output- False

*/

/*------------------------------------------------- Question 1----------------------------------------------*/

/* Reflexive-Transitive Closure */
is_mem((X,Y), reftransclos(R),S) :- is_mem((X,Y),refclos(R),S).
is_mem((X,Y), reftransclos(R), S) :- is_mem((X,Y),transclos(R),S).

/* Test Cases

1. Input- R-[(1,2),(2,3),(3,4)], S-[1,2,3,4], Query-(1,4) 
   Output- True
2. Input- R-[(1,2),(2,3),(3,1)], S-[1,2,3,4], Query-(2,2) 
   Output- True
3. Input- R-[(1,2),(2,1)], S-[1,2], Query-(1,1) 
   Output- True
4. Input- R-[(1,2),(2,3),(3,4)], S-[1,2,3,4], Query-(2,1) 
   Output- False
5. Input- R-[(1,1),(2,2)], S-[1,2], Query-(1,2) 
   Output- False

*/

/*------------------------------------------------- Question 2----------------------------------------------*/

/* Reflexive-Symmetric-Transitive Closure */
is_mem((X,Y),rst_clos(R),S) :- is_mem((X,Y),(R),S).
is_mem((X,X),rst_clos(R),S) :- mem(X,S).
is_mem((X,Y),rst_clos(R),S) :- is_mem((X,Y),transclos(symclos(R)),S).

/* Test Cases

1. Input- R-[(1,2),(1,3)], S-[1,2,3], Query-(2,3) 
   Output- True
2. Input- R-[(1,2),(2,3),(3,1)], S-[1,2,3,4], Query-(2,2) 
   Output- True
3. Input- R-[(1,2),(3,4),(1,4)], S-[1,2,3,4], Query-(2,3) 
   Output- True
4. Input- R-[(1,2),(3,4),(1,4)], S-[1,2,3,4], Query-(3,1) 
   Output- True
5. Input- R-[(1,2),(3,4)], S-[1,2,3,4], Query-(1,3) 
   Output- False

*/

/*-----------------------------------------------PART-A ends------------------------------------------------*/

/*-----------------------------------------------PART-B begins------------------------------------------------*/

/*------------------------------------------------- Question 1,2----------------------------------------------*/

/* Test Cases for Union

1. Input- S1-[1,2,3]  S2-[2,3,4] 
   Output- S-[1,2,3,4]
   This Test Case shows that Union does not have duplicates.
2. Input- S1-[1]  S2-[2,3,4] 
   Output- S-[1,2,3,4]
3. Input- S1-[]  S2-[1,2,3,4] 
   Output- S-[1,2,3,4]
4. Input- S1-[1,2]  S2-[2,1] 
   Output- S-[1,2]
5. Input- S1-[1,2,3]  S2-[2,1,4,3] 
   Output- S-[1,2,3,4]

*/

/* Test Cases for power

1. Input- I-[] 
   Output- O-[[]]
2. Input- I-[1] 
   Output- O-[[1], []]
3. Input- I-[1,2] 
   Output- O- [[1, 2], [1], [2], []]
4. Input- I-[1,1] 
   Output- O- [[1, 1], [1], [1], []]
5. Input- I-[1,2,3] 
   Output- O-[[1, 2, 3], [1, 2], [1, 3], [1], [2, 3], [2], [3], []]

*/

/*------------------------------------------------- Question 3----------------------------------------------*/

/* Intersection */
inter([],_,[]).
inter([X|R],S2,[X|S3]) :- mem(X,S2),inter(R,S2,S3).
inter([X|R],S2,S3) :- \+ mem(X,S2),inter(R,S2,S3).

/* Test Cases

1. Input- S1-[1,2,3]  S2-[2,3,4] 
   Output- S-[2,3]
2. Input- S1-[3,4]  S2-[1,2] 
   Output- S-[]
3. Input- S1-[1,2,3]  S2-[1,2,3] 
   Output- S-[1,2,3]
4. Input- S1-[]  S2-[2,1] 
   Output- S-[]
5. Input- S1-[1,3,2]  S2-[2,1,4,5] 
   Output- S-[1,2]

*/

/*------------------------------------------------- Question 4----------------------------------------------*/

/* Set Difference */
diff([],_,[]).
diff([X|R],S2,[X|S3]) :- diff(R,S2,S3), \+ mem(X,S2).
diff([X|R],S2,S3) :- mem(X,S2),diff(R,S2,S3).

/* Test Cases

1. Input- S1-[1,2,3]  S2-[2,3,4] 
   Output- S-[1]
2. Input- S1-[3,4]  S2-[1,2] 
   Output- S-[3,4]
3. Input- S1-[1,2,3]  S2-[1,2,3] 
   Output- S-[1,2,3]
4. Input- S1-[]  S2-[2,1] 
   Output- S-[]
5. Input- S1-[1,3,2]  S2-[2,1,4,5] 
   Output- S-[3]

*/

/*------------------------------------------------- Question 5----------------------------------------------*/

/* Cartesian Product */
car([],S2,[]).
car([X|S1],S2,S3) :- mapcons(X,S2,Y), car(S1,S2,S4), append(Y,S4,S3).

/* Test Cases

1. Input- S1-[1,2]  S2-[1,2] 
   Output- S- [[1|1], [1|2], [2|1], [2|2]]
2. Input- S1-[3,4,5]  S2-[1,2] 
   Output- S-[[3|1], [3|2], [4|1], [4|2], [5|1], [5|2]]
3. Input- S1-[1,2,3]  S2-[] 
   Output- S-[]
4. Input- S1-[1]  S2-[2,1] 
   Output- S- [[1|2], [1|1]]
5. Input- S1-[]  S2-[] 
   Output- S-[]

*/

/*------------------------------------------------- Question 7----------------------------------------------*/

/* Subset */
subset([],S2).
subset([X|S1],S2) :- mem(X,S2), subset(S1,S2).

/* Part 7 */
check(X,[]) :- fail.
check(X,[Y|Z]) :- subset(X,Y), subset(Y,X); check(X,Z).
check_equal(S1,S2) :- powerI(S1,A), powerI(S2,B), power_equal(A,B), power_equal(B,A).
power_equal(A,[]) :- !.
power_equal(A,[X|B]) :- check(X,A), power_equal(A,B).

/*-----------------------------------------------PART-B ends------------------------------------------------*/

