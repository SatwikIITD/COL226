
% Base cases
hastype(_, N, intT) :- integer(N).           % Numeric constant has type intT
hastype(_,N,floatT) :- float(N).
hastype(_, true, boolT). % True and False constants have type boolT
hastype(_, false, boolT).
hastype(_,S,stringT) :- atom(S).
hastype(G, pair(I, J), pairT) :- integer(I), integer(J). 
hastype(G,pair(varT(X),varT(Y)),pairT) :- member((X,intT),G), member((Y,intY),G).
hastype(G,pair(varT(X),Y),pairT) :- member((X,intT),G).
hastype(G,pair(X,varT(Y)),pairT) :- member((Y,intT),G).

hastype(G, varT(X), T) :- member((X, T), G).

% Arithmetic operators
hastype(G, E1 + E2, intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 - E2, intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 * E2, intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 / E2, intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 + E2, floatT) :- hastype(G, E1, floatT), hastype(G, E2, floatT).
hastype(G, E1 - E2, floatT) :- hastype(G, E1, floatT), hastype(G, E2, floatT).
hastype(G, E1 * E2, floatT) :- hastype(G, E1, floatT), hastype(G, E2, floatT).
hastype(G, E1 / E2, floatT) :- hastype(G, E1, floatT), hastype(G, E2, floatT).

% Pair addition
hastype(G, add_pairs(P1, P2), pairT) :-hastype(G, P1, pairT), hastype(G, P2, pairT).

% Pair subtraction
hastype(G, sub_pairs(P1, P2), pairT) :-  hastype(G, P1, pairT), hastype(G, P2, pairT).
  
% Pair multiplication
hastype(G, mul_pairs(P1, P2), pairT) :-  hastype(G, P1, pairT), hastype(G, P2, pairT).
    
% Pair division
hastype(G, div_pairs(P1, P2), pairT) :-  hastype(G, P1, pairT), hastype(G, P2, pairT).
    
   
% Boolean operators
hastype(_, E1 /\ E2, boolT) :- hastype(_, E1, boolT), hastype(_, E2, boolT).
hastype(_, E1 \/ E2, boolT) :- hastype(_, E1, boolT), hastype(_, E2, boolT).
hastype(_, \+ E, boolT) :- hastype(_, E, boolT).

 % Comparison operators
hastype(G, E1 = E2, boolT) :- hastype(G, E1, T), hastype(G, E2, T).
hastype(G, E1 > E2, boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 < E2, boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 = E2, boolT) :- hastype(G, E1, T), hastype(G, E2, T).
hastype(G, E1 > E2, boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 < E2, boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).

% Concatenation operator
hastype(G,concat(E1,E2),stringT) :- hastype(G,E1,stringT), hastype(G,E2,stringT).

