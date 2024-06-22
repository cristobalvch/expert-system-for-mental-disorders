/*
Hypothesis: Discovering common symptons in mental disorders.
*/
/*---UTILS---*/
clear :- shell('clear').

/*---METAGOL CONFIGURATION---*/
:- use_module('metagol.pl').

metagol:max_clauses(5).
body_pred(symptom_of/2).

/*---BACKGROUND KNOWLEDGE---*/
:- consult('bk.pl').

/*---METARULES---*/
metarule(ident, [P,Q], [P,A,B], [[Q,A,B]]).
metarule(chain,[P,Q,R], [P,A,C], [[Q,B,A],[R,B,C]]).
metarule(ichain,[P,Q], [P,A,B,C],[[Q,A,C],[Q,B,C]]).

/*---TRAINING---*/
a:-
    Pos = [
        common_symptom(anxiety, panic_disorder, sweating),
        common_symptom(panic_disorder, post_traumatic_stress_disorder, fear) 
        ],   
    Neg = [
    ],
    learn(Pos,Neg).

/*---TIME MEASUREMENT---*/

list_length([], 0).
list_length([_|Tail], Length) :-
    list_length(Tail, TailLength),
    Length is TailLength + 1.
    
time_run(T) :-
    get_time(T1),
    a,
    get_time(T2),
    T is T2 - T1.

average_time(N, AvgTime) :-
    findall(T, (between(1, N, _), time_run(T)), Times),
    sum_list(Times, TotalTime),
    AvgTime is TotalTime / N.

average_execution(N,AvgTime) :-
    findall(T, (between(1, N, _), time_run(T)), Times),
    list_length(Times,LenthTimes),
    sum_list(Times,TotalTime),
    AvgTime is TotalTime / LenthTimes.

/*---RUN AND TIME---*/
t :-
    N = 1,  % Number of runs for averaging
    average_execution(N, AvgTime),
    format('Average time over ~w runs: ~5f seconds~n', [N, AvgTime]).