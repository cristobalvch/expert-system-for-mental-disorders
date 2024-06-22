/*
Hypothesis: Discovering Symptoms grouped by category.
*/

/*---UTILS---*/
clear :- shell('clear').

/*---METAGOL CONFIGURATION---*/
:- use_module('metagol.pl').

metagol:max_clauses(3).
body_pred(symptom_of/2).
body_pred(category_of/2).


/*---BACKGROUND KNOWLEDGE---*/
:- consult('bk.pl').

/*---METARULES---*/
metarule(ident, [P,Q], [P,A,B], [[Q,A,B]]).
metarule(chain,[P,Q,R], [P,A,C], [[Q,B,A],[R,B,C]]).

/*---TRAINING---*/
a:-
    Pos = [ 
        group_symptom(psychotic_disorders, desilusions),
        group_symptom(psychotic_disorders, hallucinations),
        group_symptom(psychotic_disorders, psychomotor_disturbances),
        group_symptom(psychotic_disorders, asociality),
        group_symptom(fear_disorders, fear),
        group_symptom(fear_disorders, nervous),
        group_symptom(fear_disorders, difficulty_concentrating),
        group_symptom(fear_disorders, sweating),
        group_symptom(bipolar, disturbed_sleep),
        group_symptom(pharaphilic_disorder, sexual_fantasies)
        ],   
    Neg = [
        group_symptom(psychotic_disorders, fear),
        group_symptom(psychotic_disorders, nervous),
        group_symptom(psychotic_disorders, echolalia),
        group_symptom(psychotic_disorders, echopraxia),
        group_symptom(fear_disorders, rules_violation),
        group_symptom(fear_disorders, destruction_of_property),
        group_symptom(fear_disorders, sexual_thoughts),
        group_symptom(fear_disorders,  urges),
        group_symptom(bipolar, sexual_fantasies),
        group_symptom(pharaphilic_disorder, psychomotor_disturbances)

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