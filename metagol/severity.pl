/*
Hypothesis: Discovering severity of mental disorders based in total of symptoms.

 severity_level(Disorder, PersonSymptoms, Severity) :-
     find_symptoms(Disorder, DisorderSymptoms), %obtain all the symptons of the disorder
     count_matching_symptoms(PersonSymptoms,DisorderSymptoms, Count), %count matching symptoms
     severity(Count,Severity).  %obtain the severity level 

 P(A, B, C) :-
     Q(A, D), %obtain all the symptons of the disorder
     R(B ,D, E), %count matching symptoms
     X(E, C).  %obtain the severity level 
*/

/*---UTILS---*/
clear :- shell('clear').

/*---METAGOL CONFIGURATION---*/
:- use_module('metagol.pl').
metagol:max_clauses(3).
body_pred(severity/2).
body_pred(find_symptoms/2).
body_pred(count_matching_symptoms/3).

/*---BACKGROUND KNOWLEDGE---*/
:- consult('bk.pl').

/*---CLAUSES---*/
%Find symptoms with auxiliary function
find_symptoms(Disorder, DisorderSymptoms):- findall(Symptom, symptom_of(Disorder, Symptom),DisorderSymptoms).

%Severity  levels based in total of symptoms

severity(Count, mild) :- integer(Count), Count =< 2.
severity(Count, moderate) :- integer(Count), Count > 2, Count =< 5.
severity(Count, severe) :- integer(Count), Count > 5.

%Count matching symptoms
%Base case
count_matching_symptoms([], _, 0).
%Recursive case: symptom found in lists.
count_matching_symptoms([Symptom|Tail], SymptomsList, Count) :-
    member(Symptom, SymptomsList),
    count_matching_symptoms(Tail, SymptomsList, Count1),
    Count is Count1 + 1.
%Recursive case: symptom not found in list
count_matching_symptoms([Symptom|Tail], SymptomsList, Count) :-
    \+ member(Symptom, SymptomsList),
    count_matching_symptoms(Tail, SymptomsList, Count).

/*---METARULES---*/
metarule(ident, [P,Q], [P,A,B], [[Q,A,B]]).
metarule(chain,[P,Q,R], [P,A,C], [[Q,B,A],[R,B,C]]).
metarule(dident, [P,Q,R], [P,A,B], [[Q,A,B],[R,A,B]]).
metarule(tailrec, [P,Q], [P,A,B], [[Q,A,C],[P,C,B]]).
metarule(find_count, [P,Q,R,X],[P,A,B,C],[[Q,A,D],[R,B,D,E],[X,E,C]]).

/*---TRAINING---*/
a:-
    Pos = [
        severity_level(anxiety, [apprehension, difficulty_concentrating, disturbed_sleep],moderate),
        severity_level(anxiety, [apprehension, difficulty_concentrating, disturbed_sleep, fear, irritability, muscular_tension, nervous],severe),
        severity_level(anxiety, [apprehension],mild),
        severity_level(panic_disorder, [apprehension, fear, rapid_heart_palpitations, sweating, trembling, chest_pain, muscular_tension, dizziness, paraesthesias, depersonalization], severe),
        severity_level(panic_disorder, [apprehension, fear], mild)
        ],   
    Neg = [
        severity_level(anxiety, [fear],severe),
        severity_level(panic_disorder, [apprehension, fear, rapid_heart_palpitations, sweating, trembling, chest_pain, muscular_tension, dizziness, paraesthesias, depersonalization], mild),
        severity_level(panic_disorder, [apprehension, fear], moderate)
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