/*
Hypothesis: diagnose mental disorder in patients but check if mental disorder is not part of a category:

patient_diagnose(Patient,PersonSymptoms,Duration, Disorders) :-
    patient_exclude(Patient, Category), #get category to exclude
    find_disorders(Category,CategoryDisorders), % find all disorders associated an specific category
    possible_disorders(PersonSymptoms,Duration,PossibleDisorders), % find all possible disorders
    exclude_items(PossibleDisorders,CategoryDisorders,Disorders). % exclude category disorders from  the possible disorders
P(A, B, C, D) :-
    Q(A, E),
    R(E, F),
    T(B, C, G),
    X(G, F, D).
*/

/*---UTILS---*/
clear :- shell('clear').

/*---METAGOL CONFIGURATION---*/
:- use_module('metagol.pl').
metagol:max_clauses(5).
body_pred(patient_exclude/2).
body_pred(disorder_rule/3).
body_pred(possible_disorders/3).
body_pred(find_disorders/2).
body_pred(exclude_items/3).

/*---BACKGROUND KNOWLEDGE---*/
:- consult('bk.pl').
patient_exclude(cris, mood_disorders).
patient_exclude(john, psychotic_disorders).
patient_exclude(dani, fear_disorders).
patient_exclude(mary, pharaphilic_disorders).

/*---CLAUSES---*/
% Rule that counts how many elements of a list are present in another list.
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

% Rules determining whether a person has a disorders based on at least X symptoms and minimum duration.
has_schizophrenia(PersonSymptoms, Duration) :-
    integer(Duration),
    Duration >= 4,  % Duration check (in weeks)
    findall(Symptom, symptom_of(schizophrenia, Symptom),DisorderSymptoms), %Find symptoms
    count_matching_symptoms(PersonSymptoms,DisorderSymptoms, Count), %Count symptoms in common
    integer(Count),
    Count >= 3. %symptoms total check n

has_anxiety(PersonSymptoms, Duration) :-
    integer(Duration),
    Duration >= 26, 
    findall(Symptom, symptom_of(anxiety, Symptom),DisorderSymptoms),
    count_matching_symptoms(PersonSymptoms,DisorderSymptoms, Count),
    integer(Count),
    Count >= 3.

has_dysthymia(PersonSymptoms, Duration) :-
    integer(Duration),
    Duration >= 100, 
    findall(Symptom, symptom_of(dysthymia, Symptom),DisorderSymptoms),
    count_matching_symptoms(PersonSymptoms,DisorderSymptoms, Count),
    integer(Count),
    Count >= 2.

has_catatonia(PersonSymptoms, Duration) :-
    integer(Duration),
    Duration >= 2 , 
    findall(Symptom, symptom_of(catatonia, Symptom),DisorderSymptoms),
    count_matching_symptoms(PersonSymptoms,DisorderSymptoms, Count),
    integer(Count),
    Count >= 3.

has_obsessive_compulsive(PersonSymptoms, Duration) :-
    integer(Duration),
    Duration >= 26, 
    findall(Symptom, symptom_of(obsessive_compulsive, Symptom),DisorderSymptoms),
    count_matching_symptoms(PersonSymptoms,DisorderSymptoms, Count),
    integer(Count),
    Count >= 3.

has_bipolar(PersonSymptoms, Duration) :-
    integer(Duration),
    Duration >= 3, 
    findall(Symptom, symptom_of(bipolar, Symptom),DisorderSymptoms),
    count_matching_symptoms(PersonSymptoms,DisorderSymptoms, Count),
    integer(Count),
    Count >= 1.

has_panic_disorder(PersonSymptoms, Duration) :-
    integer(Duration),
    Duration >= 4, 
    findall(Symptom, symptom_of(panic_disorder, Symptom),DisorderSymptoms),
    count_matching_symptoms(PersonSymptoms,DisorderSymptoms, Count),
    integer(Count),
    Count >= 1.

has_post_traumatic_stress_disorder(PersonSymptoms, Duration) :-
    integer(Duration),
    Duration >= 4, 
    findall(Symptom, symptom_of(post_traumatic_stress_disorder, Symptom),DisorderSymptoms),
    count_matching_symptoms(PersonSymptoms,DisorderSymptoms, Count),
    integer(Count),
    Count >= 3.

has_conduct_dissocial_disorder(PersonSymptoms, Duration) :-
    integer(Duration),
    Duration >= 26, 
    findall(Symptom, symptom_of(conduct_dissocial_disorder, Symptom),DisorderSymptoms),
    count_matching_symptoms(PersonSymptoms,DisorderSymptoms, Count),
    integer(Count),
    Count >= 3.

has_pharaphilic_disorder(PersonSymptoms, Duration) :-
    integer(Duration),
    Duration >= 26, 
    findall(Symptom, symptom_of(pharaphilic_disorder, Symptom),DisorderSymptoms),
    count_matching_symptoms(PersonSymptoms,DisorderSymptoms, Count),
    integer(Count),
    Count >= 2.

%Rule to return the name of the disorder if the result is true.
disorder_rule(schizophrenia, Symptoms, Duration) :-
    has_schizophrenia(Symptoms, Duration).

disorder_rule(anxiety, Symptoms, Duration) :-
    has_anxiety(Symptoms, Duration).

disorder_rule(dysthymia, Symptoms, Duration) :-
    has_dysthymia(Symptoms, Duration).

disorder_rule(catatonia, Symptoms, Duration) :-
    has_catatonia(Symptoms, Duration).

disorder_rule(obsessive_compulsive, Symptoms, Duration) :-
    has_obsessive_compulsive(Symptoms, Duration).

disorder_rule(bipolar, Symptoms, Duration) :-
    has_bipolar(Symptoms, Duration).

disorder_rule(panic_disorder, Symptoms, Duration) :-
    has_panic_disorder(Symptoms, Duration).

disorder_rule(post_traumatic_stress_disorder, Symptoms, Duration) :-
    has_post_traumatic_stress_disorder(Symptoms, Duration).

disorder_rule(conduct_dissocial_disorder, Symptoms, Duration) :-
    has_conduct_dissocial_disorder(Symptoms, Duration).

disorder_rule(pharaphilic_disorder, Symptoms, Duration) :-
    has_pharaphilic_disorder(Symptoms, Duration).

% Rule to find all possible disorders given symptoms and duration.
possible_disorders(PersonSymptoms, Duration, Disorders) :-
    findall(Disorder, 
            (disorder(Disorder), %check if disorder exists
             disorder_rule(Disorder, PersonSymptoms, Duration)), %obtain mental disorder name
            Disorders).

% Exclude items from the first list that are in the second list.
exclude_items([], _, []). % Base case: if the first list is empty, the result is empty
exclude_items([H|T], ExcludeList, Result) :-
    member(H, ExcludeList), !, % If H is in the ExcludeList, skip it
    exclude_items(T, ExcludeList, Result).
exclude_items([H|T], ExcludeList, [H|Result]) :-
    exclude_items(T, ExcludeList, Result). % If H is not in the ExcludeList, include it in the Result

% Find disorders that belongs to a specific category.
find_disorders(Category,Disorders) :- findall(D, category_of(D, Category), Disorders).

/*---METARULES---*/
metarule(ident, [P,Q], [P,A,B], [[Q,A,B]]).
metarule(dident, [P,Q,R], [P,A,B], [[Q,A,B],[R,A,B]]).
metarule(chain,[P,Q,R], [P,A,C], [[Q,B,A],[R,B,C]]).
metarule(tailrec, [P,Q], [P,A,B], [[Q,A,C],[P,C,B]]).
metarule(find_exclude, [P,Q,R,T,X], [P,A,B,C,D], [[Q,A,E],[R,E,F],[T,B,C,G],[X,G,F,D]]).


/*---TRAINING---*/
a:-
    Pos = [
        patient_diagnose(cris,[fear,apprehension,nervous],100,[anxiety]),
        patient_diagnose(dani,[fear,apprehension,nervous],100,[])
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


