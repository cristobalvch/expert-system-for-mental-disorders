/*
Main file with clauses to diagnose mental disorders.
*/


/*---UTILS---*/
clear :- shell('clear').
:- set_prolog_flag(answer_write_options, [max_depth(0)]).

/*---BACKGROUND KNOWLEDGE---*/
:- consult('metagol/bk.pl').

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

%Rule to return the name of the disorder if the result is true
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
