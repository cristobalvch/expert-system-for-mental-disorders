/*
    This Background Knowledge was created with these references:
    - International Classification of Diseases 11th Revision (IDC-11) : https://icd.who.int/browse/2024-01/mms/en#334423054.
    - Diagnostic and Statistical Manual of Mental Disorders (DSM-5) Content: https://www.psychiatry.org/File%20Library/Psychiatrists/Practice/DSM/APA_DSM-5-Contents.pdf
    - Diagnostic and Statistical Manual of Mental Disorders (DSM-5) Full Manual: https://repository.poltekkes-kaltim.ac.id/657/1/Diagnostic%20and%20statistical%20manual%20of%20mental%20disorders%20_%20DSM-5%20(%20PDFDrive.com%20).pdf
    - National Library of Medicine (NIH): https://www.ncbi.nlm.nih.gov/books/NBK519704/
*/

%Mental Disorders
disorder(schizophrenia).
disorder(anxiety).
disorder(dysthymia).
disorder(catatonia).
disorder(obsessive_compulsive).
disorder(bipolar).
disorder(panic_disorder).
disorder(post_traumatic_stress_disorder).
disorder(conduct_dissocial_disorder).
disorder(pharaphilic_disorder).

%schizophrenia
symptom(desilusions).
symptom(hallucinations).
symptom(disorganized_thinking).
symptom(disorganized_behaviour).
symptom(psychomotor_disturbances).
symptom(alogia).
symptom(disturbed_speech).
symptom(anhedonia).
symptom(asociality).

%anxiety
symptom(fear).
symptom(nervous).
symptom(apprehension).
symptom(worrying_thoughts).
symptom(muscular_tension).
symptom(disturbed_sleep).
symptom(irritability).
symptom(difficulty_concentrating).
symptom(rapid_heart_palpitations).
symptom(less_eating).


%dysthymia
symptom(depressed_mood).
symptom(loss_of_interest).
symptom(hopelesness).
symptom(indecisiveness).
symptom(irritability).
symptom(difficulty_concentrating).
symptom(disturbed_sleep).
symptom(disturbed_appetite).
symptom(reduced_energy).
symptom(fatigue).

%catatonia
symptom(stupor).
symptom(catalepsy).
symptom(mutism).
symptom(echolalia).
symptom(echopraxia).
symptom(hypothermia).
symptom(fever).
symptom(elevated_blood_pressure).
symptom(rapid_heart_palpitations).

%obsessive_compulsive
symptom(obsessions).
symptom(unwanted_urges).
symptom(unwanted_thoughs).
symptom(repetitive_behaviours).
symptom(compulsions).
symptom(rituals).
symptom(fear).

%bipolar
symptom(depressed_mood).
symptom(euphoria).
symptom(irritability).
symptom(increased_self_esteem).
symptom(disturbed_sleep).
symptom(difficulty_concentrating).
symptom(impulsive_behaviour).
symptom(loss_of_interest).
symptom(fatigue).
symptom(hopelesness).
symptom(psychomotor_disturbances).

%panic_disorder
symptom(apprehension).
symptom(fear).
symptom(rapid_heart_palpitations).
symptom(sweating).
symptom(trembling).
symptom(chest_pain).
symptom(muscular_tension).
symptom(dizziness).
symptom(paraesthesias).
symptom(depersonalization).

%post_traumatic_stress_disorder
symptom(traumatic_event).
symptom(intrusive_memories).
symptom(flashbacks).
symptom(nightmares).
symptom(disturbed_sleep).
symptom(fear).
symptom(hypervigilance).
symptom(reduced_energy).

%conduct_social_disorder
symptom(agression).
symptom(destruction_of_property).
symptom(deceitfulness).
symptom(theft).
symptom(cruelty).
symptom(rules_violation).

%pharaphilic_disorder
symptom(atypical_sexual_arousal).
symptom(sexual_thoughts).
symptom(sexual_fantasies).
symptom(urges).
symptom(impulsive_behaviour).

%relation between mental disorder and symptons
:- discontiguous symptom_of/2.
symptom_of(schizophrenia, desilusions).
symptom_of(schizophrenia, hallucinations).
symptom_of(schizophrenia, disorganized_thinking).
symptom_of(schizophrenia, disorganized_behaviour).
symptom_of(schizophrenia, psychomotor_disturbances).
symptom_of(schizophrenia, alogia).
symptom_of(schizophrenia, disturbed_speech).
symptom_of(schizophrenia, anhedonia).
symptom_of(schizophrenia, asociality).

symptom_of(anxiety, fear).
symptom_of(anxiety, nervous).
symptom_of(anxiety, apprehension).
symptom_of(anxiety, worrying_thoughts).
symptom_of(anxiety, muscular_tension).
symptom_of(anxiety, disturbed_sleep).
symptom_of(anxiety, irritability).
symptom_of(anxiety, difficulty_concentrating).
symptom_of(anxiety, rapid_heart_palpitations).
symptom_of(anxiety, sweating).

symptom_of(dysthymia, depressed_mood).
symptom_of(dysthymia, loss_of_interest).
symptom_of(dysthymia, hopelesness).
symptom_of(dysthymia, indecisiveness).
symptom_of(dysthymia, irritability).
symptom_of(dysthymia, difficulty_concentrating).
symptom_of(dysthymia, disturbed_sleep).
symptom_of(dysthymia, disturbed_appetite).
symptom_of(dysthymia, reduced_energy).
symptom_of(dysthymia, fatigue).

symptom_of(catatonia, stupor).
symptom_of(catatonia, catalepsy).
symptom_of(catatonia, mutism).
symptom_of(catatonia, echolalia).
symptom_of(catatonia, echopraxia).
symptom_of(catatonia, hypothermia).
symptom_of(catatonia, fever).
symptom_of(catatonia, elevated_blood_pressure).
symptom_of(catatonia, rapid_heart_palpitations).

symptom_of(obsessive_compulsive, obsessions).
symptom_of(obsessive_compulsive, unwanted_urges).
symptom_of(obsessive_compulsive, unwanted_thoughs).
symptom_of(obsessive_compulsive, repetitive_behaviours).
symptom_of(obsessive_compulsive, compulsions).
symptom_of(obsessive_compulsive, rituals).
symptom_of(obsessive_compulsive, fear).

symptom_of(bipolar, depressed_mood).
symptom_of(bipolar, euphoria).
symptom_of(bipolar, irritability).
symptom_of(bipolar, increased_self_esteem).
symptom_of(bipolar, disturbed_sleep).
symptom_of(bipolar, difficulty_concentrating).
symptom_of(bipolar, impulsive_behaviour).
symptom_of(bipolar, loss_of_interest).
symptom_of(bipolar, fatigue).
symptom_of(bipolar, hopelesness).
symptom_of(bipolar, psychomotor_disturbances).

symptom_of(panic_disorder, apprehension).
symptom_of(panic_disorder, fear).
symptom_of(panic_disorder, rapid_heart_palpitations).
symptom_of(panic_disorder, sweating).
symptom_of(panic_disorder, trembling).
symptom_of(panic_disorder, chest_pain).
symptom_of(panic_disorder, muscular_tension).
symptom_of(panic_disorder, dizziness).
symptom_of(panic_disorder, paraesthesias).
symptom_of(panic_disorder, depersonalization).

symptom_of(post_traumatic_stress_disorder, traumatic_event).
symptom_of(post_traumatic_stress_disorder, intrusive_memories).
symptom_of(post_traumatic_stress_disorder, flashbacks).
symptom_of(post_traumatic_stress_disorder, nightmares).
symptom_of(post_traumatic_stress_disorder, disturbed_sleep).
symptom_of(post_traumatic_stress_disorder, fear).
symptom_of(post_traumatic_stress_disorder, hypervigilance).
symptom_of(post_traumatic_stress_disorder, reduced_energy).

symptom_of(conduct_dissocial_disorder, agression).
symptom_of(conduct_dissocial_disorder, destruction_of_property).
symptom_of(conduct_dissocial_disorder, deceitfulness).
symptom_of(conduct_dissocial_disorder, theft).
symptom_of(conduct_dissocial_disorder, cruelty).
symptom_of(conduct_dissocial_disorder, rules_violation).

symptom_of(pharaphilic_disorder, atypical_sexual_arousal).
symptom_of(pharaphilic_disorder, sexual_thoughts).
symptom_of(pharaphilic_disorder, sexual_fantasies).
symptom_of(pharaphilic_disorder, urges).
symptom_of(pharaphilic_disorder, impulsive_behaviour).

%Category of mental disorders
category_of(schizophrenia, psychotic_disorders).
category_of(anxiety, fear_disorders).
category_of(dysthymia, mood_disorders).
category_of(catatonia, catatonia).
category_of(obsessive_compulsive, obsessive_compulsive_disorder).
category_of(bipolar, mood_disorders).
category_of(panic_disorder, fear_disorders).
category_of(post_traumatic_stress_disorder, stress_disorders).
category_of(conduct_dissocial_disorder, dissocial_disorders).
category_of(pharaphilic_disorder, pharaphilic_disorder).