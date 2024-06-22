<!-- ABOUT THE PROJECT -->
## About The Project



The project aims to develop a prototype of an expert system for mental disorders using Inductive Logic Programming (ILP), a subfield of Machine Learning which uses logic programming as a uniform representation for examples, background knowledge, and hypotheses. 

The system leverages:
* The use of Prolog, to model complex relations between symptoms, durations, severity and mental disorders. 
* The use of Metagol and Popper, to enhance the program's capabilities by finding interesting patterns in the data given by the project.

The basic functionality of the expert system is to suggest mental disorders based on relations between different variables such as symptoms, duration in weeks, severity, mental disorders, and categories of disorders. This information was obtained from the following medical resources:

* ICD-11: The latest edition of the International Classification of Diseases, a globally recognized diagnostic resource for epidemiology, health management, and clinical purposes. It was released by the World Health Organization (WHO) in
2018 and updated every year.

* DSM-5: The fifth edition of the Diagnostic and Statistical Manual of Mental Disorders, published by the American Psychiatric Association (APA) in 2013. It is a comprehensive classification of mental disorders, providing standardized criteria and definitions to help diagnose mental health conditions.

<b>Important!!!</b>:
This is just a prototype so it should NOT be used to diagnose mental disorders. It’s crucial to remember that mental disorders must always be diagnosed by qualified professionals to ensure appropriate care and treatment.

<!-- GETTING STARTED -->
## Getting Started

### Prerequisites

The following programming languages are necessary to run this project:
* Prolog 
* Python 

<!-- USAGE EXAMPLES -->
## Usage

### Expert System
1. Inside the folder code open a new terminal and run ```swipl ```
2. Load the file in prolog by running ```[expert_system].```
3. Query for mental disorders by running, eg: 
```sh
possible_disorders([fear, nervous, apprehension], 26, D).
```
The formal definition of this predicate is ```possible_disorders( list_of_symptoms, duration_in_weeks, output_disorders ).```

4. To list all the possible symptoms run ```symptom(S).```
5. To list all the possible mental disorders run ```disorder(D).```
6. To list all the symptoms of a mental disorder run, eg: ```disorder(anxiety, S).```

### Metagol
1. Inside the folder code/metagol open a new terminal and run ```swipl ```
2. Load the desired file in prolog by running ```[file_name].```
3. Learn the predicate by running ```t.```

### Popper
1. Inside the folder code/popper open a new terminal and run
3. Learn the desired predicate by running 
```sh
python popper.py examples/folder_name
```
<!-- CONTACT -->
## Contact

Cristobal Veas - [linkedln](https://twitter.com/your_username) - cristobalvch29@gmail.com

<p align="right">(<a href="#readme-top">back to top</a>)</p>