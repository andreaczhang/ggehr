# Project scope 

## Target users

Educators of biostatistics who wish to simulate realistic medical data to facilitate teaching;

Students (medicine, biostatistics) who wish to deepen their understanding of topics in biostatics by understanding the data generation process.



## Key features

Structural, modular (analogy of grammar of graphics)

Flexible, easy to extend 

Easy to use: can be combined with different existing statistical packages, and produce meaningful results that can be easily interpreted

Visualization 



## Statistical topics 

Study design

- a few types of common design, focus on clinical trial
- missing pattern: missing at random, not at random
- time pattern: drop-outs 

Descriptive statistics

- (univariate) mean, median, sd
- (multivariate) correlation
- (bivariate) proportions and risks (RR, OR, sensitivity/specificity)

Inference

- confidence interval
- t-test and non-parametric alternatives
- Type 1 and 2 errors, power

Modeling

- LM and GLM

- Mixed model

- Survival



## Layers (subject + outcome + relationship+ other)

#### Subject 

Demographics (gender)

Demographics (age)

Comobidity (obesity, diabetes, asthma, etc)

#### Type of outcome 

Continuous

Binary (1,0)

Time to event

#### Causal relationship 

This is actually the most important part; and it has to be flexible

(in the form of a function)

#### Other

Missing pattern, time pattern etc



## Presets 

(With some features based on real studies. The challenge would be the causal relationship)

Cohort study

Case-control study 

Clinical trial (parallel, crossover, adaptive) with different types of outcomes







