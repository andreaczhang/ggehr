> Recall simulation basics

# One variable 

Directly from the data distribution



# Two variables 

|                                    | X (IV) discrete - groups                       | X (IV) continuous                       |
| ---------------------------------- | ---------------------------------------------- | --------------------------------------- |
| Y (DV) discrete - groups or yes/no | Smoking vs CB (analysis of contingency tables) | Age vs CB (LR, classification problems) |
| Y (DV) continuous                  | Height for men, women (hypo tests)             | Height weight (correlation)             |



Two ways

- Directly specify parameters for each one of the variables. We can not specify association in this way
- From a regression model
  - Linear model, y = b0 + b1x1 + ... bpxp + e
  - non linear,  additive model, e.g. log(y) = a + b log(x) + e
  - with interaction, e.g. y = b0 + b1x1x2 + e
  - GLM
  - Others: non-parametric, multilevel, measurement error models



### Links to common statistical tests as linear models

Y(c) - one sample t.test or non-param

X(d)Y(d) - contingency table. Chi-square test

X(d)Y(c) - t.test (two samples; ANOVA for more groups), non-param (KW for more)

X(c)Y(c) - correlation (Pearson, spearman) 



# Sim: XcYc

This is the simplest scenario, can directly come from a regression of the most common type.

This is for typical for correlations.



# Sim: XdYc

This is the common scenario for hypothesis tests: continuous measurement comparison between groups

<span style = 'color:tomato'>To Do</span>

- Basic: abstract the group-wise parameters. either by functions for dummy or else.
- Advanced: from the difference in groups (this could be less useful )



# Sim: XcYd

This scenario is not very common for hypothesis testing; but is the building block for GLM

<span style = 'color:tomato'>To Do</span>



# Sim: XcYd

At an individual level, both are categories (e.g. smoking = yes, CB = yes). It is more common to work on the aggregated level, which is a contingency table.

<span style = 'color:tomato'>To Do</span>











