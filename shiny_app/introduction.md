Simulating Treatment Heterogeneity and Cycle Effects
================
Laura Cosgrove
12/1/2019

# Motivation

For population inference based on an analysis of many n-of-1 trials, I
believe that the choice of method should best fit the features that
motivate the choice of and design of n-of-1 trials, namely, the presence
of heterogeneity in individual treatment effects, and important design
considerations such as the presence of period effects and
autocorrelation. A discussion and theoretical advancement of the
importance of simulating data that account for the features that
randomization – in this case, period effects and treatment by patient
interaction—was elegantly put forth by Araujo et al. (2016). The
recommendations of Araujo et al. are summarized in the following: in the
presence of treatment-by-patient heterogeneity and period heterogeneity,
it is always valid and recommended to use a random effects model
accounting for the above sources of variation, and when only summary
measures are present it is also valid to use a random effects
meta-analysis model for estimation. However, for simply testing the
strict null hypothesis of no treatment effect, treatment interaction by
patient can be ignored, and either matched-pair t tests or fixed effect
meta-analysis can be used. While Araujo et al. do not discuss Bayesian
hierarchical models, Empirical Bayes estimates are discussed in a
further important conclusion advancing the use of a mixed-model approach
to estimate the variance of the treatment effect. That is, if one were
to use the results of the n-of-1 meta-analysis to treat future patients,
one can use the estimated treatment effect mean and the treatment effect
variance to improve the interim treatment effect estimates for a patient
using the Empirical Bayes method for example after one cycle. It seems
to me that a fully Bayesian treatment may also be used for future
patients; the essential ingredient is that the heterogeneity (i.e.,
variance) of the treatment effect is estimated with the meta-analysis.

# Model for Simulating a Series of n-of-1 trials

As a guide, I construct the simulation study in the assumed setting of a
meta-analysis of the PREEMPT study of chronic pain (Barr et. al, 2016),
whose design but not result has been published, attending to important
features of the design and data in order to construct my simulation
dataset assuming that the question of interest is the true population
treatment effect of one treatment contrasted against one other (though
the PREEMPT study is designed to choose two of many different treatment
options for a patient based on clinician judgment).

The model is as follows:

\[Y_{irs} = \lambda_i + \beta_{ir} + \epsilon_{irs} + Z_{irs}\tau_i\]

where \(Y_{irs}\) is the measured outcome for occasion s, \(s = 1, 2\)
of cycle r, \(r = 1, 2, .... k\) for patient i, \(i = 1, 2, n\), and:

  - \(\lambda_i \sim N(\Lambda, \phi^2)\): Patient-specific mean of the
    outcome, drawn from a normal distribution with population mean of
    \(\Lambda\)

  - \(\beta_{ir} \sim N(0, \gamma^2)\) : Within patient, within cycle
    (block) random noise

  - \(\epsilon_{irs} \sim N(0, \sigma^2)\) : Within patient, within
    cycle (block), within occassion (independent of treatment) random
    noise

  - \(\tau_{i} \sim N(T, \psi^2)\): Where \(Z_{irs}\) takes values
    \(\frac{1}{2}\) when the occassion \(s\) has treatment A and
    \(-\frac{1}{2}\) when occassion \(s\) has treatment B, the
    patient-specific for treatment effect, drawn from a normal
    population mean treatment effect of T. Therefore, an estimate for
    HTE (heterogeneity of treatment effect) is \(\psi^2\).

From our PREEMPT study setting:

“The sample size required for the proposed RCT is based on the primary
outcome: change from baseline to 26 weeks on the PROMIS pain
interference scale. Assuming that the minimally important difference is
0.4 SD difference (4 points) (…) Assuming a common standard deviation of
10 points, each group (Trialist app and usual care) would need to
include 122 patients (244 in total) in order to have 80% power to detect
a 3.6-point difference in means using a 2-group t-test with a 0.05
2-sided significance level.”

Reference: PROMIS scores have a mean of 50 and standard deviation (SD)
of 10 in a clinically relevant referent population.

We hope to recover our parameter values in the mixed effects model:

`lme4::lmer(Y ~ 1 + Z + (1|id:cycle) + (Z|id) , data = df_rand)`

  - We set \(\phi\) in \(\lambda_i \sim N(\Lambda, \phi^2)\), the
    patient-specific mean of the outcome, drawn from a normal
    distribution with population mean of \(\Lambda\). In the model, the
    corresponding estimate in the variance of the outcome is under the
    estimated standard deviation of the random effect of the individual
    id.

  - We set \(\Lambda\) in \(\lambda_i \sim N(\Lambda, \phi^2)\). In the
    model, the corresponding estimate in the mean of the outcome is
    under the fixed effects intercept term.

  - We set \(\gamma\) in \(\beta_{ir} \sim N(0, \gamma^2)\), the within
    patient, within cycle (block) random noise. In the model, the
    corresponding estimate in the mean of the outcome is under the
    estimated standard deviation of the nested random effect,
    `id:cycle`.

  - We set \(\sigma\) in \(\epsilon_{irs} \sim N(0, \sigma^2)\), the
    within patient, within cycle (block), within occassion (independent
    of treatment) random noise: in the model, it is the estimated
    standard deviation of the residual random effect.

  - We set T in \(\tau_{i} \sim N(T, \psi^2)\), the population average
    treatment effect. In the model, it is estimated under the fixed
    effects slope term for Z.

  - We set \(\psi\) in \(\tau_{i} \sim N(T, \psi^2)\), the heterogeneity
    of treatment effect. In the model, it is estimated under the
    estimated standard deviation of the random effects slope term of Z
    by treatment.

# References

Araujo, A., Julious, S., & Senn, S. (2016). Understanding Variation in
Sets of N-of-1 Trials. PLOS ONE, 11(12), e0167167.
<https://doi.org/10.1371/journal.pone.0167167>
