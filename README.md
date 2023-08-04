# panelAnalysis V. 1.0

The panelAnalysis function is a starting one developed by [Dr. Oscar V. De la Torre Torres](https://oscardelatorretorres.com) to support the basic panel analysis for Econometrics-related students and researchers. If a given academic wants to perform an $n$ number of regressions, he or she must decide which panel regression model is the best fitting (from pooled regression, fixed-effects, or random effects). The appropriate process is to estimate the three models and use the F and Hausman (1978) tests. This leads to performing the estimation of the three models and the estimation of these two tests.

This task can be done with the [plm package](https://github.com/ycroissant/plm/) of [Croissant and Millo](https://cran.r-project.org/web/packages/plm/vignettes/A_plmPackage.html). Imagine that this student or researcher wants to run $n=6$ different regression models and develop a summary table with the best-fitting regression in each $n$ equation. This would need to estimate 18 regressions (6 pooled, fixed-effects, and random-effects regressions) and a summary table of the 12 (6 f and 6 Hausman tests) fitting tests. Also, imagine that this student or researcher wants to export that summary table in *.docx, *.xls, or *.tex file.

Finally, the researcher would like to perform the panel regression analysis with the Newey-West (1987) robust method to estimate the standard errors (and the corresponding t and p-values).

Estimating several panel regressions, with robust standard errors and the corresponding fitting tests, is a task that could be done by programming with R. Still, it would need several code lines (Please refer to [Colonescu](https://bookdown.org/ccolonescu/RPoE4/)(2016) for more detail).

The present function (panelAnalysis) in its first version deals with this issue in R. Given a data frame object (`Data`), an $n$ elements function vector `eqs` a character object `folder` that specifies the output folder for the analysyis tables and an $n$ elements `eqsType` object specifying

If t
