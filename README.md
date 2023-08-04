# panelAnalysis V. 1.0 (beta)
## Introduction
The panelAnalysis function is a starting one developed by [Dr. Oscar V. De la Torre Torres](https://oscardelatorretorres.com) to support the basic panel analysis for Econometrics-related students and researchers. If a given academic wants to perform an $n$ number of regressions, he or she must decide which panel regression model best fits (from pooled regression, fixed-effects, or random effects). The appropriate process is to estimate the three models and use the F and Hausman (1978) tests. This leads to performing the estimation of the three models and the estimation of these two tests.

This task can be done with the [plm package](https://github.com/ycroissant/plm/) of [Croissant and Millo](https://cran.r-project.org/web/packages/plm/vignettes/A_plmPackage.html). Imagine that this student or researcher wants to run $n=6$ different regression models and develop a summary table with the best-fitting regression in each $n$ equation. This would need to estimate 18 regressions (6 pooled, fixed-effects, and random-effects regressions) and a summary table of the 12 (6 f and 6 Hausman tests) fitting tests. Also, imagine that this student or researcher wants to export that summary table in *.docx, *.xls, or *.tex file.

Finally, the researcher would like to perform the panel regression analysis with the Newey-West (1987) robust method to estimate the standard errors (and the corresponding t and p-values).

Estimating several panel regressions, with robust standard errors and the corresponding fitting tests, is a task that could be done by programming with R. Still, it would need several code lines (Please refer to [Colonescu](https://bookdown.org/ccolonescu/RPoE4/)(2016) for more detail).

The present function (panelAnalysis) in its first version deals with this issue in R. Given a data frame object (`data`), an $n$ elements function vector `eqs`, a character object `outputFolder` that specifies the output folder for the analysis tables, and an $n$ elements `eqsType` object specifying the type of model to estimate, the next function or syntax estimates the set of regressions of interest:

`outPutPanel=panelAnalysis(eqs,outputFolder,data,eqsType)`

## Installing the function

I strongly invite you to execute this function in Rstudio in a Rmarkdown (*.Rmd) file. Why? Because the output folder path would be easier to set by just giving the name of the output folder in. the `outputFolder` input. If you want to run it in the R console it is fine. Remember to give the full hard drive path for the output folder (`C:/.../outputFolder`).

Because this function is in development (there is no R official package yet. it is to be published soon), you need to run (in your Rmarkdown chunk or the R console terminal) the next syntax:

```{r cinstallChunk}
source("https://raw.githubusercontent.com/OscarVDelatorreTorres/panelAnalysis/main/panelAnalysisR.R")
```

You will download the panelAnalysis, `logLik`, `AIC`, `BIC`, and `HQIC` functions by running it. The first function is the one of main interest, and the remaining three are also functions that I developed to estimate the log-likelihood function, the Akaike (1974), the Bayesian or Swchwarz (1978), and the Hannan-Quinn (1979) information criteria. Related to these three criteria, the original `plm` package has no function to estimate these. Therefore, these three functions are a new addition to Econometric analysis. Their use will be detailed next.

As mentioned, to estimate the set of panel regressions of interest, you will need to use the next R syntax:

```{r functionExample}
`outPutPanel=panelAnalysis(eqs,outputFolder,data,eqsType)`
```

A prerequisite for this function is the next set of packages:

- plm 
- stargazer
- sandwich
- lmtest
- officer 

Is it important to highlight that the `eqsType` object has 4 possible models to estimate (in future versions, I will include more panel regression variants of these):

1. Pooled regression (the input for this parameter is `"poolRegression"`).
2. Fixed effects (within groups) and with no time effects control (the input for this parameter is `"fixedEffects"`).
3. The Swamy-Arora (1972) random effects model (the input for this parameter is `"randomEffects"`). Please wait for future updates in the function for other random effects methods or the instrumental variable method.
4. To allow the function to estimate the three available type of panel regression models and to test for the best one, given the F and Hausman (1978) test (the input for this parameter is `"bestFitting"`).

**As an important estimation note**: If the fixed effects or the random effects model can not be estimated in the `"bestFitting"` option of the `eqsType` parameter, the function will deliver the only feasible and best fitting (possible pooled regression) model. If you select " fixedEffects " or " randomEffects " and the model is not feasible due to your sample data, the output tables will show a zero in that column.

The `plm` package (and this function) can estimate the regressions with balanced or unbalanced panels.

## A small example

Following the Carter-hill et al. (2011) examples for the panel data regression, I would like to use the Grunfeld example in that book. The authors estimated the next panel regression model:

$$inv_{i,t}=\alpha+\beta_1v_{i,t}+\beta_2k_{i,t}+\varepsilon_{i,t}$$

In the previous expression, $inv_{i,t}$ is the investment level of the i-th company (in USD millions), $v_{i,t}$ is the company value, and $k_{i,t}$ is the amount of capital.

To expose the use of the function, let's assume that we want to estimate these three functions:

1. $$inv_{i,t}=\alpha+\beta_1v_{i,t}+\varepsilon_{i,t}$$
2. $$inv_{i,t}=\alpha+\beta_2k_{i,t}+\varepsilon_{i,t}$$
3. $$inv_{i,t}=\alpha+\beta_1v_{i,t}+\beta_2k_{i,t}+\varepsilon_{i,t}$$

Also, let's assume that we want to compare the fourth model in a pool, fixed-effects, and random effects.
To estimate

**Another important estimation note**: the objects `eqs` and `eqsType` **must have the same length**. If you are going to estimate $n$ regressions, `eqs` and `eqsType` must have the text of the corresponding equations. That is, they must have a length of $n$. Please refer to the example for this issue.

The input data of this example has 5 columns:

1. `inv` that is $inv_{i,t}$.
2. `v` that is $v_{i,t}$.
3. `k` that is $k_{i,t}$.
4. `firm` that identifies the firm of the i-th row of data.
5. `year` that identifies the year or time of the i-th row of data.

The first three columns are the ones used in the conventional `plm` function and the data from the source is already a `pdata.frame` object (please refer to the [book of Colonescu](https://bookdown.org/ccolonescu/RPoE4/panel-data-models.html#organizing-the-data-as-a-panel) for more detail on how to create a `pdata.frame` object)

```{r example}
#==== Installing or uploading the necessary libraries and functions =====
if (!require(plm)) {install.packages('plm')
  library(plm)} else {library(plm)}
if (!require(stargazer)) {install.packages('stargazer')
  library(stargazer)} else {library(stargazer)}
if (!require(sandwich)) {install.packages('sandwich')
  library(sandwich)} else {library(sandwich)}
if (!require(lmtest)) {install.packages('lmtest')
  library(lmtest)} else {library(lmtest)}
if (!require(officer)) {install.packages('officer')
  library(officer)} else {library(officer)}

# The panelAnalysis function upload:

source("https://raw.githubusercontent.com/OscarVDelatorreTorres/panelAnalysis/main/panelAnalysisR.R")

#==== Data upload ====
# Uploading the Grunfeld data:
data("Grunfeld")

#==== Running the model ====
# folder specification (let's assume that the output filer will be stored in the same folder of the *.Rmd file you created for this code chunk):
outFolder="outputPanelFolder
# Equations to be estimated (6 regressions):
eqrVector=c("inv~v","inv~k","inv~k+t","inv~k+t","inv~k+t","inv~k+t")
# Models to be estimated (6 regresions):
eqsTypeVector=c("bestFitting","bestFitting","poolRegression","fixedEffects","randomEffects","bestFitting")

# Now, let's run the model and have fun:
outPutPanel=panelAnalysis(eqsVector,outputFolder,Grunfeld,eqsTypeVector)
```
