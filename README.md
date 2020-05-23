
# Introduction

This is some work to implement [ASSA COVID-19 Model](http://www.assacovid19.org.za/) assumptions in R using Imperlial College MRC unit's [squire](https://github.com/mrc-ide/squire) package.

This works repeats the model as is approximately but it has age structure so results do not correspond broadly.  Theis model also uses slightly different incidence parameters compared to ASSA.

# Why do it?

Several reasons:

* Check reasonability of ASSA model.
* Produces output by age band which the ASSA model doesn't do.
* Corrects for double counting of an assymptiomatic assumption in ASSA.
* Corrects for an aggressive assymptomatic assumption in ASSA.
* Allows modelling of hospital and ICU bed capacity constraints.

# Assumptions

The code adjusts default assumptions provided by `squire`.  To explore the default assumptions for South Africa they can be extracted by `parameters_explicit_SEEIR` and one can then examine the object.  Below we do so and plot the per age-band hospitalisaion probabilities.

```r
parameters_default <-
  parameters_explicit_SEEIR(country = "South Africa")

parameters_default$prop_hosp

```

# Scenarios

Multiple scenarios are run corresponding to the original 4 ASSA scenarios:

* `sX_incorred` should be closest to the relevant scenario from ASSA.
* `sX_fix1` fixes the double counting of assymptomatic assumptions in the above.
* `sX_fix2` reverts to the implicit assymptomatic assumptions in Verity as used in `squire`.
* `sX` models excess deaths due to hospital and ICU bed capacity constraints as well.

# Capacity Constraints

We use the squire model's aassumptions for death if requireing ICU and not receiving it is 95%.  Similarly if hospital bed would be required but not available we assume 60%.  You can [read more about these here](https://mrc-ide.github.io/global-lmic-reports/parameters.html).  They state it's based on expert clinical opinion.

# Installation

To run this you need the `odin` and `squire` packages:

Install odin:
```r
install.packages("drat")
drat:::add("mrc-ide")
install.packages("odin")
```

Install squire:
```
install.packages("devtools")
devtools::install_github("mrc-ide/squire")
```
