# Project with analysis in "Modeling Frailty Correlated Defaults with Multivariate Latent Factors"
This repository contains the analysis which is presented in "Modeling Frailty 
Correlated Defaults with Multivariate Latent Factors". There are four files 
in this project 

 - `markdown/preliminary.Rmd` with monthly hazard models without frailty. 
 - `markdown/preliminary-annual.Rmd` with annual hazard models without frailty. 
 - `markdown/rng-effects.Rmd` with monthly hazard models with frailty. 
 - `markdown/rng-effects-annual.Rmd` with annual hazard models with frailty. 
 
Each file, as of this writing, starts by printing the branch and commit id 
from [github.com/boennecd/US_PD_data](https://github.com/boennecd/US_PD_data). 
This repository is used to create the data set that is being used. The annual models 
are from an earlier analysis and are not up-to-date.

Knit the documents at the project root. You can change this in Rstudio in the "Knit" 
drop down menu. We use [packrat](https://rstudio.github.io/packrat/) to ensure 
that it is easier to reproduce the results. It should start installing the packages 
and exact versions of the packages if you open the `.Rproj` file.
