## Are freshwater fish more likely to break the temperature-size rule? Ten species in an artificially heated lake mostly get bigger with warming
*Asta Audzijonytea, Vytautas Rakauskas, Andrius Steponėnas, Vytautas Kesminas, Max Lindmark*

In this paper we analyze effects on temperature on length-at-age from a lake with a large temperature contrast over time, using Bayesian mixed models. We found that temperature had clear effects on fish length in all species, and in nine of 10 species this effect varied by age. However, only two species (ruffe and bleak) showed responses somewhat in line with TSR. 

### Reproducing Results

To reproduce our results you can either:

1. Fork the repository, clone it, open a new RStudio project with version control, and paste the repo url

2. Download a zip and work locally on your computer

We use [`renv`](https://rstudio.github.io/renv/articles/renv.html) to manage package versions. Once you've downloaded the project, run `renv::restore()` in your current working directory. This will install the package versions we used when this repository was archived. Note that packages are installed in a stand-alone project library for this paper, and will not affect your installed R packages anywhere else! `renv` does *not* help with different versions of R. We used R version 4.3.2, and ran the analysis on a 24 GB Apple M2 laptop.

### Repository structure

`R`: code to prepare data, run temperature and size models, and make figures. 

`data`: fish length at temperature data

`output`: folder with saved output (posterior samples)

`figures`: figures including figures for supporting information 

