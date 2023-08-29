# Code for a study assessing the impact and cost-effectiveness of maternal vaccination and long-acting monoclonal antibodies against RSV in England and Wales

This Github repository contains code to produce the figures in our paper "" on medRxiv. The code heavily depends on a package which runs the simulations and cost-effectiveness for user-defined parameters, `rsvie`.

## Installation

First install [R](https://cran.r-project.org/).

Though this repository is set up like an R package, I recommend you clone this package from GitHub and work through the vignettes. 
The code depends on several packages on [CRAN](https://cran.r-project.org/), which should install automatically by running the `R/main.R` script via the [`pacman`](http://trinker.github.io/pacman/vignettes/Introduction_to_pacman.html) package. Another package on this Github account runs the RSV model, `rsvie`, which also needs to be installed. This can be done by installing the `devtools` packages and calling 

```
library(devtools)
github_install("dchodge/rsvie")
library(rsvie)
```


## Overview of repository

The information in `data/`, `data-raw/`, and `datasource/` relate to the England and Wales-specific information parameterising the burden, risk of outcomes, costs and QALY loss. For more details on how this works, please see the `rsvie` package.

The `figs/` folder contains all the figures in the manuscript and supplementary.

The `outputs/` folder contains large RDS files that contain all the information about the impact and cost-effectiveness of each model after running the simulations via the 'rsvie`; package. 

The `R/` folder contains all the code used to run the models, process the outputs and plot the figures. 

## Explanation of vignettes


The `run_figs.Rmd` vignette recreates all the figures and supplementary figures in the manuscript.

The `get_metrics.Rmd` vignette recreates the values quoted in the manuscript's results section.

The `rshiny.Rmd` vignette creates a local RShiny app, which allows the user to explore the impact on the cost-effectiveness analysis when changing coverage and combined cost of purchasing and administration per dose of long-acting monoclonals and maternal vaccination.

## Contact

Any questions, please email me on david.hodgson@lshtm.ac.uk