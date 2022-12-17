# Shiny app: plot mi-pca results

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7452124.svg)](https://doi.org/10.5281/zenodo.7452124)

You can interact with the results from the study [mi-pca](<https://github.com/EdoardoCostantini/mi-pca>) by:

- Visit the online [shiny app](https://edoardocostantini.shinyapps.io/plotmipca/).
- Installing this app locally as an R package:

    ```
    devtools::install_github("https://github.com/EdoardoCostantini/plotmipca")
    ```

## Plots

### Simulation study results

By using the R function:

```
plotmipca::plotResults()
```

a shiny app is started that allows you to plot the results of the simulation study.
The help file shows how to use and read the plots produced by the app.

### Convergence plots

By using the R function:

```
plot.mi.spcr::plotMids()
```

a shiny app is started that allows you to study the trace plots for MI imputation algorithms used in the simulation study.
The help file shows how to use and read the plots produced by the app.
