# Shiny app: plot mi-pca results

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7452124.svg)](https://doi.org/10.5281/zenodo.7452124)

You can interact with the results from the study [mi-pca](<https://github.com/EdoardoCostantini/mi-pca>) by:

- Visit the online [shiny app](https://edoardocostantini.shinyapps.io/plotmipca/).
- Installing this app locally as an R package:

    - Install from GitHub:
    ```
    devtools::install_github("https://github.com/EdoardoCostantini/plotmipca")
    ```
    - Install from Zenodo version
        - Go to the [Zenodo page](https://doi.org/10.5281/zenodo.7452124)
        - Dowload the `.zip` archive
        - Unzip the folder if necessary
        - Open an R session and make sure to have the following dependencies installed:
            - `dplyr`,
            - `ggplot2`
            - `lattice`
            - `mice`
            - `pkgload`
            - `shinybrowser`
            - `shinyWidgets`
            - `shiny`
            - `usethis`
            You can use the following command to install any missing dependency:
            ```
            install.packages("shinybrowser")
            ```
        - Install the package from the local unzipped folder
        ```
        install.packages(
            "path to folder with the package",
            repos = NULL,
            type = "source"
        )
        ```

## Plots

To start the shiny apps and interact with the plots, open an R session and load the package:

```
library("plotmipca")
```

### Simulation study results

When you run the following R function:

```
plotmipca::plotResults()
```

a shiny app is started that allows you to plot the results of the simulation study.
The help file shows how to use and read the plots produced by the app.

### Convergence plots

When you run the following R function:

```
plotmipca::plotMids()
```

a shiny app is started that allows you to study the trace plots for MI imputation algorithms used in the simulation study.
The help file shows how to use and read the plots produced by the app.
