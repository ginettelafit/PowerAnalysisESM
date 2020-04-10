# PowerAnalysisESM

## Shiny app to perform power analysis to select the number of participants in ESM studies

Users can download the app and run locally on their computer by executing the following commands in R or Rstudio.

```
library(htmltools)
library(shiny)
library(DT)
library(nlme)
library(ggplot2)
library(gridExtra)
library(data.table)
library(plyr)
library(dplyr)
library(formattable)
library(tidyr)
library(MASS)
library(plyr)
library(parallel)
library(shinyjs)


library(devtools)
devtools::install_github("ginettelafit/PowerAnalysisESM", force = T)

library(PowerAnalysisESM)

runGist("https://gist.github.com/ginettelafit/6bac9d35c2521cc4fd91ce4b82490236")
```
