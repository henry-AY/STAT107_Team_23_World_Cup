# Required packages to run the repository
packages <- c(
  "randomForest", "lubridate", "forcats", "stringr", "purrr", "tidyr",
  "tibble", "ggplot2", "tidyverse", "dplyr", "readr", "rmarkdown",
  "knitr", "corrplot", "RColorBrewer", "yaml", "scales", "gtable",
  "glue", "xfun", "broom"
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}
