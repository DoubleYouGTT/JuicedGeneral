#' JuicedGeneral
#'
#' These are some general useful functions that have not been included at any other location. Quite varied from calculations to plotting.
"_PACKAGE"

#Depend on a library. Before dependency can be taken, it needs to be installed.
#@param x The library to depend on
dependonlibrary <- function(x) {
  if (x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x,dependencies = TRUE)
  }
  usethis::use_package(x, type = "Depends")
}

dependonlibrary("devtools")
dependonlibrary("haven")
dependonlibrary("data.table")
dependonlibrary("readxl")
dependonlibrary("stringr")
dependonlibrary("stats")
dependonlibrary("svDialogs")
dependonlibrary("plotly")
dependonlibrary("shiny")
dependonlibrary("scales")
