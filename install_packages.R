# Run once in the RStudio Console from the project root.
# The course was run end-to-end with R 4.5.1.
if (getRversion() < "4.5.1") {
  stop("This course was tested with R 4.5.1. Install R 4.5.1 or newer, then restart RStudio.")
}
options(repos=c(CRAN="https://cloud.r-project.org"))

cran <- c("rmarkdown", "knitr", "remotes", "randomForest", "plotly",
          "shiny", "shinythemes", "ggplot2", "reshape2", "patchwork")
missing <- cran[!vapply(cran, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing,
                   type=if (.Platform$OS.type=="windows") "binary" else getOption("pkgType"))
}

if (!requireNamespace("ToolsRTM", quietly = TRUE)) {
  remotes::install_gitlab("caminoccg/toolsrtm", upgrade = "never")
}

cat("R:", as.character(getRversion()), "\n")
cat("ToolsRTM:", as.character(packageVersion("ToolsRTM")), "\n")
cat("randomForest:", as.character(packageVersion("randomForest")), "\n")
cat("Ready. Open Exercises/01-Explore-Canopy.Rmd.\n")
cat("Optional STAC extension: install.packages(c('terra','jsonlite')) only if you refresh its series live.\n")
