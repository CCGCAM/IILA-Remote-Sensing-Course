# From the project root: source("render_exercises.R")
stopifnot(requireNamespace("rmarkdown", quietly = TRUE))
files <- c("00-Setup.Rmd", file.path("Exercises", c(
  "01-Explore-Canopy.Rmd", "02-Trait-Sensitivity.Rmd",
  "03-Field-Spectra-and-Trait-Inversion.Rmd")))
for (file in files) {
  output <- sub("\\.Rmd$", "_practical.html", basename(file))
  rmarkdown::render(file, output_file = output,
                    knit_root_dir = getwd(),
                    envir = new.env(parent = globalenv()), quiet = FALSE)
}
