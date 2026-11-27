# Shared R helpers for the four course exercises. Source from the project root.
s2_band_names <- c("B01","B02","B03","B04","B05","B06","B07",
                   "B08","B8A","B09","B10","B11","B12")
s2_model_bands <- c("B02","B03","B04","B05","B06","B07",
                    "B08","B8A","B11","B12")

baseline_canopy <- function() {
  data.frame(LAI = 3, hspot = 0.01, LIDFa = -0.35, LIDFb = -0.15,
             TypeLidf = 1, tts = 30, tto = 0, psi = 0,
             N = 1.5, Cab = 40, Car = 8, Anth = 1, Cbrown = 0,
             EWT = 0.009, LMA = 0.009, alpha = 40)
}

baseline_canopy_pro <- function() {
  row <- baseline_canopy()
  row$LMA <- 0
  row$Prot <- 0.0045
  row$CBC <- 0.005
  row
}

simulate_canopy <- function(row, soil_fraction_dry = 0.5,
                            leaf_model = "PROSPECT-D") {
  stopifnot(requireNamespace("ToolsRTM", quietly = TRUE),
            nrow(row) == 1, soil_fraction_dry >= 0,
            soil_fraction_dry <= 1,
            leaf_model %in% c("PROSPECT-D", "PROSPECT-PRO"))
  d <- ToolsRTM::dataSpec_PDB
  soil <- soil_fraction_dry * d[, 11] + (1 - soil_fraction_dry) * d[, 12]
  sail <- ToolsRTM::foursail(inputLUT = row, rsoil = soil,
                            LeafModel = leaf_model)
  rfl <- ToolsRTM::Compute_BRF(rdot = sail$rdot, rsot = sail$rsot,
                                tts = row$tts, data.light = d)
  data.frame(wavelength = d[, 1], reflectance = as.numeric(rfl))
}

sentinel2_bands <- function(spectrum) {
  out <- ToolsRTM::get.spectral.convolution.srf(
    df = data.frame(wave = spectrum$wavelength, rfl = spectrum$reflectance),
    srf = ToolsRTM::srf.sentinel2a)
  data.frame(wavelength = out$wl, reflectance = out$RFL)
}

nearest_band <- function(bands, center_nm) {
  bands$reflectance[which.min(abs(bands$wavelength - center_nm))]
}

course_lut_path <- function() {
  root <- if (file.exists("R/rtm_helpers.R")) "." else ".."
  file.path(root, "outputs", "course_lut.rds")
}

load_course_lut <- function() {
  root <- if (file.exists("R/rtm_helpers.R")) "." else ".."
  made <- file.path(root,"outputs","course_lut.rds")
  seed <- file.path(root,"data","course_lut_seed.rds")
  if (file.exists(made)) readRDS(made) else readRDS(seed)
}

build_course_lut <- function(n = 500, seed = 2026) {
  set.seed(seed)
  traits <- data.frame(
    Cab = runif(n, 10, 80), EWT = runif(n, 0.005, 0.025),
    Cbrown = runif(n, 0, 0.5), Prot = runif(n, 0.002, 0.015),
    CBC = runif(n, 0.003, 0.018), LAI = runif(n, 0.5, 6),
    N = runif(n, 1.2, 2.2), Car = runif(n, 5, 15),
    psoil = runif(n, 0.2, 0.8), tts = runif(n, 20, 45)
  )
  wavelength <- ToolsRTM::dataSpec_PDB[, 1]
  spectra <- t(vapply(seq_len(n), function(i) {
    row <- baseline_canopy_pro()
    for (name in setdiff(names(traits),"psoil")) row[[name]] <- traits[[name]][i]
    simulate_canopy(row, soil_fraction_dry=traits$psoil[i],
                    leaf_model = "PROSPECT-PRO")$reflectance
  }, numeric(length(wavelength))))
  example <- sentinel2_bands(data.frame(
    wavelength = wavelength, reflectance = spectra[1, ]))
  bands <- t(vapply(seq_len(n), function(i) {
    sentinel2_bands(data.frame(
      wavelength = wavelength, reflectance = spectra[i, ]))$reflectance
  }, numeric(nrow(example))))
  stopifnot(ncol(bands)==length(s2_band_names))
  colnames(bands) <- s2_band_names
  lut <- data.frame(traits, bands, check.names = FALSE)
  if (!all(is.finite(as.matrix(lut)))) {
    stop("The LUT contains non-finite values. Check the ToolsRTM installation.")
  }
  list(lut = lut, spectra = spectra, wavelength = wavelength)
}
