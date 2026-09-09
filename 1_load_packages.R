##################
# load R packages
##################

# Installs any package that is missing, then attaches all of them.
# Safe to re-run: nothing is installed if it is already available.
#
# Notes:
#   - doMC is Unix/macOS only (it wraps fork), so it is skipped on Windows.
#   - library() is used rather than require() so that a genuinely missing
#     package stops the script here, instead of failing hundreds of lines
#     later inside an unrelated function.

pkgs <- c("ftsa", "LaplacesDemon", "flexmix", "psych", "easyCODA", "doMC",
          "MortalityLaws", "DescTools", "xtable", "tidyverse", "ggplot2",
          "RColorBrewer", "rlist", "hdftsa", "demography", "vars", "transport",
          "Compositional", "compositions", "dplyr", "HMDHFDplus", "zCompositions",
          "MCS", "tidyr", "grid")

# doMC does not exist for Windows
if (.Platform$OS.type == "windows") {
    pkgs <- setdiff(pkgs, "doMC")
}

# ---- install whatever is missing -------------------------------------------

missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
    message("Installing ", length(missing_pkgs), " missing package(s): ",
            paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs, dependencies = TRUE)
}

# ---- attach, and report anything that still will not load ------------------

failed <- character(0)
for (p in pkgs) {
    ok <- suppressWarnings(suppressPackageStartupMessages(
        require(p, character.only = TRUE, quietly = TRUE)))
    if (!ok) failed <- c(failed, p)
}

if (length(failed) > 0) {
    stop("The following package(s) could not be installed or loaded:\n  ",
         paste(failed, collapse = ", "),
         "\nIf a package is not on CRAN for your R version, install it from its ",
         "source repository before re-running this script.",
         call. = FALSE)
}

rm(pkgs, missing_pkgs, failed, ok, p)

