# Install dependencies for the Toronto Apartment Building Evaluation Dashboard

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing package:", pkg))
    install.packages(pkg)
  } else {
    message(paste("Package already installed:", pkg))
  }
}

# packages
c_packages <- c(
  "shiny",
  "tidyverse",
  "sf",
  "leaflet",
  "leaflet.extras",
  "RColorBrewer",
  "viridis",
  "gt",
  "scales",
  "stringr",
  "lubridate",
  "bslib",
  "hexbin",
  "DT"
)

for (pkg in c_packages) {
  install_if_missing(pkg)
}

#GitHub packages

#remotes
install_if_missing("remotes")

#ggradar
if (!requireNamespace("ggradar", quietly = TRUE)) {
  remotes::install_github("ricardo-bion/ggradar")
} else {
  message("already installed: ggradar")
}

#gtExtras
if (!requireNamespace("gtExtras", quietly = TRUE)) {
  remotes::install_github("jthomasmock/gtExtras")
} else {
  message("already installed: gtExtras")
}


