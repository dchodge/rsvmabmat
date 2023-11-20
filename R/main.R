#Load all the necessary packages via pacman
if(!require(pacman)){install.packages("pacman")}

pacman::p_load(
    # utils + data
    "here", "tidyverse", "devtools", "ggdist", "patchwork", "readxl", "future", "furrr",
    "data.table", "hesim", "dampack", "dchodge"
)


#install_github("dchodge/rsvie")
pacman::p_load(rsvie)