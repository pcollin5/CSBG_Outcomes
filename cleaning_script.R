
#### load packages ####

packages <- c("tidyverse", "tidycensus", "leaflet", "mapview", "DT", "sf",
              "knitr", "rmarkdown", "kableExtra", "RColorBrewer", "tigris",
              "directlabels", "officer", "flextable", "zoo", "directlabels", 
              "readxl", "lubridate", "censusxy")



lapply(packages, library, character.only = TRUE)

#### load data ####


fiscal2019 <- read_excel("fiscal2019.xlsx", 
                              col_types = c("text", "text", "numeric", 
                                                      "date", "skip", "text", "text", "text", 
                                                      "text", "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "text", "date", "text", "text", "text", 
                                                      "text", "text", "numeric", "numeric", 
                                                      "numeric", "text", "skip", "text", 
                                                      "date", "text", "numeric", "date", 
                                                      "date", "text", "numeric", "text", 
                                                      "text", "numeric"))


#### load color scheme ####


UETHDA_Colors <- c(`orange` = "#ff5100", 
                   `dark grey` = "#808285",
                   `black` = "#000000",
                   `white` = "#ffffff",
                   `dark blue` = "#0b5394",
                   `light grey` = "#cccccc",
                   `cyan` = "00ffff")

uethda_cols <- function(...){
  cols <- c(...)
  
  if(is.null(cols))
    return(UETHDA_Colors)
  
  UETHDA_Colors[cols]
}

UETHDA_palettes <- list(
  `main` = uethda_cols("orange", "dark grey", "black"),
  
  `cool` = uethda_cols("orange", "cyan", "dark blue")
)

uethda_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- UETHDA_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


scale_color_uethda <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- uethda_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("uethda_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_uethda <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- uethda_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("uethda_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}



#### geocode the addressess ####



fiscal2019_sf <- cxy_geocode(fiscal2019, street = "Address1", city = "City", state = "State", zip = "ZIP", class = "sf", output = "full", return = "geographies", vintage = 'Current_Current')



mapview(fiscal2019_sf, zcol = "QuestionSetDescription")
