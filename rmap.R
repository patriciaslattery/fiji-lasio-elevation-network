# This R script was written by James Dorey, starting on the 23rd of October 2023.
# The script is intended to explore some of the Fijian Hylaeus data for the first revision of the 
# group
# For queries, please feel free to contact James Dorey at jbdorey@me.com



#### 0.0 Script preparation ####
# Install renv, IF NEEDED
#install.packages("renv")

##### 0.1 Working directory ####
# Choose the path to the root folder in which all other folders can be found (or made by dirMaker)
RootPath <- "C:/Users/patri/OneDrive/Documents/Uni/P8_FijianHomalictusElevation/Figures/Rmap"

# Set the working directory
setwd(RootPath)

##### 0.2 Install packages (if needed) #####
#renv::activate(project = RootPath)
# Install only those packages that are not already present in your system
# Choose packages that need to be installed 
# You may need to install gdal on your computer. This can be done on mac by using
# Homebrew in the terminal and the command "brew install gdal"
list.of.packages <- c("tidyr",             #  Part of the tidyverse
                      "magrittr",          # to use pipes
                      "ggplot2",           #  Creates many easthetic plots
                      "dplyr",             #  Part of the tidyverse
                      "tibble",            # To use tibbles
                      "stringr",           #  Part of the tidyverse — works with text strings
                      "lubridate",         #  Part of the tidyverse — works with dates
                      "tidyselect",        #  Part of the tidyverse
                      "rnaturalearth",     #  Global vector map data 
                      "rnaturalearthdata", #  To access the above global map data
                      "readr",             #  Part of the tidyverse — reads files (e.g., .csv)
                      "openxlsx",
                      "BeeBDC",
                      "ggfortify",
                      "geodata",
                      "ggrepel",
                      "ggspatial")         #  Makes ggplot2 create north arrows or scale bars
# Install sf seperately
#renv::install(c("sf"), type = "binary", rebuild = FALSE)
# List the new (not installed) packages and then if there are any, install them.
#renv::install(packages = c(list.of.packages), 
              #rebuild = FALSE) # try changing to TRUE if you're having package troubles


##### 0.3 Load packages ####
# Load all packages from the list specified above
lapply(c(list.of.packages, "sf"), 
       library, character.only = TRUE)

# Save a snapshot of the environment
#renv::snapshot()


#### 1.0 Read data ####
##### 1.1 Locations ####
# Read in the location data 
locData <- openxlsx::read.xlsx("2024_JBDorey_Fijian_Lasioglossum_CombinedData.xlsx",
                               sheet = "2021_Fijian_Homalictus_ALL")
    # This was done and then manually checked for undescribed species — now 2024 version
  #   # Correct the taxonomy
  # BeeBDC::harmoniseR(taxonomy = BeeBDC::beesTaxonomy(),
  #                    speciesColumn = "Species.name",
  #                    path = RootPath)

# Rename some columns
locData <- locData %>%
  dplyr::rename(
    decimalLatitude = Latitude,
    decimalLongitude = Longitude
  )


##### 1.2 Location names ####
islandLabels <- readr::read_csv("islandLabels.csv") %>%
  sf::st_as_sf(.,
               coords = c("decimalLongitude", "decimalLatitude"),
               crs = sf::st_crs("WGS84")) %>%
  sf::st_transform(sf::st_crs("EPSG:3460")) %>%
  dplyr::tibble() %>%
  dplyr::mutate(geometry2 = geometry %>% as.character()) %>%
  tidyr::separate_wider_delim(geometry2,
                               delim = " ",
                              names = c("decimalLatitude", "decimalLongitude")) %>% 
  dplyr::mutate(decimalLatitude = decimalLatitude %>%
                  stringr::str_remove_all("c\\("),
                decimalLongitude = decimalLongitude %>%
                  stringr::str_remove_all("\\)")%>% as.numeric() %>% round(0),
                decimalLatitude = decimalLatitude %>%
                  stringr::str_remove_all(",") %>% as.numeric() %>% round(0))
  


#### 2.0 Maps ####
##### 2.1 Map data ####
# Download world map using rnaturalearth packages
# shift coordinates to recenter worldmap
worldmap <- ggplot2::map_data("world", wrap = c(0, 360))
# Download the Fijian DEM rasters — one for each side of the dateline and convert to EPSG:3460
# — Fiji 1986
FijiRastWest <- geodata::elevation_3s(country='FJI', 
                                      path = RootPath,
                                      mask = TRUE,
                                      lat = c( -16),
                                      lon = c(180),
                                      res = "0.5") %>%
  terra::project(., terra::crs("EPSG:3460"),
                 gdal = TRUE,   method = "near",
                 threads = TRUE, res = 90)
FijiRastEast <- geodata::elevation_3s(country='FJI', 
                                      path = RootPath,
                                      mask = TRUE,
                                      lat = c( -16),
                                      lon = c(-180),
                                      res = "0.5") %>%
  terra::project(., terra::crs("EPSG:3460"),
                 gdal = TRUE,   method = "near",
                 threads = TRUE, res = 90)
# Merge the fiji map halves
FijiMap <- terra::merge(FijiRastWest, FijiRastEast) 
  terra::classify(c(0,200,400,600,800,1000,1200,1400),
                  right = FALSE)


# Set up limits
yLimInput = c(3591385+200000, 4062964+30000)
xLimInput = c(1871432- 70000, 2298672 - 100000.0)
# Reproject the Fiji 1986 extent to WGS84 for the inset
WGS84extent <- sf::st_bbox(c(xmin = xLimInput[1], xmax = xLimInput[2], 
                             ymin = yLimInput[1], ymax = yLimInput[2]),
                           crs = terra::crs("EPSG:3460")) %>%
  sf::st_as_sfc() %>%
  sf::st_transform(., crs = terra::crs("EPSG:4326")) %>%
  st_bbox()
# Extract the limits and format
WGS84extent2 <- tibble::tibble(
  point = c("xmin", "xmax", "ymin", "ymax"),
  coords = c(WGS84extent[1], WGS84extent[3],WGS84extent[2], WGS84extent[4]) %>%
    as.numeric()) %>%
  dplyr::mutate(coords = dplyr::if_else(coords < 0 & 
                                          stringr::str_detect(point, "^x"),
                                        coords + 360,
                                        coords)  )

##### 2.2 Points ####
# Turn loc data into points
# For Fiji make a unique set of points that are transformed
points_FJ <- sf::st_as_sf(locData %>%
                            # Remove coordinateless occurrences 
                            tidyr::drop_na(decimalLatitude),
                          coords = c("decimalLongitude", "decimalLatitude"),
                          crs = sf::st_crs("WGS84")) %>%
  sf::st_transform(sf::st_crs("EPSG:3460"))
# For the rest of the data use WGS
points <- sf::st_as_sf(locData %>%
                         # Remove coordinateless occurrences 
                         tidyr::drop_na(decimalLatitude),
                       coords = c("decimalLongitude", "decimalLatitude"),
                       crs = sf::st_crs("WGS84"))


##### 2.3 Fiji ####
# Make one Fiji mapping function
FijiMapR <- function(pointsIn, names, pointSize,pointColour = "white",
                     xLimInput, yLimInput, country, mapIn, fillIn, borderCol,
                     yBreaks, xBreaks, pointAlpha = 0.9, nameSuffix = "", pointStroke = 1,
                     pointShape = 21, pointFill = "black",
                     # fake points
                     fpointSize = 3,
                     fpointColour = "red",
                     fpointFill = "grey",
                     posInside = c(0.9,0.9),
                     fpointAlpha = 0.9){
  # Make the map
  PointMap <- ggplot() +
    tidyterra::geom_spatraster(data = mapIn, na.rm = TRUE,
                               aes(fill = srtm_72_16)) +
    # Change map colour scheme
    ggplot2::scale_fill_manual(values = c("#87B975","#629448","#378437", "#066938", 
                                          "#2A3B8E", "#1E76BB",  "#79B7E1"),
                               labels = c("1–199","200–399","400–599", "600–799", 
                                          "800–999", "1,000–1,199",  "1,200–1,399"),
                               na.translate = F,
                               na.value = NA,
                               guide = guide_legend(reverse = TRUE)) +
    labs(fill = "Elevation (m asl)") + 
    # GEOREFERENCED points
    geom_sf(data = pointsIn,  colour = fpointColour,
            alpha = fpointAlpha, size = fpointSize, shape = pointShape, fill = fpointFill,
            stroke = pointStroke) +
    # POINTS
    geom_sf(data = pointsIn,  colour = pointColour,
            #mapping = aes(x = "decimalLongitude", y = "decimalLatitude"),
            alpha = pointAlpha, size = pointSize, shape = pointShape, fill = pointFill,
            stroke = pointStroke) +
    # scale_color_manual("Wolbachia status", values = c(colWol,colPts),
    #                    breaks = c("Infected", "Unknown"),
    #                    labels = c("Infected", "Unknown")) +
    # scale_shape_manual(values = c(pchPts, pchWol)) +
    # Map formatting
    # Add in the map's north arrow
    theme(panel.grid.major = element_line(color = gray(.1, alpha = 0.1), 
                                          linetype = "dashed", linewidth = 0.5), # Add grid lines
          panel.border = element_rect(color = borderCol, 
                                      linetype = "solid", linewidth = 2,
                                      fill = NA), # add panel border
          panel.background = element_rect(fill = "aliceblue"),
          plot.title = element_text(face = "italic"),
          # legend background colour
          legend.key = element_rect(fill = c("#79B7E1", "#1E76BB", "#2A3B8E", 
                                             "#066938", "#378437", "#629448",
                                             "#87B975")),
          legend.position = "inside",
          legend.position.inside = posInside,
          legend.box.background = element_rect(colour = "black", fill = NA,
                                      linetype = "solid", linewidth = 1)
          ) +
    # Legend order
    guides(shape = guide_legend(order = 1), 
           col = guide_legend(order = 2, override.aes = list(alpha = 1))) +
    # Add in X and Y labels; 1 or ?element_rect-1
    xlab("") + ylab("")+
    # breaks
    scale_y_continuous(breaks = seq(yBreaks[[1]], yBreaks[[2]], by = yBreaks[[3]]),
                       limits = c(yLimInput[[1]], yLimInput[[2]])) +
    scale_x_continuous(breaks = seq(xBreaks[[1]], xBreaks[[2]], by = xBreaks[[3]]),
                       limits = c(xLimInput[[1]], xLimInput[[2]])) +
    # Add in the title
    ggtitle( paste0(names,nameSuffix)) 
}# END function

  ###### a. set up values ####
# Set up limits
yLimInput = c(3591385+100000, 4062964+60000)
xLimInput = c(1871432- 70000, 2298672 - 0)
# Reproject the Fiji 1986 extent to WGS84 for the inset
WGS84extent <- sf::st_bbox(c(xmin = xLimInput[1], xmax = xLimInput[2], 
                             ymin = yLimInput[1], ymax = yLimInput[2]),
                           crs = terra::crs("EPSG:3460")) %>%
  sf::st_as_sfc() %>%
  sf::st_transform(., crs = terra::crs("EPSG:4326")) %>%
  st_bbox()
# Set up function inputs
FijiCol <- "#21ABA5"
pointSize = 2.7
pointColour = "orange"
pointFill = "black"
pointStroke = 0.4
pointShape = 21
# Min, max, breaks
xBreaks = c(-180, 180, 1)
yBreaks = c(-20, -16, 1)
###### b. Make map ####
(LasioglossumMap <- FijiMapR(pointsIn = points_FJ,
                        names = "",
                        pointSize = pointSize,
                          # Border
                        pointColour = pointColour,
                        fpointColour = pointColour,
                          # point fill
                        pointFill = pointFill,
                        xLimInput = c(xLimInput[[1]], xLimInput[[2]]),
                        yLimInput = c(yLimInput[[1]], yLimInput[[2]]),
                        country = "Fiji", mapIn = FijiMap,
                        borderCol = FijiCol, pointStroke = pointStroke,
                        yBreaks = yBreaks, xBreaks = xBreaks,
                        posInside = c(0.9, 0.8)) + 
    # Nudge labels
   ggrepel::geom_label_repel(data = islandLabels %>%
                               dplyr::mutate(Island = stringr::str_replace(Island, 
                                                                           "Southern Lau Group",
                                                                  "Southern Lau/nGroup")),
                             aes(y = decimalLongitude, 
                                                     x = decimalLatitude, 
                                                     label = Island),
                            fontface = "bold", 
                              # VL, VanLev, Tav, Kad, Lau
                            nudge_x = c(10000, 10000, 10000, 10000, 10000, 10000), 
                            nudge_y = c(70000, 50000, 50000, 30000, 50000, 50000),
                            box.padding = 0.5,
                            fill = "#FFDD95",
                            color = "black",
                            size = 4,
                            label.size = 0.3,
                            segment.curvature = 0.2) +
   ggplot2::xlab("Longitude") +  ggplot2::ylab("Latitude")
 )

 # Save the plot
ggplot2::ggsave(filename = "output_figures/lasioMaps.jpg", device = "jpg", plot = LasioglossumMap,
                dpi = 300, height = 7.8, width = 8.7, units = "in")
   








