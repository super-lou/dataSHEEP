% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{load_shapefile}
\alias{load_shapefile}
\title{Shapefiles loading}
\usage{
load_shapefile(
  computer_shp_path,
  Code = NULL,
  france_shp_path = NULL,
  bassinHydro_shp_path = NULL,
  regionHydro_shp_path = NULL,
  secteurHydro_shp_path = NULL,
  entiteHydro_shp_path = NULL,
  entitePiezo_shp_path = NULL,
  river_shp_path = NULL,
  river_class = NULL,
  river_length = NULL,
  river_selection = NULL,
  toleranceRel = 10000
)
}
\arguments{
\item{resources_path}{Path to the resources directory.}

\item{france_dir}{Directory you want to use in ash\\resources_path\\
to get the France shapefile.}

\item{france_file}{Name of the France shapefile.}

\item{bassinHydro_dir}{Directory you want to use in ash\\resources_path\\
to get the hydrological basin shapefile.}

\item{bassinHydro_file}{Name of the hydrological basin shapefile.}

\item{regionHydro_dir}{Directory you want to use in
ash\\resources_path\\ to get the hydrological sub-basin shapefile.}

\item{regionHydro_file}{Name of the hydrological sub-basin shapefile.}

\item{river_dir}{Directory you want to use in ash\\resources_path\\
to get the hydrological network shapefile.}

\item{river_file}{Name of the hydrological network shapefile.}

\item{show_river}{Boolean to indicate if the shapefile of the
hydrological network will be charge because it is a heavy one and
that it slows down the entire process (default : TRUE)}
}
\value{
A list of shapefiles converted as tibbles that can be plot
with 'geom_polygon' or 'geom_path'.
}
\description{
Generates a list of shapefiles to draw a hydrological
map of the France
}
