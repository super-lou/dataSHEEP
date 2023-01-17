# Copyright 2022 Louis Héraut (louis.heraut@inrae.fr)*1,
#                Éric Sauquet (eric.sauquet@inrae.fr)*1
#
# *1   INRAE, France
#
# This file is part of dataSheep R package.
#
# dataSheep R package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# dataSheep R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with dataSheep R package.
# If not, see <https://www.gnu.org/licenses/>.


## 1. MAP PANEL ______________________________________________________
# Generates a map plot of the tendancy of a hydrological variable
#' @title Mini map panel
#' @export
panel_mini_map = function (data, meta, Shapefiles,
                           codeLight=NULL, verbose=FALSE) {
    
    # Extract shapefiles
    france = Shapefiles$france
    basin = Shapefiles$basin
    subBasin = Shapefiles$subBasin
    codeBasin = Shapefiles$codeBasin
    river = Shapefiles$river

    sizefr = 0.35
    sizebs = 0.3
    sizecbs = 0.4
    sizerv = 0.2

    # Stores the coordonate system 
    cf = coord_fixed()
    # Makes it the default one to remove useless warning
    cf$default = TRUE

    # Open a new plot with the personalise theme
    map = ggplot() + theme_void() +
        # Fixed coordinate system (remove useless warning)
        cf +
        
        theme(plot.margin=margin(t=0, r=0, b=0, l=0,
                                 unit="mm")) +
        
        # Plot the background of France
        geom_sf(data=france,
                color=NA,
                fill=IPCCgrey97)
    
    # If the river shapefile exists
    if (!is.null(river)) {
        # Plot the river
        map = map +
            geom_sf(data=river,
                    color=IPCCgrey85,
                    fill=NA,
                    size=sizerv)
    }
    
    map = map +
        # Plot the hydrological basin
        geom_sf(data=basin,
                color=IPCCgrey67,
                fill=NA,
                size=sizebs)
    
    map = map +
        # Plot the countour of France
        geom_sf(data=france,
                color=IPCCgrey40,
                fill=NA,
                size=sizefr)
    
    # Leaves space around the France
    xlim = c(90000, 1250000)
    ylim = c(6040000, 7120000)
    
    # Same but with less graduation and smaller size
    xmin = gpct(2, xlim, shift=TRUE)
    xint = c(0, 250*1E3)
    ymin = gpct(1, ylim, shift=TRUE)
    ymax = ymin + gpct(3, ylim)
    size = 2
    sizekm = 1.5

    map = map +
        # Adds the base line of the scale
        geom_line(aes(x=c(xmin, max(xint)+xmin),
                      y=c(ymin, ymin)),
                  color=IPCCgrey40, size=0.2) +
        # Adds the 'km' unit
        annotate("text",
                 x=max(xint)+xmin+gpct(1, xlim), y=ymin,
                 vjust=0, hjust=0, label="km",
                 color=IPCCgrey40, size=sizekm)
    # For all graduations
    for (x in xint) {
        map = map +
            # Draws the tick
            annotate("segment",
                     x=x+xmin, xend=x+xmin, y=ymin, yend=ymax,
                     color=IPCCgrey40, size=0.2) +
            # Adds the value
            annotate("text",
                     x=x+xmin, y=ymax+gpct(0.5, ylim),
                     vjust=0, hjust=0.5, label=x/1E3,
                     color=IPCCgrey40, size=size)
    }
    
    map = map +
        # Allows to crop shapefile without graphical problem
        coord_sf(xlim=xlim, ylim=ylim,
                 expand=FALSE)
    
    
    Code = levels(factor(meta$Code))
    Lon = meta$L93X_m_BH[match(meta$Code, Code)]           
    Lat = meta$L93Y_m_BH[match(meta$Code, Code)]
    
    # Creates a tibble to stores all the data to plot
    plot_map = tibble(Lon=Lon, Lat=Lat, Code=Code)
    
    if (!is.null(codeLight)) {
        # Extract data of all stations not to highlight
        plot_map_codeNo = plot_map[plot_map$Code != codeLight,]
        # Extract data of the station to highlight
        plot_map_code = plot_map[plot_map$Code == codeLight,]
        # Plots only the localisation
        map = map +
            # For all stations not to highlight
            geom_point(data=plot_map_codeNo,
                       aes(x=Lon, y=Lat),
                       shape=21, size=0.5, stroke=0.5,
                       color=IPCCgrey50,
                       fill=IPCCgrey50) +
            # For the station to highlight
            geom_point(data=plot_map_code,
                       aes(x=Lon, y=Lat),
                       shape=21, size=2, stroke=0.5,
                       color=IPCCgrey97,
                       fill=INRAEcyan)
    } else {
        # Plots only the localisation
        map = map +
            # For all stations not to highlight
            geom_point(data=plot_map,
                       aes(x=Lon, y=Lat),
                       shape=21, size=0.5, stroke=0.5,
                       color=codeAll_color,
                       fill=IPCCgrey50)
    }    
    return (map)
}

