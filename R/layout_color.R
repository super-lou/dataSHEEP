# \\\
# Copyright 2021-2022 Louis Héraut*1,
#                     Éric Sauquet*2,
#                     Valentin Mansanarez
#
# *1   INRAE, France
#      louis.heraut@inrae.fr
# *2   INRAE, France
#      eric.sauquet@inrae.fr
#
# This file is part of ash R toolbox.
#
# Ash R toolbox is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# Ash R toolbox is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ash R toolbox.
# If not, see <https://www.gnu.org/licenses/>.
# ///
#
#
# R/plotting/layout.R
#
# Regroups general parameters about plotting like the theme used ang
# color management. It mainly deals with the calling to specific
# plotting functions and the organisation of each plot for the
# generation of the PDF.


## 1. PERSONALISATION ________________________________________________
### 1.1. Personal theme ______________________________________________
#' @title Ggplot2 theme ash
#' @export
theme_ash = function () {
    theme =
        theme(
            # White background
            panel.background=element_rect(fill='grey97'),
            # Font
            text=element_text(family='sans'),
            # Border of plot
            panel.border = element_rect(color="grey80",
                                        fill=NA,
                                        size=0.7),
            # Grid
            panel.grid.major.x=element_blank(),
            panel.grid.major.y=element_blank(),
            panel.grid.minor.x=element_blank(),
            panel.grid.minor.y=element_blank(),
            # Ticks marker
            axis.ticks.x=element_line(color='grey75', size=0.3),
            axis.ticks.y=element_line(color='grey75', size=0.3),
            # Ticks label
            axis.text.x=element_text(color='grey40'),
            axis.text.y=element_text(color='grey40'),
            # Ticks length
            axis.ticks.length=unit(1.5, 'mm'),
            # Ticks minor
            ggh4x.axis.ticks.length.minor=rel(0.5),
            # Title
            plot.title=element_blank(),
            # Axis title
            axis.title.x=element_blank(),
            axis.title.y=element_text(size=9, vjust=1.2, 
                                      hjust=0.5, color='grey20'),
            # Axis line
            axis.line.x=element_blank(),
            axis.line.y=element_blank()
            )
    return (theme)
}

### 1.2. Color palette _______________________________________________
#' @title Palette ground
#' @export
Palette_ground = function () {
    palette = c("#543005",
                "#8c510a",
                "#bf812d",
                "#dfc27d",
                "#f6e8c3",
                "#c7eae5",
                "#80cdc1",
                "#35978f",
                "#01665e",
                "#003c30")
    return (palette)
}

#' @title Color event
#' @export
get_colorEvent = function () {
    colorEvent = c("#423089", "#9ed6e3", "#9dc544", "#ed6e6c")
    names(colorEvent) = c("Crue", "Crue Nivale", "Moyennes Eaux", "Étiage")
    return(colorEvent)
}

#' @title Text color event
#' @export
get_colorTextEvent = function () {
    colorTextEvent = c("#9687d5", "#d8eff4", "#cee2a2", "#f6b6b5")
    names(colorTextEvent) = c("Crue", "Crue Nivale", "Moyennes Eaux", "Étiage")
    return(colorTextEvent)
}



#' @title Switch color label
#' @export
switch_colorLabel = function (color) {
    #switch 12% https://mdigi.tools/darken-color/#f6e8c3
    if (color == "#F6E8C3") {
        newColor = "#efd695"
        
    } else if (color == "#C7EAE5") {
        newColor = "#a1dcd3"
        
    } else {
        newColor = color
    }
    return (newColor)
}

#' @title Get reverse
#' @export
get_reverse = function (var) {
    # gets the color corresponding to the mean trend
    reverse = FALSE
    if (grepl('^tFIN', var) | grepl('^t[_]', var) | grepl('^v', var)) {
        reverse = TRUE
    }
    return (reverse)
}







## 1. COLOR MANAGEMENT
### 1.1. Color on colorbar ___________________________________________
#' @title Compute color bin
#' @export
compute_colorBin = function (min, max, Palette, colorStep=256,
                             reverse=FALSE) {

    # Gets the number of discrete colors in the palette
    nSample = length(Palette)

    if (reverse) {
        Palette = rev(Palette)
    }
    # Recreates a continuous color palette
    PaletteColors = colorRampPalette(Palette)(colorStep)

    # Computes the absolute max
    maxAbs = max(abs(max), abs(min))

    bin = seq(-maxAbs, maxAbs, length.out=colorStep-1)
    upBin = c(bin, Inf)
    lowBin = c(-Inf, bin)

    res = list(Palette=PaletteColors, bin=bin, upBin=upBin, lowBin=lowBin)
    return (res)
}

#' @title Compute color
#' @export
compute_color = function (value, min, max, Palette, colorStep=256, reverse=FALSE) {

    # If the value is a NA return NA color
    if (is.na(value)) {
        return (NA)
    }
    
    res = compute_colorBin(min=min, max=max, Palette=Palette,
                           colorStep=colorStep, reverse=reverse)
    upBin = res$upBin
    lowBin = res$lowBin
    PaletteColors = res$Palette

    if (value > 0) {
        id = which(value <= upBin & value > lowBin)
    } else {
        id = which(value <= upBin & value > lowBin)
    }
    color = PaletteColors[id]
    return(color)
}

# compute_color(-51, -50, 40, Palette, colorStep=10)

#' @title Get color
#' @export
get_color = function (value, min, max, Palette, colorStep=256, reverse=FALSE, noneColor='black') {
    
    color = sapply(value, compute_color,
                   min=min,
                   max=max,
                   Palette=Palette,
                   colorStep=colorStep,
                   reverse=reverse)
    
    color[is.na(color)] = noneColor    
    return(color)
}


### 1.3. Palette tester ______________________________________________
# Allows to display the current personal palette
#' @title Palette tester
#' @export
palette_tester = function (Palette, colorStep=256) {

    outdir = 'palette'
    if (!(file.exists(outdir))) {
        dir.create(outdir)
    }

    # An arbitrary x vector
    X = 1:colorStep
    # All the same arbitrary y position to create a colorbar
    Y = rep(0, times=colorStep)

    # Recreates a continuous color palette
    Palette = colorRampPalette(Palette)(colorStep)

    # Open a void plot
    p = ggplot() + theme_void()

    for (x in X) {
        # Plot the palette
        p = p +
            annotate("segment",
                     x=x, xend=x,
                     y=0, yend=1,
                     color=Palette[x], size=1)
    }

    p = p +
        scale_x_continuous(limits=c(0, colorStep),
                           expand=c(0, 0)) +
        
        scale_y_continuous(limits=c(0, 1),
                           expand=c(0, 0))

    # Saves the plot
    outname = deparse(substitute(Palette))
    
    ggsave(plot=p,
           path=outdir,
           filename=paste(outname, '.pdf', sep=''),
           width=10, height=10, units='cm', dpi=100)

    ggsave(plot=p,
           path=outdir,
           filename=paste(outname, '.png', sep=''),
           width=10, height=10, units='cm', dpi=300)
}


#' @title Get palette
#' @export
get_palette = function (Palette, colorStep=256) {
    
    # Gets the number of discrete colors in the palette
    nSample = length(Palette)
    # Recreates a continuous color palette
    Palette = colorRampPalette(Palette)(colorStep)

    return (Palette)
}


