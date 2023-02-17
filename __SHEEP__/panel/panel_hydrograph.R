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


#' @title Hydrograph panel
#' @export
panel_hydrograph = function (QM_code, period=NULL, margin=NULL) {

    regimeHydro = find_regimeHydro(QM_code)
    id_regimeHydro = regimeHydro$id
    typology_regimeHydro = regimeHydro$typology
    regimeHydro = paste0(typology_regimeHydro, " ", id_regimeHydro)
    
    # Vector of month index
    monthNum = 1:12
    # Vector of month name abbreviation
    monthName = c("J", "F", "M", "A", "M", "J",
                  "J", "A", "S", "O", "N", "D")

    # Open a new plot with the personalise theme
    plot = ggplot() + theme_IPCC() +
        # Theme modification
        theme(
            panel.background=element_rect(fill="white"),
            panel.border=element_blank(),
            axis.text.x=element_text(margin=unit(c(0, 0, 0, 0), "mm"),
                                     vjust=1, hjust=0.5),
            axis.text.y=element_text(size=8),
            axis.ticks.x=element_blank(),
            axis.line.y=element_line(color=IPCCgrey85, size=0.3),
            plot.title=element_text(size=8, vjust=-1, 
                                    hjust=-0.01, color=IPCCgrey40),
            axis.title.y=element_blank()) +
        
        # Adds a title to the y axis
        ggtitle(bquote('QM'~'('*m^{3}*'.'*s^{-1}*')'~~.(regimeHydro)))
        # Y axis title
        # ylab()
    
    # If there is no margins specified
    if (is.null(margin)) {
        # Sets all margins to 0
        plot = plot + 
            theme(plot.margin=margin(t=0, r=0, b=0, l=0, unit="mm"))
    # Otherwise
    } else {
        # Sets margins to the given ones
        plot = plot + 
            theme(plot.margin=margin)
    }

    plot = plot +
        # Plots the bar
        geom_bar(aes(x=monthNum, y=QM_code), 
                 stat='identity',
                 fill=IPCCgrey67,
                 width=0.75, size=0.2) +
        # X axis
        scale_x_continuous(breaks=monthNum,
                           labels=monthName,
                           limits=c(0, max(monthNum)+0.5),
                           expand=c(0, 0)) + 
        # Y axis
        scale_y_continuous(limits=c(0, max(QM_code)),
                           n.breaks=4,
                           expand=c(0, 0))
    # Returns the plot
    return (plot)
} 
