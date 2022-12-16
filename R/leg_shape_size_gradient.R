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


leg_shape_size_gradient = function (shape="rect",
                                    Size=c(0.4, 0.6, 0.8, 1),
                                    color=IPCCgrey50,
                                    labelArrow=NULL,
                                    ssg_margin=margin(t=0, r=0,
                                                      b=0, l=0, "mm")) {        
    dx = 0.4
    dAl = 0.2
    dAr = 0.6
    dAup = 0.25
    dAp = 1
    Asize= 0.35
    dTl = 0.2
    dTup = 0.2
    

    nSize = length(Size)
    dX = seq(0, dx*(nSize-1), dx)
    dS = cumsum(Size) - Size/2
    X = dX + dS
    
    plot = ggplot() + theme_void() +
        coord_fixed(clip="off") + 
        theme(text=element_text(family="Helvetica"),
              plot.margin=ssg_margin)

    if (shape == "rect") {
        plot = plot +
            annotate("rect",
                     xmin=X-Size/2,
                     xmax=X+Size/2,
                     ymin=rep(0, nSize),
                     ymax=Size,
                     fill=color)
    }

    if (!is.null(labelArrow)) {
        plot = plot +
            
            annotate("segment",
                     x=min(X, na.rm=TRUE)-dAl,
                     xend=max(X, na.rm=TRUE)+dAr,
                     y=-dAup,
                     yend=-dAup,
                     color=IPCCgrey50, size=Asize,
                     arrow=arrow(length=unit(dAp, "mm"))) +
            
            annotate('text',
                     x=mean(X, na.rm=TRUE)+dTl, y=-dAup-dTup,
                     label=labelArrow,
                     angle=0,
                     hjust=0.5, vjust=1,
                     size=3, color=IPCCgrey50)
    }

    plot = plot +
        scale_x_continuous(expand=c(0, 0)) + 
        scale_y_continuous(expand=c(0, 0))
    

    return (plot)
}
