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


leg_shape_info = function (Shape=c("rect", "rect"),
                           Size=c(1, 1),
                           Color=c(IPCCgrey50, IPCCgrey50),
                           Label=c("A", "B"),
                           Cross=c(FALSE, FALSE),
                           plot_margin=margin(t=0, r=0,
                                              b=0, l=0, "mm")) {        
    dy = 1.6
    dTl = 0.4

    nShape = length(Shape)
    
    plot = ggplot() + theme_void() +
        coord_fixed(clip="off") + 
        theme(text=element_text(family="Helvetica"),
              plot.margin=plot_margin)

    for (i in 1:nShape) {
        shape = Shape[i]
        size = Size[i]
        color = Color[i]
        label = Label[i]
        cross = Cross[i]
        
        if (shape == "rect") {
            plot = plot +
                annotate("rect",
                         xmin=-size/2,
                         xmax=size/2,
                         ymin=-dy*i-size/2,
                         ymax=-dy*i+size/2,
                         fill=color)
        }
        
        if (cross) {
            plot = plot +
                annotate("point", x=0, y=-dy*i,
                         shape=4, size=size, color="white")
        }

        if (!is.null(label)) {
            plot = plot +
                annotate('text',
                         x=size/2+dTl,
                         y=-dy*i,
                         label=label,
                         angle=0,
                         hjust=0, vjust=0.5,
                         size=3, color=IPCCgrey50)
        }
    }

    plot = plot +
        scale_x_continuous(expand=c(0, 0)) + 
        scale_y_continuous(expand=c(0, 0))

    return (plot)
}
