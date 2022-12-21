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


leg_shape_info = function (Shape="rect",
                           Size=1,
                           Color=IPCCgrey50,
                           Label="A",
                           Cross=FALSE,
                           dy_icon=1,
                           dx_label=1,
                           height=10,
                           width=10,
                           WIP=FALSE,
                           margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) {

    nShape = length(Shape)
    nMax = max(c(length(Shape), length(Size),
                 length(Color), length(Label), length(Cross)))
    
    if (length(Shape) != nMax) {
        Shape = rep(Shape[1], nMax)
    }
    if (length(Size) != nMax) {
        Size = rep(Size[1], nMax)
    }
    if (length(Color) != nMax) {
        Color = rep(Color[1], nMax)
    }
    if (length(Label) != nMax) {
        Label = rep(Label[1], nMax)
    }
    if (length(Cross) != nMax) {
        Cross = rep(Cross[1], nMax)
    }

    limit = min(c(height, width))
    options(repr.plot.width=width, repr.plot.height=height)
    
    plot = ggplot() + theme_void() +
        coord_fixed(clip="off") + 
        theme(text=element_text(family="Helvetica"),
              plot.margin=margin)

    if (WIP) {
        plot = plot + 
            theme(panel.background=element_rect(fill='grey97'))
    }

    for (i in 1:nShape) {
        shape = Shape[i]
        size = Size[i]
        color = Color[i]
        label = Label[i]
        cross = Cross[i]
        
        if (shape == "rect") {
            plot = plot +
                annotate("rect",
                         xmin=0,
                         xmax=(size)*limit/10,
                         ymin=(-dy_icon*i)*limit/10,
                         ymax=(-dy_icon*i-size)*limit/10,
                         fill=color)
        }
        
        if (file.exists(shape)) {
            plot = plot +
                annotation_custom(svgparser::read_svg(shape),
                                  xmin=0,
                                  xmax=(size)*limit/10,
                                  ymin=(-dy_icon*i)*limit/10,
                                  ymax=(-dy_icon*i-size)*limit/10)
        }

        if (cross) {
            plot = plot +
                annotate("point", x=0, y=(-dy_icon*i)*limit/10,
                         shape=4, size=size, color="white")
        }

        if (!is.null(label)) {
            plot = plot +
                annotate('text',
                         x=(size+dx_label)*limit/10,
                         y=(-dy_icon*i-size/2)*limit/10,
                         label=label,
                         angle=0,
                         hjust=0, vjust=0.5,
                         size=3, color=IPCCgrey50)
        }
    }

    plot = plot +
        scale_x_continuous(limits=c(0, width),
                           expand=c(0, 0)) + 
        scale_y_continuous(limits=c(-height, 0),
                           expand=c(0, 0))
    

    return (plot)
}
