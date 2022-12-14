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


leg_colorbar = function (min, max, Palette,
                         colorStep=256, include=FALSE,
                         label=NULL, asFrac=FALSE,
                         reverse=FALSE,
                         plot_margin=margin(t=0, r=0,
                                            b=0, l=0,
                                            "mm")) {

    colorBin = compute_colorBin(min, max,
                                Palette=Palette,
                                colorStep=colorStep,
                                include=include,
                                reverse=reverse)

    Palette = colorBin$Palette
    bin = colorBin$bin
    if (is.null(label)) {
        if (asFrac) {
            label = float2frac(bin, round(colorStep/2))
        } else {
            label = round_label(bin, direction="H", ncharLim=4)
        }
    }
    
    upBin = colorBin$upBin
    lowBin = colorBin$lowBin
    nBin = length(bin)-1
    dT = 0.05
    dL = 0.2
    
    maxBin = max(bin, na.rm=TRUE)
    minBin = min(bin, na.rm=TRUE)
    bin = (bin - minBin) / (maxBin - minBin)
    upBin = (upBin - minBin) / (maxBin - minBin)
    lowBin = (lowBin - minBin) / (maxBin - minBin)
    dBin = mean(diff(bin))

    bin = bin + seq(-dL/2, nBin*dL, dL)
    upBin = upBin + seq(0, (nBin-1)*dL, dL)
    lowBin = lowBin + seq(0, (nBin-1)*dL, dL)
    
    plot = ggplot() + theme_void() +
        coord_fixed(clip="off") + 
        theme(text=element_text(family="Helvetica"),
              plot.margin=plot_margin)

    plot = plot +
        annotate("rect",
                 xmin=lowBin,
                 xmax=upBin,
                 ymin=0.5-dBin/2, ymax=0.5+dBin/2,
                 fill=Palette)
    
    for (i in 1:(nBin+1)) {
        b = bin[i]
        plot = plot +
            annotate("line",
                     x=c(b, b),
                     y=c(0.5-dBin*2/3, 0.5+dBin*2/3),
                     linewidth=0.6, color=IPCCgrey85)
    }
    
    plot = plot +
        annotate("text", x=bin, y=rep(0.5-dBin*2/3-dT, nBin+1),
                 label=label, size=3.5,
                 hjust=0.5, vjust=1,
                 fontface="bold", color=IPCCgrey50)

    plot = plot +
        scale_x_continuous(expand=c(0, 0)) + 
        scale_y_continuous(expand=c(0, 0))
    

    return (plot)
}
