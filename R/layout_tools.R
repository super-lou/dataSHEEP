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


## 1. PERSONAL PLOT __________________________________________________
### 1.1. Void plot ___________________________________________________
# A plot completly blank
#' @title Void plot
#' @export
void = function () {
    plot = ggplot() + geom_blank(aes(1,1)) +
        theme(
            plot.background = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(), 
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank()
        )
    return (plot)
}

### 1.2. Contour void plot ___________________________________________
# A plot completly blank with a contour
#' @title Contour plot
#' @export
contour = function () {
    plot = ggplot() + geom_blank(aes(1,1)) +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(), 
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            plot.background=element_rect(fill=NA, color="#EC4899"),
            plot.margin=margin(t=0, r=0, b=0, l=0, unit="mm"))
    return (plot)
}

### 1.3. Circle ______________________________________________________
# Allow to draw circle in ggplot2 with a radius and a center position
#' @title Circle
#' @export
gg_circle = function(r, xc, yc, color="black", fill=NA, ...) {
    x = xc + r*cos(seq(0, pi, length.out=100))
    ymax = yc + r*sin(seq(0, pi, length.out=100))
    ymin = yc + r*sin(seq(0, -pi, length.out=100))
    annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color,
             fill=fill, ...)
}


## 2. PLOT MANAGEMENT ________________________________________________
### 2.1. Merge _______________________________________________________
#' @title Merge
#' @export
merge_panel = function (STOCK, NAME=NULL, addMargin=FALSE,
                        paper_size=c(10, 10)) {



    if (is.null(NAME)) {
        ID = 1:nrow(STOCK)
        NAME = STOCK$name
        
    } else {
        STOCKname = STOCK$name
        nSTOCKname = length(STOCK$name)
        ID = c()
        
        nrowNAME = nrow(NAME)
        ncolNAME = ncol(NAME)
        NAME = unlist(c(NAME))
        nNAME = nrowNAME*ncolNAME
        
        for (i in 1:nNAME) {
            IDplot = which(STOCKname == NAME[i])
            ID = c(ID, IDplot)
            # NAME = c(NAME, STOCK$name[IDplot])
        }
    }

    PLOT = STOCK$plot[ID[!is.na(ID)]]
    NAME = matrix(NAME, nrow=nrowNAME, ncol=ncolNAME)
    ID[!is.na(ID)] = 1:length(ID[!is.na(ID)])
    ID = matrix(ID, nrow=nrowNAME, ncol=ncolNAME)                

    if (addMargin) {
        ncolNAME = ncol(NAME)
        ID = rbind(rep(NA, times=ncolNAME), ID,
                   rep(NA, times=ncolNAME))
        NAME = rbind(rep("margin", times=ncolNAME), NAME,
                     rep("margin", times=ncolNAME))
        
        nrowNAME = nrow(NAME)
        ID = cbind(rep(NA, times=nrowNAME), ID,
                   rep(NA, times=nrowNAME))
        NAME = cbind(rep("margin", times=nrowNAME), NAME,
                     rep("margin", times=nrowNAME))
    }

    nrowNAME = nrow(NAME)
    ncolNAME = ncol(NAME)


    
    paper_size='A4'
    if (paper_size == 'A4') {
        width = 21
        height = 29.7
    } else if (is.vector(paper_size) & length(paper_size) > 1) {
        width = paper_size[1]
        height = paper_size[2]
    }
    
    Norm_ratio = height / (height - 2*margin_height - cm_height - leg_height - foot_height - info_height)

    void_height = height / Norm_ratio
    
    Hcut = NAME[, 2]
    heightLM = rep(0, times=nrowNAME)

    heightLM[Hcut == "info"] = info_height
    heightLM[Hcut == "cm"] = cm_height
    heightLM[Hcut == "leg"] = leg_height
    heightLM[Hcut == "void"] = void_height
    heightLM[Hcut == "foot"] = foot_height
    heightLM[Hcut == "margin"] = margin_height

    col_width = (width - 2*margin_height) / (ncolNAME - 2)
    
    Wcut = NAME[(nrowNAME-1),]
    widthLM = rep(col_width, times=ncolNAME)
    widthLM[Wcut == "margin"] = margin_height

    LM_inline = PLOT[as.vector(ID)]
    
    NAME_inline = as.vector(NAME)

    plot = grid.arrange(arrangeGrob(grobs=LM_inline,
                                    nrow=nrowNAME,
                                    ncol=ncolNAME,
                                    heights=heightLM,
                                    widths=widthLM,
                                    as.table=FALSE))

    
    
    return (plot)
}

### 2.2. Add plot ____________________________________________________
add_plot = function (STOCK, plot=NULL, name="",
                     height=NA, width=NA,
                     first=FALSE, last=FALSE,
                     overwrite_by_name=FALSE) {
    
    if (overwrite_by_name == FALSE | !any(which(STOCK$name == name))) {
        if (nrow(STOCK) == 0) {
            STOCK = tibble(name=name,
                          height=height, width=width,
                          first=first, last=last,
                          plot=NULL)
        } else {
            STOCK = bind_rows(STOCK, tibble(name=name,
                                          height=height, width=width,
                                          first=first, last=last,
                                          plot=NULL))
        }
        STOCK$plot[[nrow(STOCK)]] = plot

    } else {
        id = which(STOCK$name == name)
        STOCK$height[id] = height
        STOCK$width[id] = width
        STOCK$first[id] = first
        STOCK$last[id] = last
        STOCK$plot[[id]] = plot
    }
    return (STOCK)
}


## 3. NUMBER MANAGEMENT ______________________________________________
### 3.1. Number formatting ___________________________________________
# Returns the power of ten of the scientific expression of a value
#' @title Number formatting
#' @export
get_power = function (value) {

    if (length(value) > 1) {
        power = unlist(as.list(sapply(value, get_power),
                               recursive=TRUE,
                               use.names=FALSE))
    } else {
        if (!is.na(value)) {
            # Do not care about the sign
            value = abs(value)
            
            # If the value is greater than one
            if (value >= 1) {
                # The magnitude is the number of character of integer part
                # of the value minus one
                power = nchar(as.character(as.integer(value))) - 1
                # If value is zero
            } else if (value == 0) {
                # The power is zero
                power = 0
                # If the value is less than one
            } else {
                # Extract the decimal part
                dec = gsub('0.', '', as.character(value), fixed=TRUE)
                # Number of decimal with zero
                ndec = nchar(dec)
                # Number of decimal without zero
                nnum = nchar(as.character(as.numeric(dec)))
                # Compute the power of ten associated
                power = -(ndec - nnum + 1)
            }
        } else {
            power = NA
        }
    }
    return (power)
}

### 3.2. Pourcentage of variable _____________________________________
# Returns the value corresponding of a certain percentage of a
# data serie
#' @title Pourcentage of variable
#' @export
gpct = function (pct, L, min_lim=NULL, shift=FALSE) {

    # If no reference for the serie is given
    if (is.null(min_lim)) {
        # The minimum of the serie is computed
        minL = min(L, na.rm=TRUE)
        # If a reference is specified
    } else {
        # The reference is the minimum
        minL = min_lim
    }

    # Gets the max
    maxL = max(L, na.rm=TRUE)
    # And the span
    spanL = maxL - minL
    # Computes the value corresponding to the percentage
    xL = pct/100 * as.numeric(spanL)

    # If the value needs to be shift by its reference
    if (shift) {
        xL = xL + minL
    }
    return (xL)
}

round_label = function (labelRaw, direction="V", ncharLim=4) {
    if (direction == "V") {
        label2 = signif(labelRaw, 2)
        label2[label2 >= 0] = paste0(" ", label2[label2 >= 0])
        label1 = signif(labelRaw, 1)
        label1[label1 >= 0] = paste0(" ", label1[label1 >= 0])
        label = label2
        label[nchar(label2) > ncharLim] =
            label1[nchar(label2) > ncharLim]
    } else if (direction == "H") {
        label2 = signif(labelRaw, 2)
        label1 = signif(labelRaw, 1)
        nCharLabel2 = nchar(label2)
        nCharLabel2[nCharLabel2 >= 0] =
            nCharLabel2[nCharLabel2 >= 0] + 1
        label = label2
        label[nCharLabel2 > ncharLim] = label1[nCharLabel2 > ncharLim]
    }
    return (label)
}

is.wholenumber = function (X, tol=.Machine$double.eps^0.5) {
    res = abs(X - round(X)) < tol
    return (res)
}

chr2op = function (x) {
    res = eval(parse(text=x))
    return (res)
}

float2frac = function (X, den) {
    Frac = paste0(round(X * den), "/", den)
    evalFrac = sapply(X, chr2op)
    OK = is.wholenumber(evalFrac)
    Frac[OK] = evalFrac[OK]
    return (Frac)
}

## 4. LOADING ________________________________________________________
### 4.1. Shapefile loading ___________________________________________
#' @title Shapefiles loading
#' @description  Generates a list of shapefiles to draw a hydrological
#' map of the France
#' @param resources_path Path to the resources directory.
#' @param fr_shpdir Directory you want to use in ash\\resources_path\\
#' to get the France shapefile.
#' @param fr_shpname Name of the France shapefile.
#' @param bs_shpdir Directory you want to use in ash\\resources_path\\
#' to get the hydrological basin shapefile.
#' @param bs_shpname Name of the hydrological basin shapefile.
#' @param sbs_shpdir Directory you want to use in
#' ash\\resources_path\\ to get the hydrological sub-basin shapefile.
#' @param sbs_shpname Name of the hydrological sub-basin shapefile.
#' @param rv_shpdir Directory you want to use in ash\\resources_path\\
#' to get the hydrological network shapefile.
#' @param rv_shpname  Name of the hydrological network shapefile.
#' @param show_river Boolean to indicate if the shapefile of the
#' hydrological network will be charge because it is a heavy one and
#' that it slows down the entire process (default : TRUE)
#' @return A list of shapefiles converted as tibbles that can be plot
#' with 'geom_polygon' or 'geom_path'.
#' @export
load_shapefile = function (resources_path, data,
                           fr_shpdir, fr_shpname,
                           bs_shpdir, bs_shpname,
                           sbs_shpdir, sbs_shpname,
                           cbs_shpdir, cbs_shpname, cbs_coord,
                           rv_shpdir, rv_shpname,
                           river_selection=c('all'),
                           toleranceRel=10000) {

    Code = rle(data$Code)$value
    
    # Path for shapefile
    fr_shppath = file.path(resources_path, fr_shpdir, fr_shpname)
    bs_shppath = file.path(resources_path, bs_shpdir, bs_shpname)
    sbs_shppath = file.path(resources_path, sbs_shpdir, sbs_shpname)
    cbs_shppath = file.path(resources_path, cbs_shpdir, cbs_shpname)
    rv_shppath = file.path(resources_path, rv_shpdir, rv_shpname)

    
    # France
    france = st_read(fr_shppath)
    france = st_union(france)
    france = st_simplify(france,
                         preserveTopology=TRUE,
                         dTolerance=toleranceRel)
    france = st_transform(france, 2154)
    
    # Hydrological basin
    basin = st_read(bs_shppath)
    basin = st_simplify(basin,
                        preserveTopology=TRUE,
                        dTolerance=toleranceRel/2)
    basin = st_transform(basin, 2154)
    
    # Hydrological sub-basin
    subBasin = st_read(sbs_shppath)
    subBasin = st_simplify(subBasin,
                           preserveTopology=TRUE,
                           dTolerance=toleranceRel/2)
    subBasin = st_transform(subBasin, 2154)

    # Hydrological code bassin
    codeBasin_list = lapply(cbs_shppath, read_sf)
    codeBasin_list = lapply(codeBasin_list, st_transform, 2154)
    codeBasin = do.call(rbind, codeBasin_list)
    codeBasin = codeBasin[codeBasin$Code %in% Code,]
    codeBasin = st_simplify(codeBasin,
                            preserveTopology=TRUE,
                            dTolerance=toleranceRel/10)

    # If the river shapefile needs to be load
    if (!("none" %in% river_selection)) {
        # Hydrographic network
        river = st_read(rv_shppath)

        if ('all' %in% river_selection) {
            river = river[river$Classe == 1,]
        } else {
            river = river[grepl(paste(river_selection, collapse='|'),
                                river$NomEntiteH),]
        }
        river = st_simplify(river,
                            preserveTopology=TRUE,
                            dTolerance=toleranceRel/10)
        river = st_transform(river, 2154) 
    } else {
        river = NULL
    }

    return (list(france=france,
                 basin=basin,
                 subBasin=subBasin,
                 codeBasin=codeBasin,
                 river=river))
}

### 4.2. Logo loading ________________________________________________
#' @title Logo loading
#' @export
load_logo = function (resources_path, logo_dir, logo_to_show) {
    logo_path = c()
    nLogo = length(logo_to_show)
    for (i in 1:nLogo) { 
        logo_path = c(logo_path, file.path(resources_path,
                                           logo_dir,
                                           logo_to_show[i]))
        names(logo_path)[length(logo_path)] = names(logo_to_show)[i]
    }
    return (logo_path)
}

### 4.3. Other _______________________________________________________    
#' @title Split filename
#' @export
splitext = function(file) { # tools::file_ext
    ex = strsplit(basename(file), split="\\.")[[1]]
    res = list(name=ex[1], extension=ex[2])
    return (res)
}

#' @title Split path
#' @export
split_path = function (path) {
    if (dirname(path) %in% c(".", path)) return(basename(path))
    return(c(basename(path), split_path(dirname(path))))
}
