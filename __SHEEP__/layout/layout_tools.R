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
return_to_sheepfold = function (flock,
                                page_margin=c(t=0, r=0, b=0, l=0),
                                paper_size=NULL,
                                hjust=0, vjust=1,
                                verbose=FALSE) {

    SHEEP = flock$sheep
    PLAN = flock$plan

    if (!is.null(PLAN)) {
        SHEEPid = SHEEP$id
        nSHEEPid = length(SHEEP$id)
        ID = c()
        nrowPLAN = nrow(PLAN)
        ncolPLAN = ncol(PLAN)
        PLAN = as.vector(PLAN)
        nPLAN = nrowPLAN*ncolPLAN
        ID = match(PLAN, SHEEP$id)

        PLOT = SHEEP$plot[ID[!is.na(ID)]]
        PLAN = matrix(PLAN, nrow=nrowPLAN, ncol=ncolPLAN)
        ID = matrix(ID, nrow=nrowPLAN, ncol=ncolPLAN)

        ncolPLAN = ncol(PLAN)

        rowFoot = which(PLAN[, 1] == "foot")

        if (!identical(rowFoot, integer(0))) {
            ID = rbind(rep(NA, times=ncolPLAN),
                       ID[1:(rowFoot-1),, drop=FALSE],
                       rep(NA, times=ncolPLAN),
                       ID[rowFoot:nrowPLAN,, drop=FALSE])
            PLAN = rbind(rep("tjust", times=ncolPLAN),
                         PLAN[1:(rowFoot-1),, drop=FALSE],
                         rep("bjust", times=ncolPLAN),
                         PLAN[rowFoot:nrowPLAN,, drop=FALSE])
        }
        
        nrowPLAN = nrow(PLAN)
        ID = cbind(rep(NA, times=nrowPLAN), ID,
                   rep(NA, times=nrowPLAN))
        PLAN = cbind(rep("ljust", times=nrowPLAN), PLAN,
                     rep("rjust", times=nrowPLAN))

        ncolPLAN = ncol(PLAN)
        ID = rbind(rep(NA, times=ncolPLAN), ID,
                   rep(NA, times=ncolPLAN))
        PLAN = rbind(rep("tmargin", times=ncolPLAN), PLAN,
                     rep("bmargin", times=ncolPLAN))
        
        nrowPLAN = nrow(PLAN)
        ID = cbind(rep(NA, times=nrowPLAN), ID,
                   rep(NA, times=nrowPLAN))
        PLAN = cbind(rep("lmargin", times=nrowPLAN), PLAN,
                     rep("rmargin", times=nrowPLAN))

        nrowPLAN = nrow(PLAN)
        ncolPLAN = ncol(PLAN)

    } else {
        ID = 1:nrow(SHEEP)
        PLAN = SHEEP$id
        nrowPLAN = 1
        ncolPLAN = length(PLAN)
        ID = matrix(ID, nrow=nrowPLAN, ncol=ncolPLAN)
        PLAN = matrix(PLAN, nrow=nrowPLAN, ncol=ncolPLAN)
    }

    if (!is.null(paper_size)) {
        
        HEIGHT = SHEEP$height[match(PLAN, SHEEP$id)]
        HEIGHT = matrix(HEIGHT, nrow=nrowPLAN, ncol=ncolPLAN)
        WIDTH = SHEEP$width[match(PLAN, SHEEP$id)]
        WIDTH = matrix(WIDTH, nrow=nrowPLAN, ncol=ncolPLAN)

        if (verbose) {
            print("RESUME")
            print(ID)
            print(PLAN)
            print(HEIGHT)
            print(WIDTH)
        }

        if (paper_size == 'A4') {
            paperWidth = 21
            paperHeight = 29.7
        } else if (is.vector(paper_size) & length(paper_size) > 1) {
            paperWidth = paper_size[1]
            paperHeight = paper_size[2]
        }

        sumHeight = apply(HEIGHT, 2, sum, na.rm=TRUE)
        if (all(sumHeight == 0)) {
            maxHeight = paperHeight
            HEIGHT[HEIGHT == 0] = maxHeight
            idMaxHeight = 3
        } else {
            maxHeight = max(sumHeight, na.rm=TRUE)
            idMaxHeight = which(sumHeight == maxHeight)[1]
            maxHeight = maxHeight + page_margin["t"] + page_margin["b"]
        }
        ratioHeight = paperHeight / (paperHeight - maxHeight)
        tjust_height = paperHeight * (1-vjust) / ratioHeight
        bjust_height = paperHeight * vjust / ratioHeight

        PLANcut = PLAN[, idMaxHeight]
        heights = HEIGHT[, idMaxHeight]

        res = rle(PLANcut)
        REPtimes = res$lengths
        REPid = res$values
        for (i in 1:length(REPtimes)) {
            if (REPtimes[i] > 1) {
                heights[PLANcut == REPid[i]] =
                    heights[PLANcut == REPid[i]][1] / REPtimes[i]
            }
        }    
        heights[PLANcut == "tjust"] = tjust_height
        heights[PLANcut == "bjust"] = bjust_height
        heights[PLANcut == "tmargin"] = page_margin["t"]
        heights[PLANcut == "bmargin"] = page_margin["b"]

        if (verbose) {
            print("HEIGHT")
            print(heights)
            print(sum(heights))
        }

        sumWidth = apply(WIDTH, 1, sum, na.rm=TRUE)
        if (all(sumWidth == 0)) {
            maxWidth = paperWidth
            WIDTH[WIDTH == 0] = maxWidth
            idMaxWidth = 3
        } else {
            maxWidth = max(sumWidth, na.rm=TRUE)
            idMaxWidth = which(sumWidth == maxWidth)[1]
            maxWidth = maxWidth + page_margin["l"] + page_margin["r"]
        }
        ratioWidth = paperWidth / (paperWidth - maxWidth)
        ljust_width = paperWidth * hjust / ratioWidth
        rjust_width = paperWidth * (1-hjust) / ratioWidth

        PLANcut = PLAN[idMaxWidth,]
        widths = WIDTH[idMaxWidth,]

        res = rle(PLANcut)
        REPtimes = res$lengths
        REPid = res$values
        for (i in 1:length(REPtimes)) {
            if (REPtimes[i] > 1) {
                widths[PLANcut == REPid[i]] =
                    widths[PLANcut == REPid[i]][1] / REPtimes[i]
            }
        }
        widths[PLANcut == "ljust"] = ljust_width
        widths[PLANcut == "rjust"] = rjust_width
        widths[PLANcut == "lmargin"] = page_margin["l"]
        widths[PLANcut == "rmargin"] = page_margin["r"]

        if (verbose) {
            print("WIDTH")
            print(widths)
            print(sum(widths))
        }

    } else {
        heights = SHEEP$height
        widths = SHEEP$width
        if (all(heights == 0)) {
            heights = NULL
        }
        if (all(widths == 0)) {
            widths = NULL
        }
    }

    select = select_grobs(ID)
    select = sort(select)
    grobs = SHEEP$plot[select]

    if (verbose) {
        print("SHEEP")
        print(SHEEP)
        print(select)
    }


    #     Nested layouts with arrangeGrob

    # The grid.arrange() function draws on the device; for more complex layouts, we may want to store the gtable and combine it with other objects, e.g. forming nested layouts. To this end, use arrangeGrob(),

    # g1 <- arrangeGrob(grobs = gs, layout_matrix = t(lay))
    # g2 <- arrangeGrob(grobs = gs, layout_matrix = lay)
    #     grid.arrange(g1, g2, ncol=2)
    
    plot =
        # grid.arrange(
        arrangeGrob(grobs=grobs,
                    nrow=nrowPLAN,
                    ncol=ncolPLAN,
                    heights=heights,
                    widths=widths,
                    layout_matrix=ID,
                    as.table=FALSE)
    # )
    
    if (!is.null(paper_size)) {
        res = list(plot=plot, paper_size=c(paperWidth, paperHeight))
        return (res)
        
    } else {
        return (plot)
    }
}

select_grobs = function (lay) {
    id = unique(c(t(lay))) 
    id[!is.na(id)]
} 

### 2.2. Add plot ____________________________________________________
bring_grass = function (sheep=NULL, id="",
                        height=0, width=0,
                        label="", overwrite_by_id=FALSE,
                        plan=NULL) {
    
    if (is.null(plan)) {
        plan = NA
    }
    flock = list(sheep=dplyr::tibble(), plan=as.matrix(plan))

    if (!is.null(sheep)) {
        flock = add_sheep(flock, sheep=sheep, id=id,
                          height=height, width=width,
                          label=label,
                          overwrite_by_id=FALSE)
    }
    return (flock)
}


plan_of_flock = function (flock, plan) {
    if (!is.matrix(plan) & is.character(plan)) {
        # plan =
        #     "bibi bob
        #      gael mike
        #      alice jack"
        plan = gsub("[[:space:]]+", " ",
                    unlist(strsplit(plan, "\n")))
        plan = gsub("(^[[:space:]])|([[:space:]]$)", "", plan)
        plan = strsplit(plan, " ")
        plan = matrix(unlist(plan),
                      nrow=length(plan),
                      ncol=length(plan[[1]]),
                      byrow=TRUE)
    }
    flock$plan = as.matrix(plan)
    return (flock)
}

shear_sheeps = function (flock, height=TRUE, width=TRUE) {
    if (height & !all(flock$sheep$height == 0)) {
        flock$sheep$height = flock$sheep$height/sum(flock$sheep$height)
    }
    if (width & !all(flock$sheep$width == 0)) {
        flock$sheep$width = flock$sheep$width/sum(flock$sheep$width)
    }
    return (flock) 
}

add_sheep = function (flock, sheep=NULL, id="",
                      height=0, width=0,
                      label="",
                      overwrite_by_id=FALSE) {

    if (!is.ggplot(sheep)) {
        sheep$sheep$height = sheep$sheep$height * height
        sheep$sheep$width = sheep$sheep$width * width
    }
        
    if (overwrite_by_id == FALSE |
        !any(which(flock$sheep$id == id))) {

        if (nchar(id) == 0) {
            id = nrow(flock$sheep) + 1
        }
        
        if (is.ggplot(sheep) | grid::is.grob(sheep)) {
            flock$sheep =
                dplyr::bind_rows(flock$sheep,
                                 dplyr::tibble(id=id,
                                               height=height,
                                               width=width,
                                               label=label,
                                               plot=NULL))
            flock$sheep$plot[[nrow(flock$sheep)]] = sheep
        } else {

            sheep$sheep$id = paste0(id, ".", sheep$sheep$id)
            sheep$plan = matrix(paste0(id, ".", sheep$plan),
                                nrow=nrow(sheep$plan),
                                ncol=ncol(sheep$plan))
            
            flock$sheep =
                dplyr::bind_rows(flock$sheep,
                                 sheep$sheep)
            

            # flock$plan = matrix(
            #     c("info", "chronicle", "QA", "medQJ", "criteria", "foot",
            #       "info", "chronicle", "QA", "FDC", "criteria", "foot"),
            #     ncol=2)
            # sheep$plan =matrix(c("text1", "text1", "text1", "map",
            #                      "text2", "text2", "hyd", "map",
            #                      "text3", "text4", "hyd", "map",
            #                      "text3", "text4", "hyd", "map"),
            #                    nrow=4, 
            #                    byrow=TRUE) 
            
            index = which(flock$plan == id, arr.ind=TRUE)
            index = index[nrow(index):1,]
            nh = length(levels(factor(index[, "row"])))
            nw = length(levels(factor(index[, "col"])))
            h = nrow(sheep$plan)
            w = ncol(sheep$plan)
            
            for (i in 1:nh) {
                row = index[i, "row"]

                if (h > 0) {
                    if (row == 1) {
                        flock$plan =
                            rbind(matrix(rep(flock$plan[row,], h),
                                         nrow=h, byrow=TRUE),
                                  flock$plan[(row+1):nrow(flock$plan),])
                    } else if (row == nrow(flock$plan)) {
                        flock$plan =
                            rbind(flock$plan[1:(row-1),],
                                  matrix(rep(flock$plan[row,], h),
                                         nrow=h, byrow=TRUE))
                    } else {
                        flock$plan =
                            rbind(flock$plan[1:(row-1),],
                                  matrix(rep(flock$plan[row,], h),
                                         nrow=h, byrow=TRUE),
                                  flock$plan[(row+1):nrow(flock$plan),])
                    }
                }
            }

            for (i in 1:nw) {
                col = index[i, "col"]

                if (w > 0) {
                    if (col == 1) {
                        flock$plan =
                            cbind(matrix(rep(flock$plan[, col], w),
                                         ncol=w, byrow=FALSE),
                                  flock$plan[, (col+1):ncol(flock$plan)])
                    } else if (col == ncol(flock$plan)) {
                        flock$plan =
                            cbind(flock$plan[, 1:(col-1)],
                                  matrix(rep(flock$plan[, col], w),
                                         ncol=w, byrow=FALSE))
                    } else {
                        flock$plan =
                            cbind(flock$plan[, 1:(col-1)],
                                  matrix(rep(flock$plan[, col], w),
                                         ncol=w, byrow=FALSE),
                                  flock$plan[, (col+1):ncol(flock$plan)])
                    }
                }
            }

            sheep$plan = matrix(rep(sheep$plan, each=nh),
                                nrow=nrow(sheep$plan)*nh, byrow=FALSE)
            sheep$plan = t(matrix(rep(t(sheep$plan), each=nw),
                                  nrow=ncol(sheep$plan)*nw, byrow=FALSE))
            
            flock$plan[flock$plan == id] = sheep$plan

            print("id")
            print(id)
            print("flock")
            print(flock)
            print("")
            
        }
        
    } else {
        if (is.ggplot(sheep)) {
            here = which(sheep$id == id)
            sheep$height[here] = height
            sheep$width[here] = width
            sheep$label[here] = label
            sheep$plot[[here]] = sheep
        } else {
            for (i in 1:length(sheep$id)) {
                id = sheep$id[i]
                here = which(flock$sheep$id == id)
                flock$sheep$height[here] = sheep$sheep$height[i]
                flock$sheep$width[here] = sheep$sheep$width[i]
                flock$sheep$label[here] = sheep$sheep$label[i]
                flock$sheep$plot[[here]] = sheep$sheep$plot[[i]]
            }
        }
    }
    
    return (flock)
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
                nnum = nchar(as.character(
                    as.numeric(dec)))
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


#' @title round_label
#' @export
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
#' @param france_dir Directory you want to use in ash\\resources_path\\
#' to get the France shapefile.
#' @param france_file Name of the France shapefile.
#' @param basinHydro_dir Directory you want to use in ash\\resources_path\\
#' to get the hydrological basin shapefile.
#' @param basinHydro_file Name of the hydrological basin shapefile.
#' @param regionHydro_dir Directory you want to use in
#' ash\\resources_path\\ to get the hydrological sub-basin shapefile.
#' @param regionHydro_file Name of the hydrological sub-basin shapefile.
#' @param river_dir Directory you want to use in ash\\resources_path\\
#' to get the hydrological network shapefile.
#' @param river_file  Name of the hydrological network shapefile.
#' @param show_river Boolean to indicate if the shapefile of the
#' hydrological network will be charge because it is a heavy one and
#' that it slows down the entire process (default : TRUE)
#' @return A list of shapefiles converted as tibbles that can be plot
#' with 'geom_polygon' or 'geom_path'.
#' @export
load_shapefile = function (resources_path, Code,
                           france_dir, france_file,
                           basinHydro_dir, basinHydro_file,
                           regionHydro_dir, regionHydro_file,
                           entiteHydro_dir, entiteHydro_file,
                           entiteHydro_coord,
                           river_dir, river_file,
                           river_selection=c('all'),
                           toleranceRel=10000) {
    
    # Path for shapefile
    france_path = file.path(resources_path,
                            france_dir,
                            france_file)
    basinHydro_path = file.path(resources_path,
                                basinHydro_dir,
                                basinHydro_file)
    regionHydro_path = file.path(resources_path,
                                 regionHydro_dir,
                                 regionHydro_file)
    entiteHydro_path = file.path(resources_path,
                                 entiteHydro_dir,
                                 entiteHydro_file)
    river_path = file.path(resources_path,
                           river_dir,
                           river_file)
    
    # France
    france = st_read(france_path)
    france = st_union(france)
    france = st_simplify(france,
                         preserveTopology=TRUE,
                         dTolerance=toleranceRel)
    france = st_transform(france, 2154)
    
    # Hydrological basin
    basinHydro = st_read(basinHydro_path)
    basinHydro = st_simplify(basinHydro,
                        preserveTopology=TRUE,
                        dTolerance=toleranceRel/2)
    basinHydro = st_transform(basinHydro, 2154)
    
    # Hydrological sub-basin
    regionHydro = st_read(regionHydro_path)    
    regionHydro = st_simplify(regionHydro,
                           preserveTopology=TRUE,
                           dTolerance=toleranceRel/2)
    regionHydro = st_transform(regionHydro, 2154)
    
    # Hydrological code bassin
    entiteHydro_list = lapply(entiteHydro_path, read_sf)
    entiteHydro_list = lapply(entiteHydro_list, st_transform, 2154)
    entiteHydro = do.call(rbind, entiteHydro_list)
    entiteHydro = entiteHydro[entiteHydro$Code %in% Code,]
    entiteHydro = st_simplify(entiteHydro,
                            preserveTopology=TRUE,
                            dTolerance=toleranceRel/3)
    
    entiteHydro = st_transform(entiteHydro, 2154)
    

    # If the river shapefile needs to be load
    if (!("none" %in% river_selection)) {
        # Hydrographic network
        river = st_read(river_path)

        if ('all' %in% river_selection) {
            river = river[river$Classe == 1,]
        } else {
            river = river[grepl(paste(river_selection, collapse='|'),
                                river$NomEntiteH),]
        }
        river = st_simplify(river,
                            preserveTopology=TRUE,
                            dTolerance=toleranceRel/3)
        river = st_transform(river, 2154) 
    } else {
        river = NULL
    }

    return (list(france=france,
                 basinHydro=basinHydro,
                 regionHydro=regionHydro,
                 entiteHydro=entiteHydro,
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

### 4.3. Font loading ________________________________________________
load_font = function (path=NULL, force_import=FALSE) {

    extrafont::font_import(paths=path)
    
    # if (is.null(extrafont::fonts()) | force_import) {
    # remotes::install_version("Rttf2pt1", version = "1.3.8")
    # extrafont::font_import(paths=path)
    # }
    # extrafont::loadfonts(device="all", quiet=TRUE)
    # theme = theme(text=element_text(family="frutiger-57-condensed"))
}


## 5. OTHER __________________________________________________________
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


guess_newline = function (text, nLim=20, newlineId="\n") {
    nbNewline = 0
    nbChar = nchar(text)
    while (nbChar > nLim | sum(grepl(" ", text)) == 0) {
        nbNewline = nbNewline + 1
        posSpace = which(strsplit(text, "")[[1]] == " ")
        idNewline = which.min(abs(posSpace - nLim * nbNewline))
        posNewline = posSpace[idNewline]
        text = paste(substring(text,
                               c(1, posNewline + 1),
                               c(posNewline - 1,
                                 nchar(text))),
                     collapse=newlineId)
        if (sum(grepl(" ", text)) == 0) {
            break
        }
        Newline = substr(text,
                         posNewline + 2,
                         nchar(text))
        nbChar = nchar(Newline)
    }
    return (text)
}


plotly_save = function (fig, path) {
    htmlwidgets::saveWidget(fig,
                            file=path,
                            selfcontained=TRUE)
    libdir = paste0(tools::file_path_sans_ext(basename(path)), "_files")
    unlink(file.path(dirname(path), libdir), recursive=TRUE)
}


other_letters = c("é", "è", "à")
numbers = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")
symbols = c("-", "_", ".", ",", "*")
get_alphabet_in_px = function (alphabet=c(letters, LETTERS,
                                          other_letters,
                                          numbers, symbols),
                               size=50, font="sans",
                               style="normal",
                               isNorm=TRUE,
                               out_dir="letter",
                               save=FALSE) {
    library(magick)
    if (save &!dir.exists(out_dir)) {
        dir.create(out_dir)
    }
    find_id = function (X, a, where="") {
        if (any(a %in% X)) {
            id = which(X == a)
            if (where == "first") {
                id = id[1]
            } else if (where == "last") {
                id = id[length(id)]
            }
            return (id)
        } else {
            return (NA)
        }
    }

    if (style == "bold") {
        weight = 700
    } else {
        weight = 400
    }
    
    PX = c()
    for (letter in alphabet) {
        img = image_blank(width=size, height=size, color="white")
        img = image_annotate(img, letter, size=size, style="normal",
                             weight=weight, font=font, color="#000000")
        pixels = as.character(c(image_data(img, channel="gray")))
        pixels[pixels != "ff"] = "1"
        pixels[pixels == "ff"] = "0"
        pixels = as.numeric(pixels)
        pixels = matrix(pixels, ncol=size, byrow=TRUE)
        if (save) {
            write.table(pixels,
                        file=file.path(out_dir,
                                       paste0(letter, ".txt")),
                        row.names=FALSE, col.names=FALSE)
        }
        first_one = apply(pixels, 1, find_id, a=1, where="first")
        last_one = apply(pixels, 1, find_id, a=1, where="last")
        px = max(last_one, na.rm=TRUE) -
            min(first_one, na.rm=TRUE) + 1
        PX = c(PX, px)
        names(PX)[length(PX)] = letter
    }
    PX = c(PX, PX["_"])
    names(PX)[length(PX)] = ' '
    if (isNorm) {
        PX = PX/max(PX)
    }
    return (PX)
}


X2px = function (X, PX) {
    PX[X]
}


select_good = function (X) {
    Xrle = rle(X)
    value = Xrle$values[Xrle$lengths == max(Xrle$lengths)]
    if (length(value) > 1) {
        value = mean(value, na.rm=TRUE)
    }
    return (value)
}
