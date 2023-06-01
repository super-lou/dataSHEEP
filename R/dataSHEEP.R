# Copyright 2023 Louis Héraut (louis.heraut@inrae.fr)*1
#                     
# *1   INRAE, France
#
# This file is part of dataSHEEP R package.
#
# dataSHEEP R package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# dataSHEEP R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with dataSHEEP R package.
# If not, see <https://www.gnu.org/licenses/>.


### 2.1. Merge _______________________________________________________
#' @title Merge
#' @export
#' @title bring_grass
#' @description ...
#' @param sheep ...
#' @param NULL ...
#' @param id ... (default : "")
#' @param height ...
#' @param 0 ...
#' @param width ...
#' @param 0 ...
#' @param label ... (default : "")
#' @param overwrite_by_id ...
#' @param FALSE ...
#' @param plan ...
#' @return ...
#' @examples
#' ...
#' @export
return_to_sheepfold = function (flock,
                                page_margin=c(t=0, r=0, b=0, l=0),
                                paper_size=NULL,
                                hjust=0, vjust=1,
                                verbose=FALSE) {

    SHEEP = flock$sheep
    PLAN = flock$plan
    Plots = SHEEP$plot
    Labels = SHEEP$label
    
    widths_var = list()
    nPlot = length(Plots)
    for (k in 1:nPlot) {
        if (is.null(Plots[[k]])) {
            Plots[[k]] = void()
        }

        if (Labels[k] == "align") {
            Plots[[k]] =
                ggplot_gtable(ggplot_build(Plots[[k]]))
            widths_var = append(widths_var,
                                list(Plots[[k]]$widths))
        }
    }
    
    if (length(widths_var) > 0) {
        maxWidth = do.call(grid::unit.pmax, widths_var)
        for (k in 1:nPlot) {
            if (Labels[k] == "align") {
                Plots[[k]]$widths = as.list(maxWidth)
            }
        }
    }

    SHEEP$plot = Plots

    if (!is.null(PLAN)) {
        SHEEPid = SHEEP$id
        nSHEEPid = length(SHEEP$id)
        NUM = c()
        nrowPLAN = nrow(PLAN)
        ncolPLAN = ncol(PLAN)
        PLAN = as.vector(PLAN)
        nPLAN = nrowPLAN*ncolPLAN
        NUM = match(PLAN, SHEEP$id)

        PLOT = SHEEP$plot[NUM[!is.na(NUM)]]
        PLAN = matrix(PLAN, nrow=nrowPLAN, ncol=ncolPLAN)
        NUM = matrix(NUM, nrow=nrowPLAN, ncol=ncolPLAN)

        ncolPLAN = ncol(PLAN)

        rowFoot = which(PLAN[, 1] == "foot")

        if (!identical(rowFoot, integer(0))) {
            NUM = rbind(rep(NA, times=ncolPLAN),
                        NUM[1:(rowFoot-1),, drop=FALSE],
                        rep(NA, times=ncolPLAN),
                        NUM[rowFoot:nrowPLAN,, drop=FALSE])
            PLAN = rbind(rep("tjust", times=ncolPLAN),
                         PLAN[1:(rowFoot-1),, drop=FALSE],
                         rep("bjust", times=ncolPLAN),
                         PLAN[rowFoot:nrowPLAN,, drop=FALSE])
        }
        
        nrowPLAN = nrow(PLAN)
        NUM = cbind(rep(NA, times=nrowPLAN), NUM,
                    rep(NA, times=nrowPLAN))
        PLAN = cbind(rep("ljust", times=nrowPLAN), PLAN,
                     rep("rjust", times=nrowPLAN))

        ncolPLAN = ncol(PLAN)
        NUM = rbind(rep(NA, times=ncolPLAN), NUM,
                    rep(NA, times=ncolPLAN))
        PLAN = rbind(rep("tmargin", times=ncolPLAN), PLAN,
                     rep("bmargin", times=ncolPLAN))
        
        nrowPLAN = nrow(PLAN)
        NUM = cbind(rep(NA, times=nrowPLAN), NUM,
                    rep(NA, times=nrowPLAN))
        PLAN = cbind(rep("lmargin", times=nrowPLAN), PLAN,
                     rep("rmargin", times=nrowPLAN))

        nrowPLAN = nrow(PLAN)
        ncolPLAN = ncol(PLAN)

    } else {
        NUM = 1:nrow(SHEEP)
        PLAN = SHEEP$id
        nrowPLAN = 1
        ncolPLAN = length(PLAN)
        NUM = matrix(NUM, nrow=nrowPLAN, ncol=ncolPLAN)
        PLAN = matrix(PLAN, nrow=nrowPLAN, ncol=ncolPLAN)
    }

    HEIGHT = SHEEP$height[match(PLAN, SHEEP$id)]
    HEIGHT = matrix(HEIGHT, nrow=nrowPLAN, ncol=ncolPLAN)
    WIDTH = SHEEP$width[match(PLAN, SHEEP$id)]
    WIDTH = matrix(WIDTH, nrow=nrowPLAN, ncol=ncolPLAN)
    
    get_group = function (SHEEP) {
        dot = gsub("[^.]", "", SHEEP$id)
        dot = nchar(dot)
        dot[is.na(dot)] = 0
        group = dot+1
        SHEEP$group = group
        return (SHEEP)
    }

    get_block = function (SHEEP_group) {
        SHEEP_group$block =
            gsub("[.]$", "",
                 stringr::str_extract(SHEEP_group$id, ".*[.]"))
        return (SHEEP_group)
    }
    
    SHEEP = get_group(SHEEP)
    SHEEP$block = ""    
    nGroup = max(SHEEP$group, na.rm=TRUE)
    SHEEP$num = 1:nrow(SHEEP)

    if (nGroup > 1) {
        
        for (i in nGroup:1) {
            if (i == 1) {
                break
            }
            
            SHEEP_group = SHEEP[SHEEP$group == i,]
            SHEEP_group = get_block(SHEEP_group)
            SHEEP$block[SHEEP$group == i] = SHEEP_group$block

            Block = levels(factor(SHEEP_group$block))
            nBlock = length(Block)
            for (j in 1:nBlock) {
                block = Block[j]
                SHEEP_group_block = SHEEP_group[SHEEP_group$block == block,]

                if (i == 1) {
                    OK = apply(PLAN, c(1, 2), grepl,
                               pattern=paste0(gsub("[.]", "[.]", block), "$"))
                } else {
                    OK = apply(PLAN, c(1, 2), grepl,
                               pattern=paste0(gsub("[.]", "[.]", block), "[.]"))
                }
                
                nrowOK = max(apply(OK, 2, sum))
                ncolOK = max(apply(OK, 1, sum))

                NUM_group_block = matrix(NUM[OK], nrow=nrowOK, ncol=ncolOK)
                PLAN_group_block = matrix(PLAN[OK], nrow=nrowOK, ncol=ncolOK)
                HEIGHT_group_block = matrix(HEIGHT[OK], nrow=nrowOK, ncol=ncolOK)
                WIDTH_group_block = matrix(WIDTH[OK], nrow=nrowOK, ncol=ncolOK)

                heights_group_block = apply(HEIGHT_group_block, 1, min, na.rm=TRUE)
                widths_group_block = apply(WIDTH_group_block, 2, min, na.rm=TRUE)

                rowHEIGHT_group_block = as.list(as.data.frame(t(HEIGHT_group_block)))
                rowPLAN_group_block = as.list(as.data.frame(t(PLAN_group_block)))
                names(rowHEIGHT_group_block) = NULL
                names(rowPLAN_group_block) = NULL
                idHEIGHT_group_block = apply(HEIGHT_group_block, 1, which.min)
                heights_real = mapply('[', rowHEIGHT_group_block, idHEIGHT_group_block)
                names(heights_real) =
                    mapply('[', rowPLAN_group_block, idHEIGHT_group_block)
                heights_real = heights_real[!duplicated(names(heights_real))]

                colWIDTH_group_block = as.list(as.data.frame(WIDTH_group_block))
                colPLAN_group_block = as.list(as.data.frame(PLAN_group_block))
                names(colWIDTH_group_block) = NULL
                names(colPLAN_group_block) = NULL
                idWIDTH_group_block = apply(WIDTH_group_block, 2, which.min)
                widths_real = mapply('[', colWIDTH_group_block, idWIDTH_group_block)
                names(widths_real) =
                    mapply('[', colPLAN_group_block, idWIDTH_group_block)
                widths_real = widths_real[!duplicated(names(widths_real))]

                if (all(heights_group_block == 0)) {
                    heights = NULL
                } else {
                    heights = heights_group_block
                }
                if (all(widths_group_block == 0)) {
                    widths = NULL
                } else {
                    widths = widths_group_block
                }

                grobs = SHEEP$plot[SHEEP$num %in%
                                   sort(select_grobs(NUM_group_block))]
                
                grob =
                    arrangeGrob(grobs=grobs,
                                nrow=nrow(NUM_group_block),
                                ncol=ncol(NUM_group_block),
                                heights=heights,
                                widths=widths,
                                layout_matrix=NUM_group_block,
                                as.table=FALSE)

                OK_block = SHEEP$group == i &
                    SHEEP$block == block            
                SHEEP[OK_block,]$id = block
                SHEEP[OK_block,]$height = sum(heights_real)
                SHEEP[OK_block,]$width = sum(widths_real)
                SHEEP[OK_block,]$label =
                    paste0(SHEEP_group_block$label[nchar(SHEEP_group_block$label) > 0],
                           collapse="/")
                SHEEP[OK_block,]$plot =
                    list(
                        # ggplotify::as.ggplot(
                        grob
                        # )
                    )
                SHEEP[OK_block,]$group = SHEEP_group_block$group-1
                SHEEP[OK_block,]$num = 0
                SHEEP[OK_block,]$block = ""
                SHEEP = dplyr::distinct(SHEEP, num, .keep_all=TRUE)
                SHEEP$num = 1:nrow(SHEEP)
                
                PLAN[OK] = block
                
                okPLAN = t(!apply(PLAN, 1, duplicated)) &
                    !apply(PLAN, 2, duplicated)
                row2rm = apply(okPLAN, 1, sum) != 0
                col2rm = apply(okPLAN, 2, sum) != 0
                PLAN = PLAN[row2rm, col2rm]
                nrowPLAN = nrow(PLAN)
                ncolPLAN = ncol(PLAN)
                HEIGHT = SHEEP$height[match(PLAN, SHEEP$id)]
                HEIGHT = matrix(HEIGHT, nrow=nrowPLAN, ncol=ncolPLAN)
                WIDTH = SHEEP$width[match(PLAN, SHEEP$id)]
                WIDTH = matrix(WIDTH, nrow=nrowPLAN, ncol=ncolPLAN)
                NUM = match(PLAN, SHEEP$id)
                NUM = matrix(NUM, nrow=nrowPLAN, ncol=ncolPLAN)
            }
        }
    }

    if (!is.null(paper_size)) {

        if (verbose) {
            print("RESUME")
            print(NUM)
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
            HEIGHT[HEIGHT == 0] = paperHeight - page_margin["t"] - page_margin["b"]
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
            WIDTH[WIDTH == 0] = paperWidth - page_margin["l"] - page_margin["r"]
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

    select = select_grobs(NUM)
    select = sort(select)
    grobs = SHEEP$plot[select]

    if (verbose) {
        print("SHEEP")
        print(SHEEP)
        print(select)
    }

    plot =
        arrangeGrob(grobs=grobs,
                    nrow=nrowPLAN,
                    ncol=ncolPLAN,
                    heights=heights,
                    widths=widths,
                    layout_matrix=NUM,
                    as.table=FALSE)
    
    if (!is.null(paper_size)) {
        res = list(plot=plot, paper_size=c(paperWidth, paperHeight))
        return (res)
        
    } else {
        return (plot)
    }
}

#' @title  get_group
#' @description ...
#' @param SHEEP ...
#' @return ...
#' @examples
#' ...
#' @export
select_grobs = function (lay) {
    id = unique(c(t(lay))) 
    id[!is.na(id)]
} 

### 2.2. Add plot ____________________________________________________
#' @title  get_block
#' @description ...
#' @param SHEEP_group ...
#' @return ...
#' @examples
#' ...
#' @export
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


#' @title add_sheep
#' @description ...
#' @param flock ...
#' @param sheep ...
#' @param NULL ...
#' @param id ... (default : "")
#' @param height ...
#' @param 0 ...
#' @param width ...
#' @param 0 ...
#' @param label ... (default : "")
#' @param overwrite_by_id ... (default : FALSE)
#' @return ...
#' @examples
#' ...
#' @export
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

#' @title is.wholenumber
#' @description ...
#' @param X ...
#' @param tol ... (default : .Machine$double.eps^0.5)
#' @return ...
#' @examples
#' ...
#' @export
shear_sheeps = function (flock, height=TRUE, width=TRUE) {

    SHEEP = flock$sheep
    PLAN = flock$plan    
    nrowPLAN = nrow(PLAN)
    ncolPLAN = ncol(PLAN)
    HEIGHT = SHEEP$height[match(PLAN, SHEEP$id)]
    HEIGHT = matrix(HEIGHT, nrow=nrowPLAN, ncol=ncolPLAN)
    WIDTH = SHEEP$width[match(PLAN, SHEEP$id)]
    WIDTH = matrix(WIDTH, nrow=nrowPLAN, ncol=ncolPLAN)

    get_group = function (SHEEP) {
        dot = gsub("[^.]", "", SHEEP$id)
        dot = nchar(dot)
        dot[is.na(dot)] = 0
        group = dot+1
        SHEEP$group = group
        return (SHEEP)
    }

    get_block = function (SHEEP_group) {
        SHEEP_group$block =
            gsub("[.]$", "",
                 stringr::str_extract(SHEEP_group$id, ".*[.]"))
        return (SHEEP_group)
    }
    
    SHEEP = get_group(SHEEP)
    SHEEP$block = ""    
    nGroup = max(SHEEP$group, na.rm=TRUE)
    SHEEP$num = 1:nrow(SHEEP)
    
    if (nGroup > 1) {
        
        for (i in nGroup:1) {
            if (i == 1) {
                break
            }
            
            SHEEP_group = SHEEP[SHEEP$group == i,]
            SHEEP_group = get_block(SHEEP_group)
            SHEEP$block[SHEEP$group == i] = SHEEP_group$block
            
            Block = levels(factor(SHEEP_group$block))
            nBlock = length(Block)
            for (j in 1:nBlock) {
                block = Block[j]
                SHEEP_group_block = SHEEP_group[SHEEP_group$block == block,]

                OK = apply(PLAN, c(1, 2), grepl, pattern=gsub("[.]", "[.]", block))
                nrowOK = max(apply(OK, 2, sum))
                ncolOK = max(apply(OK, 1, sum))
                # NUM_group_block = matrix(NUM[OK], nrow=nrowOK, ncol=ncolOK)
                PLAN_group_block = matrix(PLAN[OK], nrow=nrowOK, ncol=ncolOK)
                HEIGHT_group_block = matrix(HEIGHT[OK], nrow=nrowOK, ncol=ncolOK)
                WIDTH_group_block = matrix(WIDTH[OK], nrow=nrowOK, ncol=ncolOK)

                heights_group_block = apply(HEIGHT_group_block, 1, min, na.rm=TRUE)
                widths_group_block = apply(WIDTH_group_block, 2, min, na.rm=TRUE)

                rowHEIGHT_group_block = as.list(as.data.frame(t(HEIGHT_group_block)))
                rowPLAN_group_block = as.list(as.data.frame(t(PLAN_group_block)))
                names(rowHEIGHT_group_block) = NULL
                names(rowPLAN_group_block) = NULL
                idHEIGHT_group_block = apply(HEIGHT_group_block, 1, which.min)
                heights_real = mapply('[', rowHEIGHT_group_block, idHEIGHT_group_block)
                names(heights_real) =
                    mapply('[', rowPLAN_group_block, idHEIGHT_group_block)
                heights_real = heights_real[!duplicated(names(heights_real))]

                colWIDTH_group_block = as.list(as.data.frame(WIDTH_group_block))
                colPLAN_group_block = as.list(as.data.frame(PLAN_group_block))
                names(colWIDTH_group_block) = NULL
                names(colPLAN_group_block) = NULL
                idWIDTH_group_block = apply(WIDTH_group_block, 2, which.min)
                widths_real = mapply('[', colWIDTH_group_block, idWIDTH_group_block)
                names(widths_real) =
                    mapply('[', colPLAN_group_block, idWIDTH_group_block)
                widths_real = widths_real[!duplicated(names(widths_real))]

                OK_block = SHEEP$group == i &
                    SHEEP$block == block            
                SHEEP[OK_block,]$id = block
                SHEEP[OK_block,]$height = sum(heights_real)
                SHEEP[OK_block,]$width = sum(widths_real)
                SHEEP[OK_block,]$group = SHEEP_group_block$group-1
                SHEEP[OK_block,]$num = 0
                SHEEP[OK_block,]$block = ""
                SHEEP = dplyr::distinct(SHEEP, num, .keep_all=TRUE)
                SHEEP$num = 1:nrow(SHEEP)

                PLAN[OK] = block

                okPLAN = t(!apply(PLAN, 1, duplicated)) &
                    !apply(PLAN, 2, duplicated)
                row2rm = apply(okPLAN, 1, sum) != 0
                col2rm = apply(okPLAN, 2, sum) != 0
                PLAN = PLAN[row2rm, col2rm]

                nrowPLAN = nrow(PLAN)
                ncolPLAN = ncol(PLAN)
                HEIGHT = SHEEP$height[match(PLAN, SHEEP$id)]
                HEIGHT = matrix(HEIGHT, nrow=nrowPLAN, ncol=ncolPLAN)
                WIDTH = SHEEP$width[match(PLAN, SHEEP$id)]
                WIDTH = matrix(WIDTH, nrow=nrowPLAN, ncol=ncolPLAN)
            }
        }
    }
    
    rowHEIGHT = as.list(as.data.frame(t(HEIGHT)))
    rowPLAN = as.list(as.data.frame(t(PLAN)))
    names(rowHEIGHT) = NULL
    names(rowPLAN) = NULL
    idHEIGHT = apply(HEIGHT, 1, which.min)
    heights_real = mapply('[', rowHEIGHT, idHEIGHT)
    names(heights_real) =
        mapply('[', rowPLAN, idHEIGHT)
    heights_real = heights_real[!duplicated(names(heights_real))]

    colWIDTH = as.list(as.data.frame(WIDTH))
    colPLAN = as.list(as.data.frame(PLAN))
    names(colWIDTH) = NULL
    names(colPLAN) = NULL
    idWIDTH = apply(WIDTH, 2, which.min)
    widths_real = mapply('[', colWIDTH, idWIDTH)
    names(widths_real) =
        mapply('[', colPLAN, idWIDTH)
    widths_real = widths_real[!duplicated(names(widths_real))]

    if (height & !all(flock$sheep$height == 0)) {
        flock$sheep$height = flock$sheep$height/sum(heights_real)
    }
    if (width & !all(flock$sheep$width == 0)) {
        flock$sheep$width = flock$sheep$width/sum(widths_real)
    }

    return (flock) 
}

#' @title load_font
#' @description ...
#' @param path ...
#' @param NULL ...
#' @param force_import ... (default : FALSE)
#' @return ...
#' @examples
#' ...
#' @export
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
