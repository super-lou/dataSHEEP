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

normalize_reduce = function (vect, plan, vect_plans) {
    ok = vect_plans == plan
    ok[is.na(ok)] = FALSE
    vect[ok] = vect[ok] / sum(ok)
    return (vect)
}

normalize_mapply = function (vect, vect_plans) {
    if (!all(is.na(vect))) {
        plans = table(vect_plans)
        vect = purrr::reduce(plans, normalize_reduce,
                             .init=vect, vect_plans=vect_plans)
    }
    return (vect)
}


get_heights = function (HEIGHT, PLAN) {
    colHEIGHT = as.list(as.data.frame(HEIGHT))
    colPLAN = as.list(as.data.frame(PLAN))
    names(colHEIGHT) = NULL
    names(colPLAN) = NULL
    colPLAN = lapply(colPLAN, as.character)
    colPLAN_dup = lapply(colPLAN, duplicated)
    colPLAN_dup = lapply(colPLAN_dup, '!')

    colHEIGHT_not_dup = mapply('[', colHEIGHT, colPLAN_dup,
                               SIMPLIFY=FALSE)
    
    # colHEIGHT_sum = sapply(colHEIGHT_not_dup, sum, na.rm=TRUE)
    # colHEIGHT_id = which.max(colHEIGHT_sum)[1]
    
    # heights = colHEIGHT[[colHEIGHT_id]]
    # heights_real = colHEIGHT_not_dup[[colHEIGHT_id]]



    Plans = levels(factor(PLAN))
    PLAN_table = c()
    for (plan in Plans) {
        PLAN_table =
            c(PLAN_table,
              max(sapply(colPLAN, length_ok, plan=plan),
                  na.rm=TRUE))
        names(PLAN_table)[length(PLAN_table)] = plan
    }
    
    for (i in 1:length(PLAN_table)) {
        plan = names(PLAN_table)[i]
        n = PLAN_table[i]
        colHEIGHT = mapply(divide_where,
                           colHEIGHT, colPLAN,
                           plan=plan, n=n,
                           SIMPLIFY=FALSE)
    }
    
    colHEIGHT = mapply(normalize_mapply,
                       colHEIGHT, colPLAN,
                       SIMPLIFY=FALSE)
    colHEIGHT_sum = sapply(colHEIGHT_not_dup, sum, na.rm=TRUE)
    colHEIGHT_id = which(colHEIGHT_sum ==
                         max(colHEIGHT_sum, na.rm=TRUE))
    
    heights = do.call(pmax,
                      args=append(colHEIGHT[colHEIGHT_id],
                                  list(na.rm=TRUE)))
    heights_real = do.call(pmax,
                           args=append(colHEIGHT_not_dup[colHEIGHT_id],
                                       list(na.rm=TRUE)))

    # heights = round(heights, 5)
    # heights_real = round(heights_real, 5)

    return (list(heights=heights, heights_real=heights_real))
}

length_ok = function (vect, plan) {
    ok = vect == plan
    ok[is.na(ok)] = FALSE
    len = sum(ok)
    return (len)
}

divide_where = function (vect, vect_plan, plan, n) {
    if (!all(is.na(vect))) {
        vect[vect_plan == plan] = vect[vect_plan == plan] / n
    }
    return (vect)
}

get_widths = function (WIDTH, PLAN) {
    colWIDTH = as.list(as.data.frame(t(WIDTH)))
    colPLAN = as.list(as.data.frame(t(PLAN)))
    names(colWIDTH) = NULL
    names(colPLAN) = NULL
    colPLAN = lapply(colPLAN, as.character)
    colPLAN_dup = lapply(colPLAN, duplicated)
    colPLAN_dup = lapply(colPLAN_dup, '!')
    
    colWIDTH_not_dup = mapply('[', colWIDTH, colPLAN_dup,
                              SIMPLIFY=FALSE)

    Plans = levels(factor(PLAN))
    PLAN_table = c()
    for (plan in Plans) {
        PLAN_table =
            c(PLAN_table,
              max(sapply(colPLAN, length_ok, plan=plan),
                  na.rm=TRUE))
        names(PLAN_table)[length(PLAN_table)] = plan
    }
    
    for (i in 1:length(PLAN_table)) {
        plan = names(PLAN_table)[i]
        n = PLAN_table[i]
        colWIDTH = mapply(divide_where,
                          colWIDTH, colPLAN,
                          plan=plan, n=n,
                          SIMPLIFY=FALSE)
    }
    
    colWIDTH = mapply(normalize_mapply,
                      colWIDTH, colPLAN,
                      SIMPLIFY=FALSE)
    colWIDTH_sum = sapply(colWIDTH_not_dup, sum, na.rm=TRUE)
    colWIDTH_id = which(colWIDTH_sum ==
                        max(colWIDTH_sum, na.rm=TRUE))
    
    widths = do.call(pmax,
                     args=append(colWIDTH[colWIDTH_id],
                                 list(na.rm=TRUE)))
    widths_real = do.call(pmax,
                          args=append(colWIDTH_not_dup[colWIDTH_id],
                                      list(na.rm=TRUE)))

    # widths = round(widths, 5)
    # widths_real = round(widths_real, 5)
    
    return (list(widths=widths, widths_real=widths_real))
}


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
return_to_sheepfold = function (herd,
                                page_margin=c(t=0, r=0, b=0, l=0),
                                paper_size=NULL,
                                hjust=0, vjust=1,
                                verbose=FALSE) {
    if (verbose) {
        print("Look at that impressive herd :")
        print(herd)
        print("YA YA !! EVERYONE TO THE SHEEPFOLD !!")
    }
    
    SHEEP = herd$sheep
    PLAN = herd$plan
    Plots = SHEEP$plot
    Labels = SHEEP$label

    Aligns = levels(factor(Labels[grepl("align", Labels)]))
    nAlign = length(Aligns)

    if (nAlign > 0) {
        for (i in 1:nAlign) {
            
            widths_var = list()
            nPlot = length(Plots)
            for (k in 1:nPlot) {
                if (is.null(Plots[[k]])) {
                    Plots[[k]] = void()
                }
                if (grepl(Aligns[i], Labels[k])) {
                    Plots[[k]] =
                        ggplot_gtable(ggplot_build(Plots[[k]]))
                    widths_var = append(widths_var,
                                        list(Plots[[k]]$widths))
                }
            }
            
            maxWidth = do.call(grid::unit.pmax, widths_var)
            for (k in 1:nPlot) {
                if (grepl(Aligns[i], Labels[k])) {
                    Plots[[k]]$widths = as.list(maxWidth)
                }
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


                res = get_heights(HEIGHT_group_block, PLAN_group_block)
                heights_group_block = res$heights
                heights_real = res$heights_real
                
                res = get_widths(WIDTH_group_block, PLAN_group_block)
                widths_group_block = res$widths
                widths_real = res$widths_real

                if (all(is.na(heights_group_block))) {
                    heights = NULL
                } else {
                    heights = heights_group_block
                }
                if (all(is.na(widths_group_block))) {
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
                #################################################
                SHEEP[OK_block,]$height = sum(heights_real) 
                SHEEP[OK_block,]$width = sum(widths_real)
                # SHEEP[OK_block,]$height = sum(heights) 
                # SHEEP[OK_block,]$width = sum(widths)
                #################################################
                SHEEP[OK_block,]$label =
                    paste0(SHEEP_group_block$label[nchar(SHEEP_group_block$label) > 0],
                           collapse="/")
                SHEEP[OK_block,]$plot = list(grob)
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
        if (all(paper_size == 'A4')) {
            paperWidth = 21
            paperHeight = 29.7
        } else if (is.vector(paper_size) & length(paper_size) > 1) {
            paperWidth = paper_size[1]
            paperHeight = paper_size[2]
        }

        res = get_heights(HEIGHT, PLAN)
        heights = res$heights
        
        maxHeight = sum(heights, na.rm=TRUE)
        if (maxHeight == 0) {
            HEIGHT[!grepl("(just)|(margin)", PLAN)] =
                paperHeight - page_margin["t"] - page_margin["b"]
            maxHeight = paperHeight
            
        } else {
            maxHeight = maxHeight + page_margin["t"] + page_margin["b"]
        }

        if (paperHeight != maxHeight) {
            tjust_height = (1-vjust) / round(paperHeight - maxHeight, 5)
            bjust_height = vjust / round(paperHeight - maxHeight, 5)
        } else {
            tjust_height = 0
            bjust_height = 0
        }

        HEIGHT[PLAN == "tjust"] = tjust_height
        HEIGHT[PLAN == "bjust"] = bjust_height
        HEIGHT[PLAN == "tmargin"] = page_margin["t"]
        HEIGHT[PLAN == "bmargin"] = page_margin["b"]

        res = get_heights(HEIGHT, PLAN)
        heights = res$heights


        

        res = get_widths(WIDTH, PLAN)
        widths = res$widths
        widths_real = res$widths_real   
        
        maxWidth = sum(widths, na.rm=TRUE)
        if (maxWidth == 0) {
            WIDTH[!grepl("(just)|(margin)", PLAN)] =
                paperWidth - page_margin["l"] - page_margin["r"]
            maxWidth = paperWidth
            
        } else {
            maxWidth = maxWidth + page_margin["l"] + page_margin["r"]
        }

        if (paperWidth != maxWidth) {
            ljust_width = hjust / round(paperWidth - maxWidth, 5)
            rjust_width = (1-hjust) / round(paperWidth - maxWidth, 5)
        } else {
            ljust_width = 0
            rjust_width = 0
        }
        
        WIDTH[PLAN == "ljust"] = ljust_width
        WIDTH[PLAN == "rjust"] = rjust_width
        WIDTH[PLAN == "lmargin"] = page_margin["l"]
        WIDTH[PLAN == "rmargin"] = page_margin["r"]

        res = get_widths(WIDTH, PLAN)
        widths = res$widths

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

    if (verbose) {
        print("PLAN")
        print(PLAN)
        print("HEIGHT")
        print(HEIGHT)
        print("heights")
        print(heights)
        print("WIDTH")
        print(WIDTH)
        print("widths")
        print(widths)
    }

    select = select_grobs(NUM)
    select = sort(select)
    grobs = SHEEP$plot[select]

    plot =
        gridExtra::arrangeGrob(grobs=grobs,
                               nrow=nrowPLAN,
                               ncol=ncolPLAN,
                               heights=heights,
                               widths=widths,
                               layout_matrix=NUM)
    
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
                        plan=NULL, verbose=FALSE) {

    if (verbose) {
        print("Yummy !! this grass looks good, it's sure that sheep will come and taste it !")
    }
    
    if (is.null(plan)) {
        plan = NA
    }
    herd = list(sheep=dplyr::tibble(), plan=as.matrix(plan))

    if (!is.null(sheep)) {
        herd = add_sheep(herd, sheep=sheep, id=id,
                          height=height, width=width,
                          label=label,
                          overwrite_by_id=FALSE)
    }
    return (herd)
}


#' @title add_sheep
#' @description ...
#' @param herd ...
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
plan_of_herd = function (herd, plan, verbose=FALSE) {
    
    if (verbose) {
        print("Ohh it's a nice herd you want :")
        print(plan)
    }
    
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
    herd$plan = as.matrix(plan)
    return (herd)
}



shear_sheeps = function (herd, height=TRUE, width=TRUE,
                         verbose=FALSE) {

    if (verbose) {
        print("A good shear before joining the herd")
    }
    
    SHEEP = herd$sheep
    PLAN = herd$plan    
    nrowPLAN = nrow(PLAN)
    ncolPLAN = ncol(PLAN)
    HEIGHT = SHEEP$height[match(PLAN, SHEEP$id)]
    HEIGHT = matrix(HEIGHT, nrow=nrowPLAN, ncol=ncolPLAN)
    WIDTH = SHEEP$width[match(PLAN, SHEEP$id)]
    WIDTH = matrix(WIDTH, nrow=nrowPLAN, ncol=ncolPLAN)
    
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

                PLAN_group_block = matrix(PLAN[OK], nrow=nrowOK, ncol=ncolOK)
                HEIGHT_group_block = matrix(HEIGHT[OK], nrow=nrowOK, ncol=ncolOK)
                WIDTH_group_block = matrix(WIDTH[OK], nrow=nrowOK, ncol=ncolOK)

                res = get_heights(HEIGHT_group_block, PLAN_group_block)
                heights_group_block = res$heights
                heights_real = res$heights_real
                
                res = get_widths(WIDTH_group_block, PLAN_group_block)
                widths_group_block = res$widths
                widths_real = res$widths_real 

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

    res = get_heights(HEIGHT, PLAN)
    colHEIGHT_sum_max = sum(res$heights_real, na.rm=TRUE)
    if (height & !all(is.na(herd$sheep$height))) {
        herd$sheep$height = herd$sheep$height/colHEIGHT_sum_max
    }
    
    res = get_widths(WIDTH, PLAN)
    colWIDTH_sum_max = sum(res$widths_real, na.rm=TRUE)
    if (width & !all(is.na(herd$sheep$width))) {
        herd$sheep$width = herd$sheep$width/colWIDTH_sum_max
    }

    return (herd) 
}


is_sheep = function (pseudo_sheep) {
    return (tibble::is_tibble(pseudo_sheep[[1]]) &
            is.matrix(pseudo_sheep[[2]]))
}


# is.ggplot(sheep) | grid::is.grob(sheep)

#' @title load_font
#' @description ...
#' @param path ...
#' @param NULL ...
#' @param force_import ... (default : FALSE)
#' @return ...
#' @examples
#' ...
#' @export
add_sheep = function (herd, sheep=NULL, id="",
                      height=NA, width=NA,
                      label="",
                      overwrite_by_id=FALSE,
                      verbose=FALSE) {

    if (verbose) {
        print(paste0("Adding of ", id, " to the herd !"))
    }

    if (is_sheep(sheep)) {
        sheep = shear_sheeps(sheep, height=TRUE, width=TRUE,
                             verbose=verbose)

        if (all(is.na(sheep$sheep$height))) {
            sheep$sheep$height = height
        } else {
            sheep$sheep$height = sheep$sheep$height * height
        }
        
        if (all(is.na(sheep$sheep$width))) {
            sheep$sheep$width = width
        } else {
            sheep$sheep$width = sheep$sheep$width * width
        }
    }
    
    if (overwrite_by_id == FALSE |
        !any(which(herd$sheep$id == id))) {

        if (nchar(id) == 0) {
            id = nrow(herd$sheep) + 1
        }
        
        if (!is_sheep(sheep)) {
            herd$sheep =
                dplyr::bind_rows(herd$sheep,
                                 dplyr::tibble(id=id,
                                               height=height,
                                               width=width,
                                               label=label,
                                               plot=NULL))
            herd$sheep$plot[[nrow(herd$sheep)]] = sheep
            
        } else {
            sheep$sheep$id = paste0(id, ".", sheep$sheep$id)
            sheep$plan = matrix(paste0(id, ".", sheep$plan),
                                nrow=nrow(sheep$plan),
                                ncol=ncol(sheep$plan))
            
            herd$sheep =
                dplyr::bind_rows(herd$sheep,
                                 sheep$sheep)
            
            index = which(herd$plan == id, arr.ind=TRUE)
            index = index[nrow(index):1,, drop=FALSE]
            
            nh = length(levels(factor(index[, "row"])))
            nw = length(levels(factor(index[, "col"])))
            h = nrow(sheep$plan)
            w = ncol(sheep$plan)
            
            for (i in 1:nh) {
                row = index[i, "row"]

                if (h > 0) {
                    if (row == 1) {
                        if (nrow(herd$plan) == 1) {
                            herd$plan =
                                matrix(rep(herd$plan[row,], h),
                                       nrow=h, byrow=TRUE)
                        } else {
                            herd$plan =
                                rbind(matrix(rep(herd$plan[row,], h),
                                             nrow=h, byrow=TRUE),
                                      herd$plan[(row+1):nrow(herd$plan),])
                        } 
                    } else if (row == nrow(herd$plan)) {
                        herd$plan =
                            rbind(herd$plan[1:(row-1),],
                                  matrix(rep(herd$plan[row,], h),
                                         nrow=h, byrow=TRUE))
                    } else {
                        herd$plan =
                            rbind(herd$plan[1:(row-1),],
                                  matrix(rep(herd$plan[row,], h),
                                         nrow=h, byrow=TRUE),
                                  herd$plan[(row+1):nrow(herd$plan),])
                    }
                }
            }

            for (i in 1:nw) {
                col = index[i, "col"]
                if (w > 0) {
                    if (col == 1) {
                        if (ncol(herd$plan) == 1) {
                            herd$plan =
                                matrix(rep(herd$plan[, col], w),
                                       ncol=w, byrow=FALSE)
                        } else {
                            herd$plan =
                                cbind(matrix(rep(herd$plan[, col], w),
                                             ncol=w, byrow=FALSE),
                                      herd$plan[, (col+1):ncol(herd$plan)])
                        }
                    } else if (col == ncol(herd$plan)) {
                        herd$plan =
                            cbind(herd$plan[, 1:(col-1)],
                                  matrix(rep(herd$plan[, col], w),
                                         ncol=w, byrow=FALSE))
                    } else {
                        herd$plan =
                            cbind(herd$plan[, 1:(col-1)],
                                  matrix(rep(herd$plan[, col], w),
                                         ncol=w, byrow=FALSE),
                                  herd$plan[, (col+1):ncol(herd$plan)])
                    }
                }
            }

            sheep$plan = matrix(rep(sheep$plan, each=nh),
                                nrow=nrow(sheep$plan)*nh, byrow=FALSE)
            sheep$plan = t(matrix(rep(t(sheep$plan), each=nw),
                                  nrow=ncol(sheep$plan)*nw, byrow=FALSE))
            
            herd$plan[herd$plan == id] = sheep$plan
        }
        
    } else {
        if (!is_sheep(sheep)) {
            here = which(sheep$id == id)
            sheep$height[here] = height
            sheep$width[here] = width
            sheep$label[here] = label
            sheep$plot[[here]] = sheep
        } else {
            for (i in 1:length(sheep$id)) {
                id = sheep$id[i]
                here = which(herd$sheep$id == id)
                herd$sheep$height[here] = sheep$sheep$height[i]
                herd$sheep$width[here] = sheep$sheep$width[i]
                herd$sheep$label[here] = sheep$sheep$label[i]
                herd$sheep$plot[[here]] = sheep$sheep$plot[[i]]
            }
        }
    }
    
    return (herd)
}
