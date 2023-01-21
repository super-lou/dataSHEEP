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

panel_indicator_distribution = function (dataEXind,
                                         metaEXind,
                                         Colors,
                                         codeLight,
                                         icon_path,
                                         title="",
                                         alpha=0.7,
                                         dTitle=0,
                                         graph="bar",
                                         margin_add=margin(t=0, r=0, b=0, l=0, "mm")) {

    

    dx_grid = 0.2
    dx_label = 0.05
    dx_label_out = 0.4
    
    dx_bar = 0.1
    dx_cross_back = 0.05
    dx_cross = 0.035

    ech_bar = 2.6

    dy_arrow = 0.045
    dl_arrow = 0.07
    dx_arrow = 0.8
    
    ymin_grid = -1*ech_bar
    ymax_grid = 2*ech_bar

    dy_icon_out = 0.4
    
    fact = 1.15

    dy_gap = 0.35
    dy_Ind = ymax_grid + dy_gap
    
    lw_mat = 0.4
    # d_W_mat = 0.25
    
    dy_L1 = 0.2
    lw_L1 = 0.25
    
    dy_I1 = 0.2
    size_I1 = 0.45
    dr_I1 = 0.15
    
    dy_T1 = 0.3
    size_T1 = 3.2
    ech_T1 = 0.23

    dy_L2_min = 0.5
    lw_L2 = 0.25
    
    dx_L3 = 0.5
    lw_L3 = 0.45

    dy_L4 = 0.3
    lw_L4 = 0.45
    
    dy_T2 = 0.27
    dy_T2line = 0.4
    size_T2 = 2.7
    ech_T2 = 7
    
    dy_I2 = 1.3
    size_I2 = 0.7

    
    ech = 1
    ech_x = 2 
    
    complete = function (X) {
        if (length(X) < 2) {
            X = c(X, NA)
        }
        return (X)
    }

    logicalCol = names(dataEXind)[sapply(dataEXind, class) == "logical"]
    dataEXind = dataEXind[!(names(dataEXind) %in% logicalCol)]
    metaEXind = metaEXind[!(metaEXind$var %in% logicalCol),]
    
    Topic = strsplit(metaEXind$topic, "/")
    Topic = lapply(Topic, complete)
    mainTopicVAR = sapply(Topic, '[[', 1)
    names(mainTopicVAR) = metaEXind$var
    lenMainTopic = rle(mainTopicVAR)$lengths
    nMainTopic = length(lenMainTopic)
    startMainTopic =
        cumsum(c(1, lenMainTopic[1:(nMainTopic-1)])) - 1 + dx_L3
    endMainTopic = cumsum(lenMainTopic) - dx_L3
    midMainTopic = (startMainTopic + endMainTopic)/2
    mainTopic = mainTopicVAR[!duplicated(mainTopicVAR)]
    mainTopic_icon = lapply(
        file.path(icon_path, paste0(gsub(" ", "_", mainTopic), ".svg")),
        svgparser::read_svg)
    names(mainTopic_icon) = mainTopic

    vars2keep = names(dataEXind)
    vars2keep = vars2keep[!grepl("([_]obs)|([_]sim)", vars2keep)]

    dataEXind = dplyr::mutate(dataEXind,
                              dplyr::across(where(is.logical),
                                            as.numeric),
                              .keep="all")

    dataEXind = dplyr::select(dataEXind, vars2keep)

    Model = levels(factor(dataEXind$Model))
    nModel = length(Model)

    dataEXind_tmp = dataEXind
    dataEXind_tmp = dplyr::select(dataEXind_tmp, -c(Code, Model))

    matchVar = match(names(dataEXind_tmp), metaEXind$var)
    matchVar = matchVar[!is.na(matchVar)]
    dataEXind_tmp = dataEXind_tmp[matchVar]

    nameCol = names(dataEXind_tmp)
    Var = nameCol
    nVar = length(Var)

    VarTEX = gsub("etiage", "étiage", Var)
    for (i in 1:nVar) {
        var = VarTEX[i]
        
        if (grepl("[_]", var) & !grepl("[_][{]", var)) {
            var = gsub("[_]", "$_{$", var)
            var = paste0(var, "}")
        } else if (grepl("[_]", var) & grepl("[_][{]", var)) {
            var = gsub("[_][{]", "$_{$", var)
        }

        if (grepl("alpha", var)) {
            var = gsub("alpha", "\\\\bf{\u03b1}", var)
        }

        if (grepl("epsilon", var)) {
            var = gsub("epsilon", "\\\\bf{\u03b5}", var)
        }

        if (grepl("HYP", var)) {
            var = gsub("HYP", "\\\\textit{H}", var)
        }

        if (grepl("inv", var) & !grepl("inv[{]", var)) {
            var = gsub("inv", "\\\\textit{inv}", var)
        } else if (grepl("inv", var) & grepl("inv[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("inv[{]", "\\\\textit{inv}", var)
        } 

        if (grepl("log", var) & !grepl("log[{]", var)) {
            var = gsub("log", "\\\\textit{log}", var)
        } else if (grepl("log", var) & grepl("log[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("log[{]", "\\\\textit{log}", var)
        } 

        if (grepl("mean", var) & !grepl("mean[{]", var)) {
            var = gsub("mean", "\\\\textit{moy}", var)
        } else if (grepl("mean", var) & grepl("mean[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("mean[{]", "\\\\textit{moy}", var)
        } 

        if (grepl("median", var) & !grepl("median[{]", var)) {
            var = gsub("median", "\\\\textit{med}", var)
        } else if (grepl("median", var) & grepl("median[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("median[{]", "\\\\textit{med}", var)
        } 
        
        if (grepl("sqrt", var) & !grepl("sqrt[{]", var)) {
            var = gsub("sqrt", "\\\\textit{sqrt}", var)
        } else if (grepl("sqrt", var) & grepl("sqrt[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("sqrt[{]", "\\\\textit{sqrt}", var)
        } 
        
        VarTEX[i] = var
    }
    VarTEX = paste0("\\textbf{", VarTEX, "}")

    Ind = ggplot() + theme_void() + coord_fixed(clip="off") +
        theme(plot.margin=margin_add,
              plot.title=element_text(size=9,
                                      vjust=0, hjust=dTitle,
                                      color=IPCCgrey25))

    VarRAW = metaEXind$var
    VarRAW = gsub("median", "med", VarRAW)
    VarRAW = gsub("mean", "moy", VarRAW)
    VarRAW = gsub("HYP", "H", VarRAW)
    VarRAW = gsub("alpha", "A", VarRAW)
    VarRAW = gsub("epsilon", "E", VarRAW)
    OK_ = grepl("[_]", VarRAW)
    tmp = gsub("^.*[_]", "", VarRAW)
    tmp = gsub("([{])|([}])", "", tmp)
    tmp[!OK_] = ""
    tmp = gsub("[[:alnum:]]", "*", tmp)
    VarRAW[OK_] = gsub("[{].*[}]", "", VarRAW[OK_])
    VarRAW[!OK_] = gsub("([{])|([}])", "", VarRAW[!OK_])
    VarRAW = gsub("[_].*$", "", VarRAW)
    VarRAW = paste0(VarRAW, tmp)
    VarRAW = strsplit(VarRAW, "*")

    convert2space = function (X) {
        X = gsub("[[:digit:]]", "1.1", X)
        X = gsub("[[:upper:]]", "1.6", X)
        X = gsub("[[:lower:]]", "1.1", X)
        X = gsub("([-])|([,])", "0.5", X)
        X = gsub("([*])", "0.9", X)
        return (X)    
    }

    Space = lapply(VarRAW, convert2space)
    Space = lapply(Space, as.numeric)
    Space = lapply(Space, sum)
    Space = unlist(Space)
    maxSpace = max(Space)

    dy = dy_Ind

    
    Ind = Ind +
        ggtitle(title)

    
    # grid line
    major_tick = c(-1, -0.5, 0, 0.5, 1, 1.5, 2)
    for (t in major_tick) {
        color = IPCCgrey85
        size = 0.2
        Ind = Ind +
            annotate("line",
                     x=(c(-dx_grid, nVar+dx_grid))*ech_x,
                     y=c(t, t)*ech_bar,
                     color=color,
                     size=size,
                     lineend="round") +
            annotate("text",
                     x=(-dx_grid-dx_label)*ech_x,
                     y=t*ech_bar,
                     label=t,
                     hjust=1,
                     vjust=0.5,
                     color=IPCCgrey40,
                     size=3)
    }

    ref_tick = c(KGE=1, NSE=1, Rc=1, epsilon=1)
    
    for (i in 1:nVar) {
        var = Var[i]

        varRAW = gsub("[{]", "[{]", var)
        varRAW = gsub("[}]", "[}]", varRAW)
        varRAW = gsub("[_]", "[_]", varRAW)
        tickOk = sapply(names(ref_tick), grepl, x=varRAW)
        
        if (any(tickOk)) {
            tick = ref_tick[tickOk]
        } else {
            tick = 0
        }
        color = IPCCgrey25
        size = 0.4
        Ind = Ind +
            annotate("line",
                     x=c(i-1, i)*ech_x,
                     y=c(tick, tick)*ech_bar,
                     color=color,
                     size=size,
                     lineend="round")
        
        if (graph == "bar") {
            Alpha = c(0.4, 0.6, 0.8)
            P = c(0.9, 0.8, 0.6)
                            
            for (j in 1:nModel) {
                model = Model[j]
                dataEXind_model = dataEXind[dataEXind$Model == model,]
                Q_model = quantile(dataEXind_model[[var]]*ech_bar,
                                   probs=c(max(P), 1-max(P)),
                                   na.rm=TRUE)
                Q_model[Q_model < ymin_grid] = ymin_grid
                Q_model[Q_model > ymax_grid] = ymax_grid
                Ind = Ind +
                    annotate("line",
                             x=rep((i-1) + 0.5 -
                                   (nModel/2)*dx_bar+dx_bar/2 +
                                   (j-1)*dx_bar,
                                   2)*ech_x,
                             y=Q_model,
                             color="white",
                             linewidth=2,
                             lineend="round")
            }
            
            for (j in 1:nModel) {
                model = Model[j]
                dataEXind_model = dataEXind[dataEXind$Model == model,]
                for (k in 1:length(P)) {
                    Q_model = quantile(dataEXind_model[[var]]*ech_bar,
                                       probs=c(P[k], 1-P[k]),
                                       na.rm=TRUE)
                    Q_model[Q_model < ymin_grid] = ymin_grid
                    Q_model[Q_model > ymax_grid] = ymax_grid
                    Ind = Ind +
                        annotate("line",
                                 x=rep((i-1) + 0.5 -
                                       (nModel/2)*dx_bar+dx_bar/2 +
                                       (j-1)*dx_bar,
                                       2)*ech_x,
                                 y=Q_model,
                                 color=Colors[names(Colors) == model],
                                 linewidth=1.5,
                                 alpha=Alpha[k],
                                 lineend="round")
                }

                dataEXind_model_code =
                    dataEXind_model[dataEXind_model$Code == codeLight,]
                if (nrow(dataEXind_model_code) != 0) {
                    Ind = Ind +
                        annotate("line",
                                 x=c((i-1) + 0.5 -
                                     (nModel/2)*dx_bar+dx_bar/2 +
                                     (j-1)*dx_bar - dx_cross_back,
                                     (i-1) + 0.5 -
                                     (nModel/2)*dx_bar+dx_bar/2 +
                                     (j-1)*dx_bar + dx_cross_back)*ech_x,
                                 y=rep(dataEXind_model_code[[var]],
                                       2)*ech_bar,
                                 color="white",
                                 linewidth=1)   
                }
            }

            for (j in 1:nModel) {
                model = Model[j]
                dataEXind_model = dataEXind[dataEXind$Model == model,]
                dataEXind_model_code =
                    dataEXind_model[dataEXind_model$Code == codeLight,]
                if (nrow(dataEXind_model_code) != 0) {
                    Ind = Ind +
                        annotate("line",
                                 x=c((i-1) + 0.5 -
                                     (nModel/2)*dx_bar+dx_bar/2 +
                                     (j-1)*dx_bar - dx_cross,
                                     (i-1) + 0.5 -
                                     (nModel/2)*dx_bar+dx_bar/2 +
                                     (j-1)*dx_bar + dx_cross)*ech_x,
                                 y=rep(dataEXind_model_code[[var]],
                                       2)*ech_bar,
                                 color=Colors[names(Colors) == model],
                                 alpha=alpha,
                                 linewidth=0.3,
                                 lineend="round")

                    above = dataEXind_model_code[[var]] > ymax_grid
                    below = ymin_grid > dataEXind_model_code[[var]]
                    if (above | below) {
                        x = ((i-1) + 0.5 -
                             (nModel/2)*dx_bar+dx_bar/2 +
                             (j-1)*dx_bar)*ech_x

                        Q_model =
                            quantile(dataEXind_model[[var]]*ech_bar,
                                           probs=c(max(P), 1-max(P)),
                                           na.rm=TRUE)
                        Q_model[Q_model < ymin_grid] = ymin_grid
                        Q_model[Q_model > ymax_grid] = ymax_grid
                        
                        if (above) {
                            y = max(Q_model) +
                                dy_arrow*ech_bar
                            yend = max(Q_model) +
                                (dy_arrow +
                                 dl_arrow)*ech_bar
                        } else if (below) {
                            y = min(Q_model) -
                                dy_arrow*ech_bar
                            yend = min(Q_model) -
                                (dy_arrow +
                                 dl_arrow)*ech_bar
                        }
                        
                        Ind = Ind +
                            annotate("segment",
                                     x=x, xend=x,
                                     y=y, yend=yend,
                                     color=
                                         Colors[names(Colors) == model],
                                     alpha=alpha,
                                     linewidth=0.3,
                                     arrow=arrow(length=unit(dx_arrow,
                                                             "mm")),
                                     lineend="round")
                        
                    }
                }
            }
        }

        
        if (graph == "violon") {
            DX = list()
            DY = list()
            for (j in 1:nModel) {
                model = Model[j]
                dataEXind_model = dataEXind[dataEXind$Model == model,]
                D = density(dataEXind_model[[var]],
                            n=1000, from=ymin_grid, to=ymax_grid,
                            na.rm=TRUE)
                DX_model = D$x
                DY_model = D$y
                DX = append(DX, list(DX_model))
                DY = append(DY, list(DY_model))
            }

            DYmaxAbs = lapply(DY, abs)
            DYmaxAbs = lapply(DYmaxAbs, max, na.rm=TRUE)
            DYmaxAbs = max(unlist(DYmaxAbs), na.rm=TRUE)

            norm = function (X) {
                return (X/(fact*DYmaxAbs))
            }
            DY = lapply(DY, norm)
            
            for (j in 1:nModel) {
                model = Model[j]
                DX_model = DX[[j]]
                DY_model = DY[[j]]
                Ind = Ind +
                    annotate("path",
                             x=((i-1) + 0.5)*ech_x + DY_model,
                             y=DX_model,
                             color="white",
                             linewidth=1,
                             lineend="round") +
                    annotate("path",
                             x=((i-1) + 0.5)*ech_x - DY_model,
                             y=DX_model,
                             color="white",
                             linewidth=1,
                             lineend="round")
            }
            for (j in 1:nModel) {
                model = Model[j]
                DX_model = DX[[j]]
                DY_model = DY[[j]]
                Ind = Ind +
                    annotate("path",
                             x=((i-1) + 0.5)*ech_x + DY_model,
                             y=DX_model,
                             color=Colors[names(Colors) == model],
                             linewidth=0.6,
                             alpha=alpha,
                             lineend="round") +
                    annotate("path",
                             x=((i-1) + 0.5)*ech_x - DY_model,
                             y=DX_model,
                             color=Colors[names(Colors) == model],
                             linewidth=0.6,
                             alpha=alpha,
                             lineend="round")
            }
        }
    }
    
    
    for (i in 1:nVar) { 
        
        Ind = Ind +
            
            annotate("line",
                     x=rep((i-1) + 0.5, 2)*ech_x,
                     y=c(dy,
                         dy + dy_L1 + dy_I1/2)*ech,
                     linewidth=lw_L1, color=IPCCgrey67) +
            
            gg_circle(r=dr_I1*ech,
                      xc=((i-1) + 0.5)*ech_x,
                      yc=(dy + dy_L1 + dy_I1)*ech,
                      color=IPCCgrey67, linewidth=lw_L1,
                      fill="white") +
            
            annotate("line",
                     x=rep((i-1) + 0.5, 2)*ech_x,
                     y=c(dy + dy_gap +
                         dy_L1 + dy_I1 + dy_T1,
                         dy +
                         dy_L1 + dy_I1 + dy_T1 + 
                         maxSpace*ech_T1 + dy_L2_min)*ech,
                     linewidth=lw_L1, color=IPCCgrey67) +
            
            annotate("rect",
                     xmin=((i-1) + 0.1)*ech_x,
                     xmax=((i-1) + 0.9)*ech_x,
                     ymin=(dy +
                           dy_L1 + dy_I1 + dy_T1)*ech,
                     ymax=(dy +
                           dy_L1 + dy_I1 + dy_T1 +
                           Space[i]*ech_T1)*ech,
                     fill="white",
                     color=NA) +
            
            annotate("text",
                     x=((i-1) + 0.5)*ech_x,
                     y=(dy +
                        dy_L1 + dy_I1 + dy_T1)*ech,
                     label=TeX(VarTEX[i]),
                     hjust=0, vjust=0.675,
                     angle=90,
                     size=size_T1,
                     color=IPCCgrey40)
    }

    dy = dy + dy_L1 + dy_I1 + dy_T1 + maxSpace*ech_T1 + dy_L2_min


    nLine = c()
    for (i in 1:nMainTopic) {
        nLim = as.integer((endMainTopic[i] - startMainTopic[i])*ech_T2)
        label = guess_newline(mainTopic[i], nLim=nLim)
        nLine = c(nLine, length(label))
    }
    dy_I2 = dy_I2 + dy_T2line*max(nLine)

    for (i in 1:nMainTopic) {
        
        nLim = as.integer((endMainTopic[i] - startMainTopic[i])*ech_T2)
        label = guess_newline(mainTopic[i], nLim=nLim)
        label =  rev(unlist(strsplit(label, "\n")))
        nLine = length(label)
        
        Ind = Ind +
            annotation_custom(
                mainTopic_icon[[i]],
                xmin=midMainTopic[i]*ech_x - size_I2*ech,
                xmax=midMainTopic[i]*ech_x + size_I2*ech,
                ymin=(dy + 
                      dy_L4 + dy_I2 - size_I2)*ech,
                ymax=(dy + 
                      dy_L4 + dy_I2 + size_I2)*ech)

        for (j in 1:nLine) {
            Ind = Ind +
                annotate("text",
                         x=midMainTopic[i]*ech_x,
                         y=(dy + 
                            dy_L4 + dy_T2 +
                            (j-1)*dy_T2line)*ech,
                         hjust=0.5, vjust=0,
                         angle=0,
                         label=label[j],
                         fontface="bold",
                         size=size_T2,
                         color=IPCCgrey05)
        }

        Ind = Ind +
            annotate("line",
                     x=c(midMainTopic[i], midMainTopic[i])*ech_x,
                     y=c(dy,
                         dy + dy_L4)*ech,
                     linewidth=lw_L4, color=IPCCgrey48,
                     lineend="round") +

    annotate("line",
             x=c(startMainTopic[i], endMainTopic[i])*ech_x,
             y=rep(dy, 2)*ech,
             linewidth=lw_L3, color=IPCCgrey48,
             lineend="round")
    }

    dy = dy + dy_L4 + dy_I2 + size_I2
    
    
    Ind = Ind +
        
        scale_x_continuous(
            limits=c((-dx_grid-dx_label-dx_label_out)*ech_x,
                     NA),
            expand=c(0, 0)) +
        
        scale_y_continuous(
            limits=c(ymin_grid-dy_gap*ech, (dy+dy_icon_out)*ech),
            expand=c(0, 0))

    return (Ind)
}
