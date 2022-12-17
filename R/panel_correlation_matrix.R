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

panel_correlation_matrix = function (dataEx2D_model,
                                     metaVAR,
                                     icon_path,
                                     level=0.1,
                                     cm_margin=margin(t=0, r=0,
                                                      b=0, l=0,
                                                      "cm"),
                                     ...) {

    lw_mat = 0.4
    lw_topic = 0.6
    
    dt = 0.25
    size = 3.2
    size_icon = 1
    size_topic = 3
    
    dy_icon_topic = 2.2
    dy_text_topic = 0.4
    dy_line_mainTopic = 0.5
    dy_line_subTopic = 1
    dy_line_topic = 0.5

    ech = 25
    ech_char = 0.7
    ech_newline = 2
    
    complete = function (X) {
        if (length(X) < 2) {
            X = c(X, NA)
        }
        return (X)
    }
    Topic = strsplit(metaVAR$topic, "/")
    Topic = lapply(Topic, complete)
    mainTopicVAR = sapply(Topic, '[[', 1)
    names(mainTopicVAR) = metaVAR$var
    lenMainTopic = rle(mainTopicVAR)$lengths
    nMainTopic = length(lenMainTopic)
    startMainTopic =
        cumsum(c(1, lenMainTopic[1:(nMainTopic-1)])) - 1 + dy_line_topic
    endMainTopic = cumsum(lenMainTopic) - dy_line_topic
    midMainTopic = (startMainTopic + endMainTopic)/2
    mainTopic = mainTopicVAR[!duplicated(mainTopicVAR)]
    
    subTopicVAR = sapply(Topic, '[[', 2)
    names(subTopicVAR) = metaVAR$var
    subTopic = subTopicVAR[!duplicated(subTopicVAR)]

    mainTopic_icon = lapply(
        file.path(icon_path, paste0(gsub(" ", "_", mainTopic), ".svg")),
        svgparser::read_svg)
    subTopic_icon = lapply(
        file.path(icon_path, paste0(gsub(" ", "_", subTopic), ".svg")),
        svgparser::read_svg)
    
    names(mainTopic_icon) = mainTopic
    names(subTopic_icon) = subTopic
    
    vars2keep = names(dataEx2D_model)
    vars2keep = vars2keep[!grepl("([_]obs)|([_]sim)", vars2keep)]

    dataEx2D_model = dplyr::mutate(dataEx2D_model,
                                   dplyr::across(where(is.logical),
                                                 as.numeric),
                                   .keep="all")
    
    dataEx2D_model = dplyr::select(dataEx2D_model, vars2keep)
    nameRow = dataEx2D_model$Code
    dataEx2D_model = dplyr::select(dataEx2D_model, -c(Code, Model))

    dataEx2D_model = dataEx2D_model[match(names(dataEx2D_model),
                                          metaVAR$var)]

    nameCol = names(dataEx2D_model)
    Var = nameCol
    nVar = ncol(dataEx2D_model)

    nCol = ncol(dataEx2D_model)
    col2rm = c()
    for (i in 1:nCol) {
        if (sum(!is.na(dataEx2D_model[[i]])) < 3) {
            col2rm = c(col2rm, names(dataEx2D_model)[i])
        }
    }
    if (!is.null(col2rm)) {
        dataEx2D_model = dplyr::select(dataEx2D_model, -col2rm)
        nameCol = names(dataEx2D_model)
    }
    
    dataEx2D_model = as.matrix(dataEx2D_model)

    colnames(dataEx2D_model) = nameCol
    rownames(dataEx2D_model) = nameRow

    CORRmat = cor(dataEx2D_model,
                  method="spearman",
                  use="pairwise.complete.obs")
    
    Pmat = corrplot::cor.mtest(dataEx2D_model,
                               conf.level=1-level,
                               method="spearman",
                               use="pairwise.complete.obs")$p

    if (!is.null(col2rm)) {
        nCol2add = length(col2rm)
        nVarCORR = length(colnames(CORRmat))
        
        for (i in 1:nCol2add) {
            missVar = col2rm[i]
            id = which(Var == missVar)
            CORRmat = rbind(CORRmat[1:(id-1),],
                            rep(NA, nVarCORR),
                            CORRmat[id:nrow(CORRmat),])
            rownames(CORRmat)[id] = missVar
            Pmat = rbind(Pmat[1:(id-1),],
                         rep(NA, nVarCORR),
                         Pmat[id:nrow(Pmat),])
            rownames(Pmat)[id] = missVar
            
        }
        
        for (i in 1:nCol2add) {
            missVar = col2rm[i]
            id = which(Var == missVar)
            CORRmat = cbind(CORRmat[, 1:(id-1)],
                            rep(NA, nVarCORR+nCol2add),
                            CORRmat[, id:ncol(CORRmat)])
            colnames(CORRmat)[id] = missVar
            Pmat = cbind(Pmat[, 1:(id-1)],
                         rep(NA, nVarCORR+nCol2add),
                         Pmat[, id:ncol(Pmat)])
            colnames(Pmat)[id] = missVar
        }
    }

    Colors = get_color(CORRmat, -1, 1,
                       Palette=Palette_rainbow(),
                       colorStep=6, include=TRUE,
                       reverse=TRUE)
    
    COLORmat = matrix(Colors, nrow=nrow(CORRmat), ncol=ncol(CORRmat))
    SIZEmat = (abs(CORRmat))^(1/6)*ech
    Xmat = matrix(rep(0:(nVar-1)*ech, nVar), nrow=nVar, byrow=TRUE) + 0.5*ech
    Ymat = matrix(rep((nVar-1):0*ech, nVar), nrow=nVar) + 0.5*ech
    XMINmat = Xmat - SIZEmat/2
    XMAXmat = Xmat + SIZEmat/2
    YMINmat = Ymat - SIZEmat/2
    YMAXmat = Ymat + SIZEmat/2

    COLOR = unlist(as.list(COLORmat))
    XMIN = unlist(as.list(XMINmat))
    XMAX = unlist(as.list(XMAXmat))
    YMIN = unlist(as.list(YMINmat))
    YMAX = unlist(as.list(YMAXmat))

    nOKPmat = Pmat > level
    nOKPmat[!nOKPmat] = NA
    XPSIZEmat = SIZEmat*nOKPmat/ech
    XPmat = Xmat*nOKPmat
    YPmat = Ymat*nOKPmat

    XPSIZE = unlist(as.list(XPSIZEmat))
    XPSIZE = XPSIZE[!is.na(XPSIZE)]
    XP = unlist(as.list(XPmat))
    XP = XP[!is.na(XP)]
    YP = unlist(as.list(YPmat))
    YP = YP[!is.na(YP)]

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

    cm = ggplot() + theme_void() + coord_fixed(clip="off") + 
        theme(text=element_text(family="Helvetica"),
              plot.margin=cm_margin)

    cm = cm +
        annotate("rect", xmin=XMIN, xmax=XMAX,
                 ymin=YMIN, ymax=YMAX,
                 fill=COLOR)
    
    cm = cm +
        annotate("rect", xmin=0, xmax=nVar*ech, ymin=0, ymax=nVar*ech,
                 linewidth=lw_mat, color=IPCCgrey95, fill=NA)
    for (i in 1:(nVar-1)) {
        cm = cm +
            annotate("line", x=c(0, nVar)*ech, y=c(i, i)*ech,
                     linewidth=lw_mat, color=IPCCgrey95) +
            annotate("line", x=c(i, i)*ech, y=c(0, nVar)*ech,
                     linewidth=lw_mat, color=IPCCgrey95)
    }

    cm = cm +
        annotate("point", x=XP, y=YP,
                 shape=4, size=XPSIZE, color="white")
    
    cm = cm +
        annotate("text",
                 x=rep(-dt*ech, nVar),
                 y=(nVar-1):0*ech + 0.5*ech,
                 hjust=1, vjust=0.5,
                 label=TeX(VarTEX), size=size,
                 color=IPCCgrey40) +
        
        annotate("text",
                 x=0:(nVar-1)*ech + 0.5*ech,
                 y=rep(-dt*ech, nVar),
                 hjust=1, vjust=0.5,
                 angle=90,
                 label=TeX(VarTEX), size=size,
                 color=IPCCgrey40)

    VarRAW = metaVAR$var
    VarRAW = gsub("median", "med", VarRAW)
    VarRAW = gsub("HYP", "H", VarRAW)
    VarRAW = gsub("alpha", "a", VarRAW)
    VarRAW = gsub("epsilon", "e", VarRAW)
    tmp = gsub("^.*[_]", "", VarRAW)
    tmp = gsub("([{])|([}])", "", tmp)
    tmp[!grepl("[_]", VarRAW)] = ""
    tmp = strrep(".", nchar(tmp))
    VarRAW = gsub("[{].*[}]", "", VarRAW)
    VarRAW = gsub("[_].*$", "", VarRAW)
    VarRAW = paste0(VarRAW, tmp)
    VarRAW = strsplit(VarRAW, "*")

    convert2space = function (X) {
        X = gsub("[[:digit:]]", "1", X)
        X = gsub("[[:upper:]]", "2", X)
        X = gsub("[[:lower:]]", "1", X)
        X = gsub("([.])|([-])", "1", X)
        return (X)    
    }
    Space = lapply(VarRAW, convert2space)
    Space = lapply(Space, as.numeric)
    Space = lapply(Space, sum)
    Space = unlist(Space)
    maxSpace = max(Space)
    
    for (i in 1:nMainTopic) {
        cm = cm +
            annotation_custom(
                mainTopic_icon[[i]],
                xmin=(midMainTopic[i] - size_icon)*ech,
                xmax=(midMainTopic[i] + size_icon)*ech,
                ymin=(nVar + dt + 
                      dy_line_subTopic + maxSpace*ech_char +
                      dy_line_mainTopic +
                      dy_icon_topic - size_icon)*ech,
                ymax=(nVar + dt +
                      dy_line_subTopic + maxSpace*ech_char +
                      dy_line_mainTopic +
                      dy_icon_topic + size_icon)*ech) +
            
            annotate("text",
                     x=midMainTopic[i]*ech,
                     y=(nVar + dt +
                        dy_line_subTopic + maxSpace*ech_char +
                        dy_line_mainTopic +
                        dy_text_topic)*ech,
                     hjust=0.5, vjust=0,
                     angle=0,
                     # label=guess_newline(mainTopic[i],
                     #                     round(lenMainTopic[i]*
                     #                           ech_newline)),
                     label=mainTopic[i],
                     fontface="bold",
                     size=size_topic,
                     color=IPCCgrey20) +
            
            annotate("line",
                     x=c(midMainTopic[i], midMainTopic[i])*ech,
                     y=c(nVar + dt +
                         dy_line_subTopic + maxSpace*ech_char,
                         nVar + dt +
                         dy_line_subTopic + maxSpace*ech_char +
                         dy_line_mainTopic)*ech,
                     linewidth=lw_topic, color=IPCCgrey20,
                     lineend="round") +

            annotate("line",
                     x=c(startMainTopic[i], endMainTopic[i])*ech,
                     y=rep(nVar + dt +
                           dy_line_subTopic + maxSpace*ech_char,
                           2)*ech,
                     linewidth=lw_topic, color=IPCCgrey20,
                     lineend="round")
    }
    
    for (i in 1:nVar) {
        cm = cm +
            annotate("line",
                     x=rep((i-1) + 0.5, 2)*ech,
                     y=c(nVar + dt,
                         nVar + dt +
                         maxSpace*ech_char +
                         dy_line_subTopic)*ech,
                     linewidth=lw_topic, color=IPCCgrey20,
                     lineend="round") +
            
            annotate("rect",
                     xmin=((i-1) + 0.1)*ech,
                     xmax=((i-1) + 0.9)*ech,
                     ymin=nVar*ech,
                     ymax=(nVar +
                           Space[i]*ech_char)*ech,
                     fill="white",
                     color=NA) +
        
            annotate("text",
                     x=((i-1) + 0.5)*ech,
                     y=(nVar+dt)*ech,
                     label=TeX(VarTEX[i]),
                     hjust=0, vjust=0.5,
                     angle=90,
                     size=size,
                     color=IPCCgrey40)
    }
    

    cm = cm +
        scale_x_continuous(expand=c(0, 0)) + 
        scale_y_continuous(expand=c(0, 0))
    

    cb = leg_colorbar(-1, 1, Palette=Palette_rainbow(),
                      colorStep=6, include=TRUE,
                      asFrac=TRUE,
                      reverse=TRUE,
                      ...)

    

    res = list(cm=cm, cb=cb)
    return (res)
}
