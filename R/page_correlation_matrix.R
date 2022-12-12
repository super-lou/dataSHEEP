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


page_correlation_matrix = function (dataEx2D, logo_path="", df_page=NULL, figdir='') {

    Model = levels(factor(dataEx2D$Model))
    nModel = length(Model)


    for (i in 1:nModel) {
        model = Model[i]
        print(model)

        df_P = tibble()
        var_plotted = c()
        
        dataEx2D_model = dataEx2D[dataEx2D$Model == model,]

        text = paste0("<b>", model, "</b>")
        info = richtext_grob(text,
                              x=0, y=0,
                              margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                              hjust=0, vjust=0,
                              gp=gpar(col="#00A3A8", fontsize=16))

        df_P = add_plot(df_P,
                        plot=info,
                        name="info")
        
        res = panel_correlation_matrix(dataEx2D_model)
        cm = res$cm
        cb = res$cb

        df_P = add_plot(df_P,
                        plot=cm,
                        name="cm")

        df_P = add_plot(df_P,
                        plot=cb,
                        name="cb")

        df_P = add_plot(df_P,
                        plot=void(),
                        name="void")

        info_height = 1
        cm_height = 20
        cb_height = 0.6
        foot_height = 1.25
        margin_height = 0.5
        
        footName = paste0('matrice de corrélation : ', model)
        if (is.null(df_page)) {
            n_page = i
        } else {
            if (nrow(df_page) == 0) {
                n_page = 1
            } else {
                n_page = df_page$n[nrow(df_page)] + page
            }
        }

        print("foot panel")
        
        foot = panel_foot(footName, n_page,
                          foot_height, logo_path)
        df_P = add_plot(df_P,
                        plot=foot,
                        name="foot",
                        overwrite_by_name=TRUE)

        print("adding margin")

        LM_id = c()
        LM_name = c()
        
        id_plot = which(df_P$name == "info")
        LM_id = c(LM_id, id_plot)
        LM_name = c(LM_name, df_P$name[id_plot])
        
        id_plot = which(df_P$name == "cm")
        LM_id = c(LM_id, id_plot)
        LM_name = c(LM_name, df_P$name[id_plot])

        id_plot = which(df_P$name == "cb")
        LM_id = c(LM_id, id_plot)
        LM_name = c(LM_name, df_P$name[id_plot])       

        id_plot = which(df_P$name == "void")
        LM_id = c(LM_id, id_plot)
        LM_name = c(LM_name, df_P$name[id_plot])

        id_plot = which(df_P$name == "foot")
        LM_id = c(LM_id, id_plot)
        LM_name = c(LM_name, df_P$name[id_plot])

        P = df_P$plot[LM_id[!is.na(LM_id)]]

        LM_name = matrix(LM_name)
        LM_id[!is.na(LM_id)] = 1:length(LM_id[!is.na(LM_id)])
        LM_id = matrix(LM_id)                

        LMcol = ncol(LM_id)
        LM_id = rbind(rep(NA, times=LMcol), LM_id,
                      rep(NA, times=LMcol))
        LM_name = rbind(rep("margin", times=LMcol), LM_name,
                        rep("margin", times=LMcol))
        
        LMrow = nrow(LM_id)
        LM_id = cbind(rep(NA, times=LMrow), LM_id,
                      rep(NA, times=LMrow))
        LM_name = cbind(rep("margin", times=LMrow), LM_name,
                        rep("margin", times=LMrow))
        LMcol = ncol(LM_id)

        print("paper cutting")

        paper_size='A4'
        if (paper_size == 'A4') {
            width = 21
            height = 29.7
        } else if (is.vector(paper_size) & length(paper_size) > 1) {
            width = paper_size[1]
            height = paper_size[2]
        }
        
        Norm_ratio = height / (height - 2*margin_height - cm_height - cb_height - foot_height - info_height)

        void_height = height / Norm_ratio
        
        Hcut = LM_name[, 2]
        heightLM = rep(0, times=LMrow)

        heightLM[Hcut == "info"] = info_height
        heightLM[Hcut == "cm"] = cm_height
        heightLM[Hcut == "cb"] = cb_height
        heightLM[Hcut == "void"] = void_height
        heightLM[Hcut == "foot"] = foot_height
        heightLM[Hcut == "margin"] = margin_height

        col_width = (width - 2*margin_height) / (LMcol - 2)
        
        Wcut = LM_name[(LMrow-1),]
        widthLM = rep(col_width, times=LMcol)
        widthLM[Wcut == "margin"] = margin_height

        LM_inline = P[as.vector(LM_id)]
        
        LM_name_inline = as.vector(LM_name)

        print("arrange plot")

        plot = grid.arrange(arrangeGrob(grobs=LM_inline,
                                        nrow=LMrow,
                                        ncol=LMcol,
                                        heights=heightLM,
                                        widths=widthLM,
                                        as.table=FALSE))

        filename = paste0("correlation_", model, ".pdf")

        if (!(file.exists(figdir))) {
            dir.create(figdir, recursive=TRUE)
        }
        ggplot2::ggsave(plot=plot,
                        path=figdir,
                        filename=filename,
                        width=width, height=height, units='cm', dpi=300,
                        device=cairo_pdf)
    }
}
