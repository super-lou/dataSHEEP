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


page_correlation_matrix = function (dataEx2D, metaVAR,
                                    icon_path="", logo_path="",
                                    df_page=NULL,
                                    figdir='') {

    Model = levels(factor(dataEx2D$Model))
    nModel = length(Model)

    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)

    leg_width = 8
    tl_width = 21 - leg_width - page_margin["l"] - page_margin["r"]

    info_height = 1
    cm_height = 20
    cm_width = 21 - page_margin["l"] - page_margin["r"]
    
    cb_height = 2
    si_height = 2
    ssg_height = 2
    tl_height = cb_height + si_height + ssg_height
    
    foot_height = 1.25

    cm_margin = margin(t=1.2, r=0, b=2, l=0.5, "cm")
    tl_margin = margin(t=0, r=0, b=0, l=0, "cm")
    cb_margin = margin(t=0, r=0, b=0, l=0, "cm")
    ssg_margin = margin(t=0, r=0, b=0, l=0, "cm")
    si_margin = margin(t=0, r=0, b=0, l=0, "cm")

    NAME = matrix(c("info", "cm", "cb", "ssg", "si", "foot",
                    "info", "cm", "tl", "tl", "tl", "foot"),
                  ncol=2)
    

    for (i in 1:nModel) {
        model = Model[i]
        print(model)

        STOCK = tibble()
        var_plotted = c()
        
        dataEx2D_model = dataEx2D[dataEx2D$Model == model,]

        text = paste0(
            "<b>Matrice de corrélation des critères d'évaluation</b><br>",
            model)
        info = richtext_grob(text,
                             x=0, y=1,
                             margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                             hjust=0, vjust=1,
                             gp=gpar(col="#00A3A8", fontsize=16))
        STOCK = add_plot(STOCK,
                         plot=info,
                         name="info",
                         height=info_height)
        
        res = panel_correlation_matrix(dataEx2D_model,
                                       metaVAR,
                                       icon_path=icon_path,
                                       margin=cm_margin)
        cm = res$cm
        subTopic_path = res$info
        STOCK = add_plot(STOCK,
                         plot=cm,
                         name="cm",
                         height=cm_height)

        tl = leg_shape_info(Shape=subTopic_path,
                            Size=1.2,
                            Label=names(subTopic_path),
                            dy_icon=1,
                            dx_label=0.2,
                            height=tl_height,
                            width=tl_width,
                            margin=tl_margin)
        STOCK = add_plot(STOCK,
                         plot=tl,
                         name="tl",
                         width=tl_width)

        cb = leg_colorbar(-1, 1, Palette=Palette_rainbow(),
                          colorStep=6, include=TRUE,
                          asFrac=TRUE,
                          reverse=TRUE,
                          height=cb_height,
                          width=leg_width,
                          margin=cb_margin)
        STOCK = add_plot(STOCK,
                         plot=cb,
                         name="cb",
                         height=cb_height,
                         width=leg_width)

        ssg = leg_shape_size_gradient(shape="rect",
                                      Size=c(1, 1.5, 2, 2.5),
                                      color=IPCCgrey50,
                                      labelArrow="Plus corrélé",
                                      dy_arrow=1,
                                      size_arrow=0.25,
                                      dz_arrow=2,
                                      dx_text=0.2,
                                      dy_text=2,
                                      height=ssg_height,
                                      width=leg_width,
                                      margin=ssg_margin)
        STOCK = add_plot(STOCK,
                         plot=ssg,
                         name="ssg",
                         height=ssg_height,
                         width=leg_width)

        si = leg_shape_info(Shape="rect",
                            Size=1,
                            Color=IPCCgrey50,
                            Label=c(
                                "Significatif à un risque de 10 %",
                                "Non significatif à un risque de 10 %"),
                            Cross=c(FALSE, TRUE),
                            dy_icon=1,
                            dx_label=0.2,
                            height=si_height,
                            width=leg_width,
                            margin=si_margin)
        STOCK = add_plot(STOCK,
                         plot=si,
                         name="si",
                         height=si_height,
                         width=leg_width)

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
        foot = panel_foot(footName, n_page,
                          foot_height, logo_path)
        STOCK = add_plot(STOCK,
                         plot=foot,
                         name="foot",
                         height=foot_height)

        # STOCK = add_plot(STOCK,
        #                  plot=void(),
        #                  name="void",
        #                  width=void_width)

        res = merge_panel(STOCK, NAME=NAME,
                          page_margin=page_margin,
                          paper_size="A4",
                          hjust=0, vjust=1)

        plot = res$plot
        paper_size = res$paper_size

        print(paper_size)

        filename = paste0("correlation_", model, ".pdf")

        if (!(file.exists(figdir))) {
            dir.create(figdir, recursive=TRUE)
        }
        ggplot2::ggsave(plot=plot,
                        path=figdir,
                        filename=filename,
                        width=paper_size[1],
                        height=paper_size[2], units='cm',
                        dpi=300,
                        device=cairo_pdf)
    }
}
