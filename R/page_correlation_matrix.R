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

    info_height = 1
    cm_height = 20
    leg_width = 4
    void_width = 16
    cb_height = 1
    ssg_height = 1
    foot_height = 1.25
    margin_size = 0.5

    for (i in 1:nModel) {
        model = Model[i]
        print(model)

        STOCK = tibble()
        var_plotted = c()
        
        dataEx2D_model = dataEx2D[dataEx2D$Model == model,]

        text = paste0("<b>", model, "</b>")
        info = richtext_grob(text,
                             x=0, y=0,
                             margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                             hjust=0, vjust=0,
                             gp=gpar(col="#00A3A8", fontsize=16))

        STOCK = add_plot(STOCK,
                         plot=info,
                         name="info",
                         height=info_height)
        
        res = panel_correlation_matrix(dataEx2D_model)
        cm = res$cm
        cb = res$cb

        STOCK = add_plot(STOCK,
                         plot=cm,
                         name="cm",
                         height=cm_height)

        STOCK = add_plot(STOCK,
                         plot=cb,
                         name="cb",
                         height=cb_height,
                         width=leg_width)

        ssg = leg_shape_size_gradient(shape="rect",
                                      Size=c(0.2, 0.3, 0.4, 0.5),
                                      color="grey50",
                                      labelArrow="plus corrélé")

        STOCK = add_plot(STOCK,
                         plot=ssg,
                         name="ssg",
                         height=ssg_height,
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

        STOCK = add_plot(STOCK,
                         plot=void(),
                         name="void",
                         # height=ssg_height,
                         width=void_width)

        NAME = matrix(c("info", "cm", "void", "void", "foot",
                        "info", "cm", "cb", "ssg", "foot"), nrow=5)

        res = merge_panel(STOCK, NAME=NAME,
                          margin_size=margin_size,
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
