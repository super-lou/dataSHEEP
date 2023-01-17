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


page_diagnostic_datasheet = function (data,
                                      meta,
                                      dataEXind,
                                      metaEXind,
                                      dataEXserie,
                                      ModelGroup=NULL,
                                      icon_path="",
                                      logo_path="",
                                      df_page=NULL,
                                      Shapefiles=NULL,
                                      figdir="") {
        
    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)

    leg_width = 11
    void_width = 21 - leg_width - page_margin["l"] - page_margin["r"]


    
    info_height = 3
    chronicle_height = 3

    
    
    cm_height = 22
    cm_width = 21 - page_margin["l"] - page_margin["r"]
    
    cb_height = 1.25
    ssg_height = 1.25
    si_height = 1
    tl_height = cb_height + si_height + ssg_height
    
    foot_height = 1.25

    cm_margin = margin(t=1.2, r=0, b=2, l=0.5, "cm")
    tl_shift = c(x=3, y=0)
    cb_shift = c(x=2.5, y=0)
    ssg_shift = c(x=2.5, y=0)
    si_shift = c(x=2.5, y=0.2)

    NAME = matrix(c("info", "chronicle", "foot",
                    "info", "chronicle", "foot"),
                  ncol=2)
    WIP = FALSE


    data_obs = dplyr::distinct(dplyr::select(data,
                                             c(Code, Date, Q_obs)))
    data_obs = dplyr::rename(data_obs, Q=Q_obs)
    
    Code = levels(factor(meta$Code))
    nCode = length(Code)
    
    for (i in 1:nCode) {
        code = Code[i]
        data_code = data[data$Code == code,]
        STOCK = tibble()
        
        info = panel_info(data_obs,
                          meta,
                          Shapefiles=Shapefiles,
                          codeLight=code,
                          to_do='all',
                          zone_to_show='France')
        STOCK = add_plot(STOCK,
                         plot=info,
                         name="info",
                         height=info_height)
        
        chronicle = panel_chronicle(data_code,
                                    isSqrt=TRUE,
                                    missRect=TRUE,
                                    grid=TRUE,
                                    first=TRUE,
                                    last=FALSE)
        STOCK = add_plot(STOCK,
                         plot=chronicle,
                         name="chronicle",
                         height=chronicle_height)
        
        # res = panel_correlation_matrix(dataEX_model,
        #                                metaEX,
        #                                icon_path=icon_path,
        #                                margin=cm_margin)
        # cm = res$cm
        # subTopic_path = res$info
        # STOCK = add_plot(STOCK,
        #                  plot=cm,
        #                  name="cm",
        #                  height=cm_height)

        # STOCK = add_plot(STOCK,
        #                  plot=void(),
        #                  name="void",
        #                  width=void_width)
        
        # cb = leg_colorbar(-1, 1, Palette=Palette_rainbow(),
        #                   colorStep=6, include=TRUE,
        #                   asFrac=TRUE,
        #                   reverse=TRUE,
        #                   size_color=0.3,
        #                   dx_color=0.4,
        #                   dy_color=0.45,
        #                   height=cb_height,
        #                   width=leg_width,
        #                   shift=cb_shift,
        #                   WIP=WIP)
        # STOCK = add_plot(STOCK,
        #                  plot=cb,
        #                  name="cb",
        #                  height=cb_height,
        #                  width=leg_width)

        # ssg = leg_shape_size_gradient(shape="rect",
        #                               Size=c(0.1, 0.15, 0.2, 0.25),
        #                               color=IPCCgrey50,
        #                               labelArrow="Plus corrélé",
        #                               dx_shape=0.2,
        #                               dy_shape=0.1,
        #                               dy_arrow=0.3,
        #                               size_arrow=0.25,
        #                               dz_arrow=1,
        #                               dl_arrow=0,
        #                               dr_arrow=0,
        #                               dx_text=0.3, 
        #                               height=ssg_height,
        #                               width=leg_width,
        #                               shift=ssg_shift,
        #                               WIP=WIP)
        # STOCK = add_plot(STOCK,
        #                  plot=ssg,
        #                  name="ssg",
        #                  height=ssg_height,
        #                  width=leg_width)


        

        footName = paste0('fiche station : ', code)
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

        res = merge_panel(STOCK, NAME=NAME,
                          page_margin=page_margin,
                          paper_size="A4",
                          hjust=0, vjust=1)

        plot = res$plot
        paper_size = res$paper_size

        print(paper_size)

        filename = paste0("diagnostic_datasheet_", code, ".pdf")

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
