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
                                      Colors,
                                      ModelGroup=NULL,
                                      icon_path="",
                                      Warnings=NULL,
                                      logo_path="",
                                      df_page=NULL,
                                      Shapefiles=NULL,
                                      figdir="") {
        
    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)

    leg_width = 11

    
    info_height = 3
    chronicle_height = 3
    medQJ_height = 7
    FDC_height = 7
    Ind_height = 15

    foot_height = 1.25
    
    # legColor_height = 29.7 - info_height - chronicle_height - medQJ_height - Ind_height - foot_height - page_margin["l"] - page_margin["r"]
    legColor_height = 3
    
    
    void_height = legColor_height
    
    medQJ_width = 10
    FDC_width = 10
    legColor_width = 5

    void_width = 21 - legColor_width - page_margin["l"] - page_margin["r"]
    

    
    cm_height = 22
    cm_width = 21 - page_margin["l"] - page_margin["r"]
    
    cb_height = 1.25
    ssg_height = 1.25
    si_height = 1
    tl_height = cb_height + si_height + ssg_height
    
    

    cm_margin = margin(t=1.2, r=0, b=2, l=0.5, "cm")
    tl_shift = c(x=3, y=0)
    cb_shift = c(x=2.5, y=0)
    ssg_shift = c(x=2.5, y=0)
    si_shift = c(x=2.5, y=0.2)

    NAME = matrix(c(
        "info", "chronicle", "medQJ", "Ind", "legColor", "foot",
        "info", "chronicle", "FDC", "Ind", "void", "foot"),
    ncol=2)
    WIP = FALSE


    data_obs = dplyr::distinct(dplyr::select(data,
                                             c(Code, Date, Q_obs)))
    data_obs = dplyr::rename(data_obs, Q=Q_obs)

    Model = levels(factor(dataEXind$Model))
    nModel = length(Model)
                   
    Code = levels(factor(meta$Code))
    nCode = length(Code)
    
    for (i in 1:nCode) {
        code = Code[i]
        data_code = data[data$Code == code,]

        dataEXserie_code = list()
        for (j in 1:length(dataEXserie)) {
            dataEXserie_code = append(
                dataEXserie_code,
                list(dataEXserie[[j]][dataEXserie[[j]]$Code == code,]))
        }
        names(dataEXserie_code) = names(dataEXserie)

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
        
        chronicle = panel_spaghetti(data_code,
                                    title="(a) Débit journalier",
                                    unit="m^{3}.s^{-1}",
                                    alpha=0.35,
                                    isSqrt=TRUE,
                                    missRect=TRUE,
                                    isBack=FALSE,
                                    isTitle=TRUE,
                                    dTitle=-0.04,
                                    sizeYticks=6,
                                    date_labels="%Y",
                                    breaks="10 years",
                                    minor_breaks="2 years",
                                    isBackObsAbove=FALSE,
                                    grid=TRUE,
                                    margin_add=margin(t=1, r=0, b=0, l=0, "mm"),
                                    first=FALSE,
                                    last=TRUE)
        STOCK = add_plot(STOCK,
                         plot=chronicle,
                         name="chronicle",
                         height=chronicle_height)

        dataMOD = dataEXserie_code[["median{QJ}"]]
        dataMOD$Date = as.Date(dataMOD$Yearday-1,
                               origin=as.Date("1972-01-01"))
        dataMOD = dplyr::rename(dataMOD,
                                Q_obs="median{QJ}_obs",
                                Q_sim="median{QJ}_sim")
        medQJ = panel_spaghetti(dataMOD,
                                Colors,
                                title="(b) Débit journalier médian inter-annuel",
                                unit="m^{3}.s^{-1}",
                                alpha=0.85,
                                isSqrt=TRUE,
                                missRect=FALSE,
                                isBack=FALSE,
                                isTitle=TRUE,
                                dTitle=-0.3,
                                date_labels="%d/%m",
                                breaks="3 months",
                                minor_breaks="1 months",
                                Xlabel="",
                                isBackObsAbove=TRUE,
                                grid=TRUE,
                                margin_add=
                                    margin(t=0, r=3.5, b=0, l=0, "mm"),
                                first=FALSE,
                                last=TRUE)
        STOCK = add_plot(STOCK,
                         plot=medQJ,
                         name="medQJ",
                         height=medQJ_height,
                         width=medQJ_width)

        dataMOD = dataEXserie_code[["FDC"]]
        dataMOD = dplyr::rename(dataMOD,
                                Date="FDC_obs_p",
                                Q_obs="FDC_obs_Q",
                                Q_sim="FDC_sim_Q")
        FDC = panel_spaghetti(dataMOD,
                              Colors,
                              title="(c) Courbe des débits classés",
                              unit="m^{3}.s^{-1}",
                              alpha=0.85,
                              isSqrt=TRUE,
                              missRect=FALSE,
                              isTitle=TRUE,
                              isBack=FALSE,
                              dTitle=-0.24,
                              breaks=0.2,
                              minor_breaks=0.1,
                              break_round=1,
                              Xlabel="Probabilité de dépassement",
                              isBackObsAbove=TRUE,
                              grid=TRUE,
                              margin_add=
                                  margin(t=0, r=0, b=0, l=3.5, "mm"),
                              first=FALSE,
                              last=TRUE)
        STOCK = add_plot(STOCK,
                         plot=FDC,
                         name="FDC",
                         height=FDC_height,
                         width=FDC_width)

        Ind = panel_indicator_distribution(
            dataEXind,
            metaEXind,
            meta,
            Colors,
            codeLight=code,
            icon_path=icon_path,
            Warnings=Warnings,
            title="(d) Critères de diagnostic",
            alpha=0.85,
            alpha_spread=0.25,
            dTitle=0.01,
            add_name=TRUE,
            margin_add=
                margin(t=0, r=0, b=0, l=0, "cm"))
        STOCK = add_plot(STOCK,
                         plot=Ind,
                         name="Ind",
                         height=Ind_height)

        STOCK = add_plot(STOCK,
                         plot=void(),
                         name="void",
                         height=void_height,
                         width=void_width)
        

        footName = paste0('Fiche station de diagnostic')
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
