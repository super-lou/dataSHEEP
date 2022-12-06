# \\\
# Copyright 2021-2022 Louis Héraut*1,
#                     Éric Sauquet*2,
#                     Valentin Mansanarez
#
# *1   INRAE, France
#      louis.heraut@inrae.fr
# *2   INRAE, France
#      eric.sauquet@inrae.fr
#
# This file is part of ash R toolbox.
#
# Ash R toolbox is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# Ash R toolbox is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ash R toolbox.
# If not, see <https://www.gnu.org/licenses/>.
# ///
#
#
# R/plotting/layout.R
#
# Regroups general parameters about plotting like the theme used ang
# color management. It mainly deals with the calling to specific
# plotting functions and the organisation of each plot for the
# generation of the PDF.


## 3. LAYOUT _________________________________________________________
# Generates a PDF that gather datasheets, map and summarize table about the trend analyses realised on selected stations
#' @title Layout panel
#' @export
layout_panel = function (data, df_meta, structure, layout_matrix,
                         to_plot=c('datasheet', 'table', 'map',
                                   'map_regime', 'map_trend', 'map_mean'),
                         figdir='', filedir_opt='', filename_opt='',
                         variable='', df_trend=NULL,
                         level=0.1, unit2day=365.25, var='',
                         event='', unit='', samplePeriod='',
                         glose=NULL, trend_period=NULL,
                         mean_period=NULL, colorForce=FALSE,
                         exXprob=0.01,
                         linetype_per='solid',
                         axis_xlim=NULL,
                         paper_size='A4',
                         time_header=NULL,
                         info_header=NULL, foot_note=TRUE,
                         info_height=2.8, time_height=3,
                         var_ratio=3, foot_height=1.25,
                         shapefile_list=NULL,
                         resdir=NULL,
                         logo_path=NULL,
                         zone_to_show=NULL,
                         pdf_chunk=c('all'),
                         show_colorEvent=FALSE) {

    dateFile = format(Sys.Date(), "%Y%m%d")

    # Name of the document
    outfile = "ASH"
    # If there is an option to mention in the filename it adds it
    if (filename_opt != '') {
        outfile = paste0(outfile, '_', filename_opt)
    }
    # Add the 'pdf' extensionto the name
    outfile = paste0(outfile, '_', dateFile, '.pdf')

    # If there is not a dedicated figure directory it creats one
    outdir = file.path(figdir, filedir_opt)
    if (!(file.exists(outdir))) {
        dir.create(outdir)
    }

    # If there is not a dedicated figure directory it creats one
    outdir_code = file.path(figdir, filedir_opt, paste0('ASH_', dateFile))
    if (!(file.exists(outdir_code))) {
        dir.create(outdir_code)
    }

    # Names of a temporary directory to store all the independent pages
    outdirTmp = file.path(outdir, 'tmp')
    # Creates it if it does not exist
    if (!(file.exists(outdirTmp))) {
        dir.create(outdirTmp)
    # If it already exists it deletes the pre-existent directory
    # and recreates one
    } else {
        if (!is.null(to_plot)) {
            unlink(outdirTmp, recursive=TRUE)
            dir.create(outdirTmp)
        }
    }

    outdirTmp_pdf = file.path(outdirTmp, 'pdf')
    # Creates it if it does not exist
    if (!(file.exists(outdirTmp_pdf))) {
        dir.create(outdirTmp_pdf)
    }

    outdirTmp_png = file.path(outdirTmp, 'png')
    # Creates it if it does not exist
    if (!(file.exists(outdirTmp_png))) {
        dir.create(outdirTmp_png)
    }

    # Number of type/variable
    nbp = length(data)

    # Convert data tibble to list of tibble if it is not the case
    if (all(class(data) != 'list')) {
        data = list(data)
    }

    if (all(class(df_trend) != 'list')) {
        df_trend = list(df_trend)
    }

    if (length(level) != nbp) {
        level = rep(level[1], nbp)
    }
    
    if (length(unit2day) != nbp) {
        unit2day = rep(unit2day[1], nbp)
    }

    if (length(var) != nbp) {
        var = rep(var[1], nbp)
    }

    if (length(glose) != nbp) {
        glose = rep(glose[1], nbp)
    }

    if (length(event) != nbp) {
        event = rep(event[1], nbp)
    }

    if (length(unit) != nbp) {
        unit = rep(unit[1], nbp)
    }

    if (length(samplePeriod) != nbp) {
        samplePeriod = rep(samplePeriod, nbp)
    }

    # Creates a blank list to store all the data of each type of plot
    list_df2plot = vector(mode='list', length=nbp)

    # For all the type of graph / number of studied variables
    for (i in 1:nbp) {
        # Creates a list that gather all the info for one type of graph
        df2plot = list(data=data[[i]], 
                       trend=df_trend[[i]],
                       level=level[[i]],
                       unit2day=unit2day[[i]],
                       var=var[[i]],
                       event=event[[i]],
                       unit=unit[[i]],
                       samplePeriod=samplePeriod[[i]],
                       glose=glose[[i]])
        # Stores it
        list_df2plot[[i]] = df2plot
    }

    if ('summary' %in% to_plot) {
        df_page = tibble(section='Sommaire', subsection=NA, n=1)
    } else {
        df_page = tibble()
    }
    
    # If map needs to be plot
    if ('map' %in% to_plot | 'map_regime' %in% to_plot) {
            df_page = map_panel(NULL, 
                                df_meta,
                                idPer_trend=length(trend_period),
                                trend_period=trend_period,
                                mean_period=mean_period,
                                colorForce=colorForce,
                                exXprob=exXprob,
                                mapType='regime',
                                shapefile_list=shapefile_list,
                                foot_note=foot_note,
                                foot_height=foot_height,
                                zone_to_show=zone_to_show,
                                logo_path=logo_path,
                                outdirTmp_pdf=outdirTmp_pdf,
                                outdirTmp_png=outdirTmp_png, 
                                df_page=df_page,
                                verbose=FALSE)
    }
            
    if ('map' %in% to_plot | 'map_trend' %in% to_plot) {
        df_page = map_panel(list_df2plot, 
                            df_meta,
                            idPer_trend=length(trend_period),
                            trend_period=trend_period,
                            mean_period=mean_period,
                            colorForce=colorForce,
                            exXprob=exXprob,
                            mapType='trend',
                            shapefile_list=shapefile_list,
                            foot_note=foot_note,
                            foot_height=foot_height,
                            zone_to_show=zone_to_show,
                            logo_path=logo_path,
                            outdirTmp_pdf=outdirTmp_pdf,
                            outdirTmp_png=outdirTmp_png, 
                            df_page=df_page)
    }
    
    if ('map' %in% to_plot | 'map_mean' %in% to_plot) {     
            df_page = map_panel(list_df2plot, 
                                df_meta,
                                idPer_trend=length(trend_period),
                                trend_period=trend_period,
                                mean_period=mean_period,
                                colorForce=colorForce,
                                exXprob=exXprob,
                                mapType='mean',
                                shapefile_list=shapefile_list,
                                foot_note=foot_note,
                                foot_height=foot_height,
                                zone_to_show=zone_to_show,
                                logo_path=logo_path,
                                outdirTmp_pdf=outdirTmp_pdf,
                                outdirTmp_png=outdirTmp_png, 
                                df_page=df_page)
    }

    # If summarize table needs to be plot
    if ('table' %in% to_plot) {
        df_page = table_panel(list_df2plot,
                              df_meta,
                              trend_period,
                              mean_period,
                              colorForce=colorForce,
                              exXprob=exXprob,
                              slice=19,
                              paper_size='A3',
                              foot_note=foot_note,
                              foot_height=foot_height,
                              resdir=resdir,
                              logo_path=logo_path,
                              outdirTmp_pdf=outdirTmp_pdf,
                              outdirTmp_png=outdirTmp_png, 
                              df_page=df_page)
    }

    # If datasheets needs to be plot
    if ('datasheet' %in% to_plot) {
        df_page = datasheet_panel(list_df2plot,
                                  df_meta,
                                  trend_period=trend_period,
                                  mean_period=mean_period,
                                  linetype_per=linetype_per,
                                  axis_xlim=axis_xlim,
                                  colorForce=colorForce,
                                  exXprob=exXprob,
                                  info_header=info_header,
                                  time_header=time_header,
                                  foot_note=foot_note,
                                  structure=structure,
                                  info_height=info_height,
                                  time_height=time_height,
                                  var_ratio=var_ratio,
                                  foot_height=foot_height,
                                  paper_size=paper_size,
                                  shapefile_list=shapefile_list,
                                  logo_path=logo_path,
                                  zone_to_show=zone_to_show,
                                  show_colorEvent=show_colorEvent,
                                  outdirTmp_pdf=outdirTmp_pdf,
                                  outdirTmp_png=outdirTmp_png, 
                                  df_page=df_page,
                                  pdf_chunk=pdf_chunk)
    }

    if ('summary' %in% to_plot) {
        summary_panel(df_page,
                      foot_note,
                      foot_height,
                      logo_path=logo_path,
                      outdirTmp_pdf=outdirTmp_pdf,
                      outdirTmp_png=outdirTmp_png)
    }

    # Combine independant pages into one PDF
    details = file.info(list.files(outdirTmp_pdf, full.names=TRUE))
    details = details[with(details, order(as.POSIXct(mtime))),]
    listfile_path = rownames(details)

    if ('summary' %in% to_plot) {
        summary_path = listfile_path[length(listfile_path)]
        listfile_path = listfile_path[-length(listfile_path)]
        listfile_path = c(summary_path, listfile_path)
    }

    if (pdf_chunk == 'by_code') {
        # Get all different stations code
        Code = rle(data[[1]]$Code)$value
        for (code in Code) {
            listfile_code_path = listfile_path[grepl(code, listfile_path)]
            pdf_combine(input=listfile_code_path,
                        output=file.path(outdir_code, paste0(code, '.pdf')))
        }
    }
    
    if (pdf_chunk == 'all') {
        pdf_combine(input=listfile_path,
                    output=file.path(outdir, outfile))
    }
} 
