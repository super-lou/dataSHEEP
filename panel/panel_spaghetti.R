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


#' @title panel spaghetti
#' @export
panel_spaghetti = function (data_code, Colors=NULL,
                            title="débit journalier",
                            unit="m^{3}.s^{-1}",
                            alpha=0.7,
                            isSqrt=FALSE, missRect=FALSE,
                            isBack=TRUE,
                            isTitle=TRUE, dTitle=0,
                            sizeYticks=9,
                            date_labels="%Y",
                            breaks="10 years",
                            minor_breaks="2 years",
                            d_breaks=0,
                            break_round=-1,
                            isNormLaw=FALSE,
                            Xlabel=NULL,
                            isBackObsAbove=TRUE,
                            axis_xlim=NULL, grid=TRUE,
                            margin_add=margin(t=0, r=0, b=0, l=0, "mm"),
                            first=FALSE, last=FALSE) {

    isDate = inherits(data_code$Date, 'Date')

    if (isNormLaw) {
        data_code = data_code[data_code$Date != 0 &
                              data_code$Date != 1,]
    }
    
    if ("Model" %in% names(data_code)) {

        select_good = function (X) {
            Xrle = rle(X)
            value = Xrle$values[Xrle$lengths == max(Xrle$lengths)]
            if (length(value) > 1) {
                value = mean(value, na.rm=TRUE)
            }
            return (value)
        }
        
        Model = levels(factor(data_code$Model))
        nModel = length(Model)
        
        # data_code_obs =
        #     dplyr::distinct(dplyr::select(data_code,
        #                                   c(Code, Date, Q_obs)))
        data_code_obs =
            dplyr::summarise(dplyr::group_by(data_code, Date),
                             Q=select_good(Q_obs),
                             .groups="drop")
        
        # data_code_obs = dplyr::rename(data_code_obs, Q=Q_obs)

        # print(data_code_obs, n=Inf)

        maxQ_obs = max(data_code_obs$Q, na.rm=TRUE)
        minQ_obs = min(data_code_obs$Q, na.rm=TRUE)
        maxQ_sim = max(data_code$Q_sim, na.rm=TRUE)
        minQ_sim = min(data_code$Q_sim, na.rm=TRUE)
        maxQ = max(c(maxQ_obs, maxQ_sim), na.rm=TRUE)
        minQ = min(c(minQ_obs, minQ_sim), na.rm=TRUE)
        
    } else {
        data_code_obs = data_code
        maxQ = max(data_code_obs$Q, na.rm=TRUE)
        minQ = min(data_code_obs$Q, na.rm=TRUE)
    }

    maxQ_win = maxQ * 1.05
    minQ_win = minQ * 0.95#expansion

    if (is.null(axis_xlim)) {
        limits = c(min(data_code_obs$Date), max(data_code_obs$Date))
    } else {
        limits = axis_xlim
    }

    
    # Open new plot
    p = ggplot() + coord_cartesian(clip="off") + 
        theme_IPCC(isBack, isTitle, dTitle=dTitle, isXlabel=!is.null(Xlabel)) +
        # theme_WIP() + 
        theme(panel.border=element_blank(),
              axis.text.y=element_text(size=sizeYticks))
    

    ### Grid ###
    if (!grid) {
        p = p +
            theme(panel.grid.major.y=element_blank())
    }

    ### Missing data ###
    # If the option is TRUE
    if (missRect) {
        # Remove NA data
        NAdate = data_code_obs$Date[is.na(data_code_obs$Q)]
        # Get the difference between each point of date data without NA
        dNAdate = diff(NAdate)
        # If difference of day is not 1 then
        # it is TRUE for the beginning of each missing data period 
        NAdate_Down = NAdate[append(Inf, dNAdate) != 1]
        # If difference of day is not 1 then
        # it is TRUE for the ending of each missing data period 
        NAdate_Up = NAdate[append(dNAdate, Inf) != 1]

        if (isDate) {
            xmin = as.Date(NAdate_Down)
            xmax = as.Date(NAdate_Up)
        } else {
            xmin = NAdate_Down
            xmax = NAdate_Up
        }
        
        # Plot the missing data period
        p = p +
            annotate("rect",
                     xmin=xmin, 
                     ymin=0, 
                     xmax=xmax, 
                     ymax=Inf,
                     linetype=0,
                     fill=INRAElightcyan,
                     alpha=0.4)
    }

    # zeroline
    p = p +
        ggplot2::annotate("line",
                          x=limits,
                          y=c(0, 0),
                          color=IPCCgrey60,
                          size=0.5,
                          lineend="round")
    
    ### Data ###
    if (!isBackObsAbove) {
        p = p +
            ggplot2::annotate("line",
                              x=data_code_obs$Date,
                              y=data_code_obs$Q,
                              color="white",
                              linewidth=0.4,
                              lineend="round")
    }
    
    if ("Model" %in% names(data_code)) {
        for (i in 1:nModel) {
            model = Model[i]
            data_model_code = data_code[data_code$Model == model,] 
            # Plot the data as line
            p = p +
                ggplot2::annotate("line",
                                  x=data_model_code$Date,
                                  y=data_model_code$Q_sim,
                                  color="white",
                                  linewidth=0.7,
                                  lineend="round")
        }
        if (is.null(Colors)) {
            Colors = rep(IPCCgrey67, nModel)
            names(Colors) = Model
        }
        for (i in 1:nModel) {
            model = Model[i]
            data_model_code = data_code[data_code$Model == model,] 
            # Plot the data as line
            p = p +
                ggplot2::annotate("line",
                                  x=data_model_code$Date,
                                  y=data_model_code$Q_sim,
                                  color=Colors[names(Colors) == model],
                                  linewidth=0.4,
                                  alpha=alpha,
                                  lineend="round")
        }
    }

    if (isBackObsAbove) {
        p = p +
            ggplot2::annotate("line",
                              x=data_code_obs$Date,
                              y=data_code_obs$Q,
                              color="white",
                              linewidth=1.7,
                              lineend="round") +
            ggplot2::annotate("line",
                              x=data_code_obs$Date,
                              y=data_code_obs$Q,
                              color=IPCCgrey25,
                              linewidth=0.55,
                              lineend="round")
    } else {
        p = p +
            ggplot2::annotate("line",
                              x=data_code_obs$Date,
                              y=data_code_obs$Q,
                              color=IPCCgrey25,
                              linewidth=0.2,
                              lineend="round") 
    }

    
    # Y axis title
    unit = gsub(" ", "\\\\,", unit)
    ylabel = paste0(title, "\\,", "($", unit, "$)")
    yTeXlabel = TeX(ylabel)

    if (isTitle) {
        p = p +
            ggtitle(yTeXlabel)
    } else {
        p = p +
            ylab(yTeXlabel)
    }

    if (!is.null(Xlabel)) {
        p = p +
            xlab(Xlabel)
    }

    if (first) {
        position = 'top'
    } else {
        position = 'bottom'
    }

    get_breaks = function(X) {
        if (isDate) {
            Xmin = round(lubridate::year(min(X)), break_round)
            Xmax = round(lubridate::year(max(X)), break_round)
            if (Xmax-Xmin <= 1) {
                Xmin = lubridate::year(X)[1]
                Xmax = lubridate::year(X)[1] + 1
            }
            res = seq.Date(from=as.Date(paste0(Xmin, "-01-01")) + d_breaks,
                           to=as.Date(paste0(Xmax, "-01-01")) + d_breaks,
                           by=breaks)
        } else {
            Xmin = round(min(X), break_round)
            Xmax = round(max(X), break_round)
            res = seq(from=Xmin + d_breaks,
                      to=Xmax + d_breaks,
                      by=breaks)
        }
        return (res)
    }

    get_minor_breaks = function(X) {
        if (isDate) {
            Xmin = round(lubridate::year(min(X)), break_round)
            Xmax = round(lubridate::year(max(X)), break_round)
            if (Xmax-Xmin <= 1) {
                Xmin = lubridate::year(X)[1]
                Xmax = lubridate::year(X)[1] + 1
            }
            res = seq.Date(from=as.Date(paste0(Xmin, "-01-01")),
                           to=as.Date(paste0(Xmax, "-01-01")),
                           by=minor_breaks)
        } else {
            Xmin = round(min(X), break_round)
            Xmax = round(max(X), break_round)
            res = seq(from=Xmin + d_breaks,
                      to=Xmax + d_breaks,
                      by=minor_breaks)
        }
        return (res)
    }

    if (isNormLaw) {
        
        p = p +
            annotation_custom(
                ggplotGrob(ggplot() + theme_void() +
                           annotate("segment",
                                    x=0.7, xend=0.75,
                                    y=-1, yend=-1,
                                    color=IPCCgrey50,
                                    linewidth=0.3,
                                    arrow=arrow(length=unit(1, "mm")),
                                    lineend="round") +
                           annotate("text",
                                    x=0.76, y=-1,
                                    label="1   basses eaux",
                                    color=IPCCgrey50,
                                    size=2.7,
                                    vjust=0.51, hjust=0) +
                           annotate("segment",
                                    x=0.3, xend=0.25,
                                    y=-1, yend=-1,
                                    color=IPCCgrey50,
                                    linewidth=0.3,
                                    arrow=arrow(length=unit(1, "mm")),
                                    lineend="round") +
                           annotate("text",
                                    x=0.24, y=-1,
                                    label="hautes eaux   0",
                                    color=IPCCgrey50,
                                    size=2.7,
                                    vjust=0.51, hjust=1) +
                           scale_x_continuous(limits=c(0, 1),
                                              expand=c(0, 0)) +
                           scale_y_continuous(limits=c(-2, 2),
                                              expand=c(0, 0))),
                xmin=-Inf, xmax=Inf,
                ymin=-2, ymax=2)

        
        p = p +
            scale_x_continuous(
                trans=scales::probability_trans("norm"),
                breaks=get_breaks,
                minor_breaks=get_minor_breaks,
                guide='axis_minor',
                limits=limits,
                position=position, 
                expand=c(0, 0))
        
    } else {
        if (isDate) {
            p = p +
                scale_x_date(
                    breaks=get_breaks,
                    minor_breaks=get_minor_breaks,
                    guide='axis_minor',
                    date_labels=date_labels,
                    limits=limits,
                    position=position, 
                    expand=c(0, 0))
        } else {
            p = p +
                scale_x_continuous(
                    breaks=get_breaks,
                    minor_breaks=get_minor_breaks,
                    guide='axis_minor',
                    limits=limits,
                    position=position, 
                    expand=c(0, 0))
        }
    }
    

    # Parameters of the y axis
    if (get_power(minQ) >= 4) {
        labels = function(X) {
            TeX(paste0(format(
                round(X/10^get_power(X), 1), nsmall=1),
                "x", 10, "$^{$",
                get_power(X),
                "}"))
        }
    } else {
        labels = waiver()
    }

    if (isSqrt) {
        p = p + scale_y_sqrt(limits=c(0, NA),
                             n.breaks=4,
                             labels=labels,
                             expand=expansion(mult=c(0, 0.1)))
        
    } else {
        p = p +
            scale_y_continuous(limits=c(0, NA),
                               n.breaks=5,
                               labels=labels,
                               expand=expansion(mult=c(0, 0.1)))
    }

    # Margins
    tt = 2.5
    t = 2
    tb = 3
    b = 2
    
    if (last == "all") {
        pLastTRUE = p
        pLastFALSE = p
        if (first) {
            pLastFALSE = pLastFALSE +
                theme(plot.margin=
                          margin(t=tt, r=0, b=tb, l=0, unit="mm")+
                          margin_add)
            pLastTRUE = pLastTRUE +
                theme(plot.margin=
                          margin(t=tt, r=0, b=0, l=0, unit="mm")+
                          margin_add)
        } else {
            pLastFALSE = pLastFALSE + 
                theme(plot.margin=
                          margin(t=t, r=0, b=b, l=0, unit="mm")+
                          margin_add,
                      axis.text.x=element_blank())
            pLastTRUE = pLastTRUE +
                theme(plot.margin=
                          margin(t=t, r=0, b=0, l=0, unit="mm")+
                          margin_add)
        }

        res = list(lastTRUE=pLastTRUE, lastFALSE=pLastFALSE)
        return(res)
        
    } else {
        if (first & !last) {
            p = p +
                theme(plot.margin=
                          margin(t=tt, r=0, b=tb, l=0, unit="mm")+
                          margin_add)
        } else if (!first & last) {
            p = p + 
                theme(plot.margin=
                          margin(t=t, r=0, b=0, l=0, unit="mm")+
                          margin_add)
        } else if (first & last) {
            p = p + 
                theme(plot.margin=
                          margin(t=tt, r=0, b=0, l=0, unit="mm")+
                          margin_add)
        } else if (!first & !last){
            p = p + 
                theme(plot.margin=
                          margin(t=t, r=0, b=b, l=0, unit="mm")+
                          margin_add,
                      axis.text.x=element_blank())
        }
        return(p)
    }
} 
