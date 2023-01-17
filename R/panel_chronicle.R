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


#' @title panel chronicle
#' @export
panel_chronicle = function (data_code, isSqrt=FALSE, missRect=FALSE,
                            axis_xlim=NULL, grid=TRUE,
                            first=FALSE, last=FALSE) {


    isDate = as.Date("1970-01-01")
    
    if ("Model" %in% names(data_code)) {
        Model = levels(factor(data_code$Model))
        nModel = length(Model)
        
        data_code_obs =
            dplyr::distinct(dplyr::select(data, c(Code, Date, Q_obs)))
        data_code_obs = dplyr::rename(data_code_obs, Q=Q_obs)

        maxQ_obs = max(data_code_obs$Q, na.rm=TRUE)
        minQ_obs = min(data_code_obs$Q, na.rm=TRUE)
        maxQ_sim = max(data_code$Q_sim, na.rm=TRUE)
        minQ_sim = min(data_code$Q_sim, na.rm=TRUE)
        maxQ = max(c(maxQ_obs, maxQ_sim), na.rm=TRUE)
        minQ = min(c(minQ_obs, minQ_sim), na.rm=TRUE)
        
    } else {
        data_code_obs = data_code
        maxQ = max(data_code$Q, na.rm=TRUE)
        minQ = min(data_code$Q, na.rm=TRUE)
    }

    maxQ_win = maxQ * 1.05
    minQ_win = minQ * 0.95#expansion

    if (is.null(axis_xlim)) {
        limits = c(min(data_code_obs$Date), max(data_code_obs$Date))
    } else {
        limits = axis_xlim
    }
    
    # Open new plot
    p = ggplot() + theme_IPCC() +
        theme(panel.border=element_blank(),
              axis.text.y=element_text(size=7))

    # ### Grid ###
    if (grid) {
        p = p +
            theme(panel.grid.major.y=element_line(color=IPCCgrey85,
                                                  size=0.3))
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

        # Plot the missing data period
        p = p +
            annotate("rect",
                     xmin=as.Date(NAdate_Down), 
                     ymin=0, 
                     xmax=as.Date(NAdate_Up), 
                     ymax=Inf,
                     linetype=0,
                     fill=INRAElightcyan,
                     alpha=0.4)
    }

    p = p +
        ggplot2::annotate("line",
                          x=limits,
                          y=c(0, 0),
                          color=IPCCgrey60,
                          size=0.6,
                          lineend="round")
    
    ### Data ###
    p = p +
        ggplot2::annotate("line",
                          x=data_code_obs$Date,
                          y=data_code_obs$Q,
                          color="white",
                          linewidth=0.4,
                          lineend="round")
    
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
                                  linewidth=0.6,
                                  lineend="round")
        }
        for (i in 1:nModel) {
            model = Model[i]
            data_model_code = data_code[data_code$Model == model,] 
            # Plot the data as line
            p = p +
                ggplot2::annotate("line",
                                  x=data_model_code$Date,
                                  y=data_model_code$Q_sim,
                                  color=IPCCgrey67,
                                  linewidth=0.4,
                                  alpha=0.5,
                                  lineend="round")
        }
    }

    p = p +
        ggplot2::annotate("line",
                          x=data_code_obs$Date,
                          y=data_code_obs$Q,
                          color=IPCCgrey25,
                          linewidth=0.2,
                          lineend="round")
    
    # Y axis title
    var = "Q"
    unit = "m^{3}.s^{-1}"
    unit = gsub(" ", "\\\\,", unit)
    ylabel = paste0("\\textbf{", var, "}", "\\,", "\\[$", unit, "$\\]")
    yTeXlabel = TeX(ylabel)
    
    p = p +
        ylab(yTeXlabel)

    if (first) {
        position = 'top'
    } else {
        position = 'bottom'
    }

    breaks = function(X) {
        Xmin = min(X)
        Xmax = max(X)
        seq.Date(from=as.Date(paste0(
                     round(lubridate::year(Xmin), -1),
                     "-01-01")),
                 to=as.Date(paste0(
                     round(lubridate::year(Xmax), -1),
                     "-01-01")),
                 by="10 years")
    }

    minor_breaks = function(X) {
        Xmin = min(X)
        Xmax = max(X)
        seq.Date(from=as.Date(paste0(
                     round(lubridate::year(Xmin), -1),
                     "-01-01")),
                 to=as.Date(paste0(
                     round(lubridate::year(Xmax), -1),
                     "-01-01")),
                 by="2 years")
    }
    
    date_labels = "%Y"

    # Parameters of the x axis contain the limit of the date dataEx
    p = p +
        scale_x_date(
            breaks=breaks,
            minor_breaks=minor_breaks,
            guide='axis_minor',
            date_labels=date_labels,
            limits=limits,
            position=position, 
            expand=c(0, 0)
        )

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
                theme(plot.margin=margin(t=tt, r=0, b=tb, l=0,
                                         unit="mm"))
            pLastTRUE = pLastTRUE +
                theme(plot.margin=margin(t=tt, r=0, b=0, l=0,
                                         unit="mm"))
        } else {
            pLastFALSE = pLastFALSE + 
                theme(plot.margin=margin(t=t, r=0, b=b, l=0,
                                         unit="mm"),
                      axis.text.x=element_blank())
            pLastTRUE = pLastTRUE +
                theme(plot.margin=margin(t=t, r=0, b=0, l=0,
                                         unit="mm"))
        }

        res = list(lastTRUE=pLastTRUE, lastFALSE=pLastFALSE)
        return(res)
        
    } else {
        if (first & !last) {
            p = p +
                theme(plot.margin=margin(t=tt, r=0, b=tb, l=0,
                                         unit="mm"))
        } else if (!first & last) {
            p = p + 
                theme(plot.margin=margin(t=t, r=0, b=0, l=0,
                                         unit="mm"))
        } else if (first & last) {
            p = p + 
                theme(plot.margin=margin(t=tt, r=0, b=0, l=0,
                                         unit="mm"))
        } else if (!first & !last){
            p = p + 
                theme(plot.margin=margin(t=t, r=0, b=b, l=0,
                                         unit="mm"),
                      axis.text.x=element_blank())
        }
        return(p)
    }
} 
