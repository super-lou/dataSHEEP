
#' @title Event panel
#' @export
panel_event = function(event, colorEvent, colorTextEvent) {

    if (event != 'Resume' & event != 'None') {
        plot = ggplot() + theme_void() +
            
            theme(plot.margin=margin(t=0, r=3, b=0, l=0, unit="mm")) + 
            
            annotate("rect",
                     xmin=0, xmax=1,
                     ymin=0, ymax=5,
                     fill=colorEvent[event]) +
            
            annotate("text",
                     x=0.5, y=0.1,
                     vjust=0.5, hjust=0,
                     label=toupper(event),
                     color=colorTextEvent[event],
                     fontface="bold",
                     size=2.9,
                     angle=90) +
            
            scale_x_continuous(limits=c(0, 1),
                               expand=c(0, 0)) +
            
            scale_y_continuous(limits=c(0, 5),
                               expand=c(0, 0))
    } else {
        plot = void()
    }
    return (plot)
} 
