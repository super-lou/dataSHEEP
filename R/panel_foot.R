

### 4.2. Foot note panel______________________________________________
#' @title Foot panel
#' @export
panel_foot = function (name, n_page, foot_height, logo_path) {
    
    nLogo = length(logo_path)
    nbg = nLogo + 3
    P = vector(mode='list', length=nbg)
    P[[1]] = void()
    LM_row = c(1)
    widths = c(1)

    for (i in 1:nLogo) {
        path = logo_path[i]
        logo = names(logo_path)[i]
        img = readPNG(path)
        
        if (logo == 'PR') {
            grob = rasterGrob(img,
                              x=0, hjust=0,
                              width=unit(0.8*foot_height, "cm"))
            width = 0.2
        }
        if (logo == 'FR') { 
            grob = rasterGrob(img,
                              x=0, hjust=0,
                              width=unit(1*foot_height, "cm"))
            width = 0.2
        }
        if (logo == 'INRAE') {
            grob = rasterGrob(img,
                              y=0.565,
                              vjust=0.5,
                              width=unit(1.08*foot_height, "cm"))
            width = 0.25
        }
        if (logo == 'AEAG') {
            grob = rasterGrob(img,
                              y=0.49,
                              vjust=0.5,
                              width=unit(0.7*foot_height, "cm"))
            width = 0.2
        }
        P[[i+1]] = grob
        LM_row = c(LM_row, i+1)
        widths = c(widths, width)
    }

    text_page = paste0(name, "  <b>p. ", n_page, "</b>")
    
    text_date = format(Sys.Date(),
                       "%B<span style='color:white'>&#95;</span>%Y")

    # Converts all texts to graphical object in the right position
    gtext_page = richtext_grob(text_page,
                               x=1, y=0,
                               margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                               hjust=1, vjust=0.5,
                               gp=gpar(col="#00A3A8", fontsize=8))

    gtext_date = richtext_grob(text_date,
                               x=1, y=0.55,
                               margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                               hjust=1, vjust=0.5,
                               gp=gpar(col="#00A3A8", fontsize=6))

    P[[nLogo+2]] = gtext_page
    LM_row1 = c(LM_row, nLogo+2)
    P[[nLogo+3]] = gtext_date
    LM_row2 = c(LM_row, nLogo+3)
    widths = c(widths, 1)
    
    # Creates the matrix layout
    LM = matrix(c(LM_row1,
                  LM_row2),
                nrow=2, 
                byrow=TRUE)
    
    # Arranges all the graphical objetcs
    plot = grid.arrange(grobs=P,
                        layout_matrix=LM,
                        widths=widths)
    
    # Return the plot object
    return (plot)
} 
