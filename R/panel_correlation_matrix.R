

# library(ggplot2)
# library(corrplot)
# library(ggcorrplot)


panel_correlation_matrix = function (dataEx2D_model) {

    vars2keep = names(dataEx2D_model)
    vars2keep = vars2keep[!grepl("([_]obs)|([_]sim)", vars2keep)]

    dataEx2D_model = dplyr::mutate(dataEx2D_model,
                           dplyr::across(where(is.logical),
                                         as.numeric),
                           .keep="all")
    
    dataEx2D_model = dplyr::select(dataEx2D_model, vars2keep)
    nameRow = dataEx2D_model$Code
    dataEx2D_model = dplyr::select(dataEx2D_model, -c(Code, Model))
    nameCol = names(dataEx2D_model)

    nCol = ncol(dataEx2D_model)
    col2rm = c()
    for (i in 1:nCol) {
        if (sum(!is.na(dataEx2D_model[[i]])) < 3) {
            col2rm = c(col2rm, names(dataEx2D_model)[i])
        }
    }
    if (!is.null(col2rm)) {
        dataEx2D_model = dplyr::select(dataEx2D_model, -col2rm)
        nameCol = names(dataEx2D_model)
    }
    
    dataEx2D_model = as.matrix(dataEx2D_model)

    colnames(dataEx2D_model) = nameCol
    rownames(dataEx2D_model) = nameRow

    corrmatrix = cor(dataEx2D_model,
                     # method="pearson",
                     method="spearman",
                     # use="complete.obs")
                     use="pairwise.complete.obs")
    corrmatrix[!is.finite(corrmatrix)] = 0
    p = corrplot::cor.mtest(dataEx2D_model,
                            conf.level=0.95,
                            # method="pearson",
                            method="spearman",
                            # use="complete.obs")$p
                            use="pairwise.complete.obs")$p
    p[!is.finite(p)] = 1

    plot = ggcorrplot::ggcorrplot(corrmatrix,
                                  hc.order=TRUE,
                                  type="full",
                                  tl.cex=5)
    

    #     filename = paste0("correlation_", model, ".pdf")

    #     if (!(file.exists(figdir))) {
    #         dir.create(figdir, recursive=TRUE)
    #     }
    
    #     ggplot2::ggsave(plot=plot,
    #                     path=figdir,
    #                     filename=filename,
    #                     width=15, height=15, units='cm', dpi=300)
    
    return (plot)
}
