

# library(ggplot2)
# library(corrplot)
# library(ggcorrplot)


plot_correlation_matrix = function (dataEx, figdir) {

    Model = levels(factor(dataEx$Model))

    vars2keep = names(dataEx)
    vars2keep = vars2keep[!grepl("([_]obs)|([_]sim)", vars2keep)]

    dataEx = dplyr::mutate(dataEx,
                           dplyr::across(where(is.logical),
                                         as.numeric),
                           .keep="all")
    
    for (model in Model) {
        print(model)
        dataEx_model = dataEx[dataEx$Model == model,]
        dataEx_model = dplyr::select(dataEx_model, vars2keep)
        nameRow = dataEx_model$Code
        dataEx_model = dplyr::select(dataEx_model, -c(Code, Model))
        nameCol = names(dataEx_model)

        nCol = ncol(dataEx_model)
        col2rm = c()
        for (i in 1:nCol) {
            if (sum(!is.na(dataEx_model[[i]])) < 3) {
                col2rm = c(col2rm, names(dataEx_model)[i])
            }
        }
        if (!is.null(col2rm)) {
            dataEx_model = dplyr::select(dataEx_model, -col2rm)
            nameCol = names(dataEx_model)
        }
        
        dataEx_model = as.matrix(dataEx_model)

        colnames(dataEx_model) = nameCol
        rownames(dataEx_model) = nameRow

        corrmatrix = cor(dataEx_model, use="pairwise.complete.obs")
        corrmatrix[!is.finite(corrmatrix)] = 0
        p = corrplot::cor.mtest(dataEx_model,
                                conf.level=0.95,
                                use="pairwise.complete.obs")$p
        p[!is.finite(p)] = 1

        plot = ggcorrplot::ggcorrplot(corrmatrix,
                                      hc.order=TRUE,
                                      type="full",
                                      tl.cex=5)
   

        filename = paste0("correlation_", model, ".pdf")

        if (!(file.exists(figdir))) {
            dir.create(figdir, recursive=TRUE)
        }
        
        ggplot2::ggsave(plot=plot,
                        path=figdir,
                        filename=filename,
                        width=15, height=15, units='cm', dpi=300)
    }
}
