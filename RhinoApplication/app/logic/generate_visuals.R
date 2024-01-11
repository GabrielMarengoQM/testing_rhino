box::use(
  tidyverse[...],
  dplyr[...],
  plotly[plot_ly, layout, add_trace],
  stats[...]
)

# Rda data
box::use(
  app/logic/import_rda_data[...]
)

# Plots ----
# General ----
# Takes as input selected data, utilises list of lists with [[1]] data + [[2]] name of data
#' @export
getDataFromUserSelect <- function(selected_data, data) {
  gene_lists <- list()
  for (i in data) {
    if (i[[2]] %in% selected_data) {
      gene_lists <- append(gene_lists, list(i))
    }
  }
  return(gene_lists)
}

# Mouse model data functions ----
# Takes gene list, returns dataframe for plot
#' @export
getImpcPlotData <- function(gene_lists, data) {
  
  main.annotated.data.frame <- data
  
  mouse_data_list <- list()
  for (i in gene_lists) {
    i <- i[[1]] # Get Data
    
    impc_data <- main.annotated.data.frame[main.annotated.data.frame$gene_symbol %in% i, c('mgi_id', 'impc_viability')]
    impc_plot_data <- impc_data %>%
      dplyr::filter(mgi_id != "NA") %>%
      dplyr::filter(impc_viability != "NA") %>%
      dplyr::mutate(impc_viability_2 = ifelse(!impc_viability %in% c("lethal","subviable","viable"),
                                              "conflicting", impc_viability)) %>%
      dplyr::group_by(impc_viability_2) %>%
      dplyr::tally() %>%
      dplyr::mutate(impc_viability_3 = factor(impc_viability_2,
                                              levels = c("lethal","subviable","viable"))) %>%
      dplyr::mutate(percentage = (n/sum(n)*100))
    
    # Remove conflicting rows 
    impc_plot_data <- impc_plot_data[impc_plot_data$impc_viability_2 != "conflicting", ]
    
    # Round the numeric columns to 3 decimal places
    impc_plot_data <- impc_plot_data %>%
      dplyr::mutate_at(vars('percentage'), list(~ round(., 3)))
    
    if (dim(impc_plot_data)[1] != 3) {
      # if true then one category has 0 genes and needs to be filled
      levels <- c('viable', 'subviable', 'lethal')
      current_rows <- impc_plot_data$impc_viability_3
      missing_rows <- levels[!levels %in% current_rows]
      
      # Add missing rows with a value of 0 for both 'n' and 'percentage'
      missing_data <- data.frame(impc_viability_3 = missing_rows, n = 0, percentage = 0)
      
      # Update impc_plot_data with the missing rows
      impc_plot_data <- bind_rows(impc_plot_data, missing_data)
    }
    
    mouse_data_list <- c(mouse_data_list, list(impc_plot_data))
  }
  
  return(mouse_data_list)
}

# Takes impc data frame, create plotly plot
#' @export
generateImpcPlot <- function(impc_plot_data) {
  # conflicted::conflicts_prefer(plotly::layout)
  x_axis <- c("lethal", "subviable", "viable")
  data <- impc_plot_data[, c('impc_viability_3', 'percentage')]
  new_col_names <- c("x_axis", "percentage")
  colnames(data) <- new_col_names
  
  impc_plot <- plot_ly(data, x = ~x_axis, y = ~percentage, type = 'bar',
                       name = "EXAMPLE", textposition = 'outside', text = ~percentage) %>%
    plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'IMPC preweaning viability assessment'))
  
  return(impc_plot)
}

# Takes gene lists & impc data frames, creates plotly plot with multiple traces
#' @export
generateMultipleTracesImpcPlot <- function(plots, gene_lists_for_plots) {
  percentage_cols <- lapply(plots, function(plot) plot$percentage)
  
  # Bind the percentage column
  df <- data.frame(x_axis = c("lethal", "subviable", "viable"))
  df <- bind_cols(df, !!!percentage_cols)
  # Rename the columns
  list_names <- sapply(gene_lists_for_plots, function(x) x[[2]])
  
  # Create column names for the dataframe
  col_names <- c("x_axis", list_names)
  
  # Assign column names to your dataframe (replace df with your actual dataframe)
  colnames(df) <- col_names
  # set y_col as first value name for initial plotly obj
  y_col <- names(df)[2] # first value after xaxis column
  y_col
  p <- plot_ly(df, x = ~x_axis, y = as.formula(paste0("~", y_col)),
               type = 'bar', name = y_col, textposition = 'outside', text = ~get(y_col)) %>%
    plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'IMPC preweaning viability assessment'))
  p
  # set y_cols2 for rest of value names for traces
  y_cols1<- names(df)[-1]
  y_cols2 <- y_cols1[-1]
  # Add traces
  for (i in y_cols2) {
    text_col <- paste0("text_", i)  # New variable for dynamic text
    df[[text_col]] <- df[[i]]
    
    p <- p %>%
      add_trace(data = df, y = as.formula(paste0("~", i)), name = i, text = as.formula(paste0("~", text_col)))
  }
  # Print the resulting plot
  p
}

#' @export
getMgiPlotData <- function(gene_list) {
  
  mgi_data <- main.annotated.data.frame[main.annotated.data.frame$gene_symbol %in% gene_list, c('mgi_id', 'mgi_viability')]
  
  mgi_plot_data <- mgi_data %>%
    filter(mgi_id != "NA") %>%
    filter(mgi_viability != "NA") %>%
    mutate(mgi_viability_2 = ifelse(!mgi_viability %in% c("lethal", "viable"),
                                    "conflicting", mgi_viability)) %>%
    group_by(mgi_viability_2) %>%
    tally() %>%
    mutate(mgi_viability_3 = factor(mgi_viability_2,
                                    levels = c("lethal", "viable"))) %>%
    mutate(percentage = (n/sum(n)*100))
  
  # Remove conflicting rows 
  mgi_plot_data <- mgi_plot_data[mgi_plot_data$mgi_viability_2 != "conflicting", ]
  
  # Round the numeric columns to 3 decimal places
  mgi_plot_data <- mgi_plot_data %>%
    mutate_at(vars('percentage'), list(~ round(., 3)))
  
  if (dim(mgi_plot_data)[1] != 2) {
    # if true then one category has 0 genes and needs to be filled
    levels <- c('viable', 'lethal')
    current_rows <- mgi_plot_data$mgi_viability_3
    missing_rows <- levels[!levels %in% current_rows]
    
    # Add missing rows with a value of 0 for both 'n' and 'percentage'
    missing_data <- data.frame(mgi_viability_3 = missing_rows, n = 0, percentage = 0)
    
    # Update impc_plot_data with the missing rows
    mgi_plot_data <- bind_rows(mgi_plot_data, missing_data)
  }
  
  return(mgi_plot_data)
}

#' @export
generateMgiPlot <- function(mgi_plot_data) {
  # conflicted::conflicts_prefer(plotly::layout)
  x_axis <- c("lethal", "viable")
  data <- mgi_plot_data[, c('mgi_viability_3', 'percentage')]
  new_col_names <- c("x_axis", "percentage")
  colnames(data) <- new_col_names
  
  mgi_plot <- plot_ly(data, x = ~x_axis, y = ~percentage, type = 'bar',
                      name = "EXAMPLE", textposition = 'outside', text = ~percentage) %>%
    plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'MGI viability assessment'))
  
  return(mgi_plot)
}

# disease plots ----
#' @export
getHasOmimPlotData <- function(gene_list) {
  
  omim_data <- genesMetaDataDf_data[genesMetaDataDf_data$gene_symbol %in% gene_list, c('hgnc_id', 'omim_phenotype_name')]
  
  omim_plot_data <- omim_data %>%
    mutate(has_omim_phenotype = if_else(!is.na(omim_phenotype_name), "yes", "no"))
  
  omim_summary_data <- omim_plot_data %>%
    group_by(has_omim_phenotype) %>%
    summarize(count = n()) %>%
    mutate(percentage = (count / sum(count)) * 100)
  
  # Round the numeric columns to 3 decimal places
  omim_summary_data <- omim_summary_data %>%
    mutate_at(vars('percentage'), list(~ round(., 3)))
  
  if (dim(omim_summary_data)[1] != 2) {
    # if true then one category has 0 genes and needs to be filled
    levels <- c('yes', 'no')
    current_rows <- omim_summary_data$has_omim_phenotype
    missing_rows <- levels[!levels %in% current_rows]
    
    # Add missing rows with a value of 0 for both 'n' and 'percentage'
    missing_data <- data.frame(has_omim_phenotype = missing_rows, n = 0, percentage = 0)
    
    # Update impc_plot_data with the missing rows
    omim_summary_data <- bind_rows(omim_summary_data, missing_data)
  }
  
  return(omim_summary_data)
}

#' @export
generateHasOmimPlot <- function(omim_summary_data) {
  # conflicted::conflicts_prefer(plotly::layout)
  x_axis <- c("yes", "no")
  data <- omim_summary_data[, c('has_omim_phenotype', 'percentage')]
  new_col_names <- c("x_axis", "percentage")
  colnames(data) <- new_col_names
  
  omim_plot <- plot_ly(data, x = ~x_axis, y = ~percentage, type = 'bar',
                       name = "EXAMPLE", textposition = 'outside', text = ~percentage) %>%
    plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'Mendelian disease association (OMIM)'))
  
  return(omim_plot)
}

#' @export
getOmimLethalityPlotData <- function(gene_list) {
  
  omim_lethality_data <- genesMetaDataDf_data[genesMetaDataDf_data$gene_symbol %in% gene_list, c('hgnc_id', 'omim_gene_lethality')]
  
  omim_summary_data <- omim_lethality_data %>%
    filter(!is.na(omim_gene_lethality)) %>%
    filter(omim_gene_lethality %in% c("lethal", "nonlethal")) %>%
    group_by(omim_gene_lethality) %>%
    summarize(count = n()) %>%
    mutate(percentage = (count / sum(count)) * 100)
  
  # Round the numeric columns to 3 decimal places
  omim_lethality_summary_data <- omim_summary_data %>%
    mutate_at(vars('percentage'), list(~ round(., 3)))
  
  if (dim(omim_lethality_summary_data)[1] != 2) {
    # if true then one category has 0 genes and needs to be filled
    levels <- c("lethal", "nonlethal")
    current_rows <- omim_lethality_summary_data$omim_gene_lethality
    missing_rows <- levels[!levels %in% current_rows]
    
    # Add missing rows with a value of 0 for both 'n' and 'percentage'
    missing_data <- data.frame(omim_gene_lethality = missing_rows, n = 0, percentage = 0)
    
    # Update impc_plot_data with the missing rows
    omim_lethality_summary_data <- bind_rows(omim_lethality_summary_data, missing_data)
  }
  
  return(omim_lethality_summary_data)
}

#' @export
generateOmimLethalityPlot <- function(omim_lethality_summary_data) {
  # conflicted::conflicts_prefer(plotly::layout)
  x_axis <- c("lethal", "nonlethal")
  data <- omim_lethality_summary_data[, c('omim_gene_lethality', 'percentage')]
  new_col_names <- c("x_axis", "percentage")
  colnames(data) <- new_col_names
  
  omim_lethality_plot <- plot_ly(data, x = ~x_axis, y = ~percentage, type = 'bar',
                                 name = "Example", textposition = 'outside', text = ~percentage) %>%
    plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'Lethal Phenotypes (OMIM)'))
  
  return(omim_lethality_plot)
}

#' @export
constraintMetricsPlots <- function(gene_lists_for_plots, metric_col_name, x_axis_text) {
  metrics_data_list <- list()
  for (i in gene_lists_for_plots) {
    metrics_data <- gene.constraint.metrics.num.only[gene.constraint.metrics.num.only$gene_symbol %in% (i[[1]]), c('gene_symbol', metric_col_name)]
    metrics_data_list <- c(metrics_data_list, list(metrics_data))
  }
  
  df <- purrr::reduce(metrics_data_list, full_join, by = "gene_symbol")
  
  # Extract the second elements (list names) from gene_lists_for_plots
  list_names <- sapply(gene_lists_for_plots, function(x) x[[2]])
  
  # Create column names for the dataframe
  col_names <- c("x_axis", list_names)
  
  # Assign column names to your dataframe (replace df with your actual dataframe)
  colnames(df) <- col_names
  # set y_col as first value name for initial plotly obj
  y_col <- names(df)[2] # first value after xaxis column
  y_col
  p <- plot_ly(df, y = as.formula(paste0("~", y_col)), x = y_col, name = y_col, type = "violin", box = list(visible = T),
               hoverinfo = "text", hovertext = paste("Gene Symbol: ", df$x_axis, "<br>", x_axis_text, df[[y_col]])) %>%
    plotly::layout(yaxis = list(title = x_axis_text))
  
  # set y_cols2 for rest of value names for traces
  y_cols1<- names(df)[-1]
  y_cols2 <- y_cols1[-1]
  # Add traces
  for (i in y_cols2) {
    text_col <- paste0("text_", i)  # New variable for dynamic text
    df[[text_col]] <- df[[i]]
    
    p <- p %>%
      add_trace(data = df, y = as.formula(paste0("~", i)), x = i, name = i, text = as.formula(paste0("~", text_col)),
                hoverinfo = "text", hovertext = paste("Gene Symbol: ", df$x_axis, "<br>", x_axis_text, df[[i]]))
  }
  # Print the resulting plot
  return(p)
}

# Gene search highlight data point
#' @export
addGeneTrace <- function(plot, col, gene, x_axis_text) {
  for (i in plot$x$attrs) {
    plot <- plot %>%
      add_trace(
        y = gene.constraint.metrics.num.only[[col]][gene.constraint.metrics.num.only$gene_symbol == gene],
        x = i$x, 
        name = gene,
        hoverinfo = "text", 
        hovertext = paste("Gene Symbol: ", gene, "<br>", x_axis_text, 
                          gene.constraint.metrics.num.only[[col]][gene.constraint.metrics.num.only$gene_symbol == gene]),
        type = "scatter", mode = 'markers'
      )
  }
  return(plot)
}

# Threshold line
#' @export
hline <- function(y = 0, color = "grey") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(dash = "dash", color = color)
  )
}