#' @export
dpc_gene_list_data <- function() {
  #list_of_dpcs <- readRDS("../../../rda/zipped_dpc_names_genes.rda")
  list_of_dpcs <- readRDS("./rda/dpc_gene_lists.rda")
}

#' @export
morphic_gene_list_data <- function() {
  morphic_gene_list_data <- readRDS("./rda/morphic_gene_list_data.rda")
}

#' @export
meta_data_table_data <- function() {
  #genesMetaDataDf_data <- readRDS('../../../rda/all_genes_with_anot_jan2.rda')
  genesMetaDataDf_data <- readRDS('./rda/all_genes_with_anot_jan16.rda')
}

#' @export
visuals_data <- function() {
  main.annotated.data.frame <- readRDS('./rda/main.annotated.data.frame.rda')
}

#' @export
constraint_metrics <- function() {
  gene.constraint.metrics.num.only <- readRDS('./rda/gene.constraint.metrics.num.only.rda')
}
