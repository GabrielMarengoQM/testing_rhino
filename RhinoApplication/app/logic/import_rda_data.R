#' @export
dpc_gene_list_data <- function() {
  #list_of_dpcs <- readRDS("../../../rda/zipped_dpc_names_genes.rda")
  list_of_dpcs <- readRDS("/Users/gabrielm/Desktop/Rstuff/essentialgenes_rshiny_rhino_test/rda/zipped_dpc_names_genes.rda")
}

#' @export
meta_data_table_data <- function() {
  #genesMetaDataDf_data <- readRDS('../../../rda/all_genes_with_anot_jan2.rda')
  genesMetaDataDf_data <- readRDS('/Users/gabrielm/Desktop/Rstuff/essentialgenes_rshiny_rhino_test/rda/all_genes_with_anot_jan2.rda')
}

#' @export
visuals_data <- function() {
  main.annotated.data.frame <- readRDS('/Users/gabrielm/Desktop/Rstuff/essentialgenes_rshiny_rhino_test/rda/main.annotated.data.frame.rda')
}

#' @export
constraint_metrics <- function() {
  gene.constraint.metrics.num.only <- readRDS('/Users/gabrielm/Desktop/Rstuff/essentialgenes_rshiny_rhino_test/rda/gene.constraint.metrics.num.only.rda')
}
