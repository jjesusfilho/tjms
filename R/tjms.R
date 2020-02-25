#' \code{tjms} package
#'
#' Baixa  e organiza decisões do TJMS
#'
#'
#' @docType package
#' @name tjms
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("doc_link", "valor", "variavel", "row_id"))
}
