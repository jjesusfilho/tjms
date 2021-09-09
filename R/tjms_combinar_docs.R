#' Combinar pdfs baixados
#'
#' @param arquivos Vetor de arquivos
#' @param dir_origem Diretório de origem se arg arquivos não informado
#' @param dir_destino Diretório destino dos pdfs combinados
#'
#' @return pdf
#' @export
#'
tjms_combinar_docs <- function(arquivos = NULL, dir_origem = ".",  dir_destino = NULL){

  if (is.null(dir_destino) || !dir.exists(dir_destino)){

    stop("Você deve informar um diretório existente")

  }

  if (is.null(arquivos)){

    arquivos <- list.files(dir_origem, full.names = TRUE, pattern = "pdf$")

  }




  lista <- tibble::tibble(arquivos) %>%
    dplyr::mutate(processo = stringr::str_extract(arquivos,"\\d+"),
                  doc_id = stringr::str_extract(arquivos,'\\d+(?=\\.pdf)')) %>%
    #inner_join(select(df,processo,doc_id,url_doc), by = c("processo","doc_id")) %>%
    dplyr::mutate(doc_id = as.integer(doc_id)) %>%
    dplyr::arrange(processo,doc_id) %>%
    dplyr::group_split(processo)

  purrr::walk(lista, ~{

    processo <- unique(.x$processo)

    qpdf::pdf_combine(.x$arquivos, file.path(dir_destino,paste0(processo,".pdf")))

  })


}
