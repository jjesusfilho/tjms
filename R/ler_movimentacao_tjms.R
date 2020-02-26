#' Lê movimentação processual do tjms
#'
#' @param arquivos Vetor com os caminhos para os arquivos
#' @param diretorio Se não informar os arquivos, informar o diretório
#'
#' @return tibble
#' @export
#'
ler_movimentacao_tjms <-function(arquivos = NULL,diretorio = ".") {
  if (is.null(arquivos)){
    arquivos <- list.files(
      path = diretorio, pattern = ".html",
      full.names = TRUE
    )
  }
  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    processo <- stringr::str_extract(.x, "\\d{20}")
    texto <- xml2::read_html(.x) %>%
      xml2::xml_find_first(xpath = "//table/tbody[@id='tabelaTodasMovimentacoes']")
    data <- xml2::xml_find_all(texto, ".//td[@width='120']") %>%
      xml2::xml_text(trim = TRUE)
    mov <- xml2::xml_find_all(texto, ".//td[@style='vertical-align: top; padding-bottom: 5px']") %>%
      xml2::xml_text(trim = TRUE)
    doc_link <-xml2::xml_find_all(texto, ".//td[@width='120']/following-sibling::td[2]") %>%
      xml2::xml_child("a") %>%
      purrr::map(~xml2::xml_attr(.x,"href")) %>%
      unlist()
    tibble::tibble(processo = processo, data = data, movimentacao = mov, doc_link)
  }), otherwise = NULL))
}
