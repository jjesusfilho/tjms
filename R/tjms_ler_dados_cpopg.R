#' Lê metadados dos processos de primeiro grau do TJMS
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Se não informados os arquivos, informar o diretório
#' @param wide Se TRUE, as informações serão dispostas em wide format.
#'
#' @return tibble
#' @export
#'
tjms_ler_dados_cpopg <- function(arquivos = NULL, diretorio = ".", wide = FALSE) {
  if (is.null(arquivos)) {
    arquivos <- list.files(
      path = diretorio, pattern = ".html",
      full.names = TRUE
    )
  }
  dados <- purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~ {
    processo <- stringr::str_extract(.x, "\\d{20}")


    resposta <- .x %>% xml2::read_html()
    digital <- resposta %>% xml2::xml_find_first("boolean(//*[@class='linkPasta'] |//*[@class='linkConsultaSG'])")

    codigo <- resposta %>%
      xml2::xml_find_all("//a[contains(@href,'processo.codigo')]") %>%
      xml2::xml_attr("href") %>%
      stringr::str_extract("(?<=processo.codigo=)\\w+")

    if (length(codigo) > 1) {
      codigo <- duplicated(codigo) %>%
        which() %>%
        codigo[.] %>%
        unique()
    }
    cdProcesso <- resposta %>%
      xml2::xml_find_first("//*[@name='cdProcesso']") %>%
      xml2::xml_attr("value")

    variavel <- resposta %>%
      xml2::xml_find_all("//table[@id=''][@class='secaoFormBody']//*[@width='150']") %>%
      xml2::xml_text() %>%
      stringr::str_squish()

    valor <- resposta %>%
      xml2::xml_find_all("//table[@id=''][@class='secaoFormBody']//*[@width='150']/following-sibling::td") %>%
      xml2::xml_text() %>%
      stringr::str_squish()
    tibble::tibble(
      processo = processo, codigo_processo = codigo,
      cd_processo = cdProcesso, digital, variavel, valor
    )
  }), NULL))
  if (wide == TRUE) {
    dados <- dados %>%
      dplyr::group_by_at(dplyr::vars(-valor)) %>%
      dplyr::mutate(row_id = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = variavel, value = valor) %>%
      dplyr::select(-row_id)
  }
  return(dados)
}
