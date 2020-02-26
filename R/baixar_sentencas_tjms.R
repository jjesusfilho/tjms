#' Baixa sentencas do tjms a partir de tabela de sentencas retirada da
#'     movimentação
#'
#' @param df Movimentação com url da sentença
#' @param diretorio diretório onde serão armazenadas as sentenças
#'
#' @details Os arquivos são armazenados sem extenção, pois não sabemos se
#'     eles são rtf ou pdf. A função ler_sentencas_tjms se encarrega de
#'     fazer essa verificação.
#' @return Arquivos sem extensão. Não dá para saber se o arquivo é pdf ou
#'     rtf.
#' @export
#'
baixar_sentencas_tjms <- function(df, diretorio = ".") {
  df <- df %>%
    dplyr::filter(!is.na(doc_link))

  lista <- slider::slide(df, ~.x)

  url <- "https://esaj.tjms.jus.br/pastadigital/getRTF.do?"

  purrr::walk(lista, purrr::possibly(purrrogress::with_progress(~ {
    cd_documento <- .x$doc_link %>%
      stringr::str_extract("(?<=cdDocumento.)\\d+")

    codigo <- .x$doc_link %>%
      stringr::str_extract("(?<=codigo.).+?(?=&)")

    processo <- abjutils::build_id(.x$processo)
    foro <- stringr::str_extract(.x$processo, "\\d{2}$")
    query <-
      list(
        nuSeqRecurso = "00000",
        nuProcesso = processo,
        cdDocumento = cd_documento,
        conferenciaDocEdigOriginal = "false",
        nmAlias = "PG5MS",
        origemDocumento = "M",
        nuPagina = "0",
        numInicial = "1",
        tpOrigem = "2",
        flOrigem = "P",
        deTipoDocDigital = "Senten\xe7a",
        cdProcesso = codigo,
        cdFormatoDoc = "2",
        cdForo = foro,
        idDocumento = paste0(cd_documento, "-1-0"),
        numFinal = "1",
        sigiloExterno = "N"
      )
    arquivo <- file.path(diretorio, paste0("sentenca_processo_", .x$processo, "_documento_", codigo))
    httr::GET(url, query = query, httr::write_disk(arquivo, overwrite = TRUE))
  }), NULL))
}
