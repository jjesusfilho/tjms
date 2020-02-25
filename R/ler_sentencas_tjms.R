#' Lê sentenças baixadas com função baixar_sentencas_tjms
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Se não informados os arquivos, informar o diretório
#'
#' @details Esta função antes de ler, verifica se o texto é rtf ou pdf.
#' @return tibble com número do processo, número do documento e inteiro teor.
#' @export
#'
ler_sentencas_tjms <- function(arquivos = NULL, diretorio = ".") {
  if (is.null(arquivos)) {
    arquivos <- list.files(diretorio, "sentenca_", full.names = T)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~ {
    if (striprtf::looks_rtf(.x)) {
      texto <- texto <- unrtf::unrtf(.x, "text") %>%
        stringr::str_remove("\\X+---------+")
    } else {
      texto <- pdftools::pdf_text(.x) %>%
        paste0(collapse = "\n")
    }

    processo <- stringr::str_extract(.x, "\\d{20}")

    cd_documento <- stringr::str_extract(.x, "(?<=documento_).+")

    tibble::tibble(processo, cd_documento, texto)
  }), NULL))
}
