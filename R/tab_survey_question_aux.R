#' Extração de p-valor
#'
#' Extração de p-valor por um teste de aderência do tipo da qui-quadrado,
#' aplicado a uma tabela de contingência. Utilizado dentro da tabulação das
#' questões do survey para gerar a coluna de p-valor porquestão principal.
#'
#' @import stats
#'
#' @param x Tibble. tabela de contingência
#'
#' @return Numeric. P-valor relativo à contingência
#'
#'
extract_p <- function(x) {
  stats::chisq.test(x, simulate.p.value = TRUE)$p.value
}

#' gerar nomes
#' @param number Numeric. Gera o nome da questão a partir do número
#'
#' @return Character. Nome da questão principal composto pelo número precedido
#' pelo caracter "q".
#'
gen_names <- function(number) {
  paste("q", number, sep = "")
}

#' Aplicação de rótulo
#'
#' Utiliza os dados de rótulo de questões e resposta para aplicar aos dados
#'
#' @param data Tibble. Dados já organizados com os percentuais calculados
#' @param question_label Tibble. Dados gerados pelo usuário com os rótulos
#' de questão vinculado aos identificadores de questão
#' @param answer_label Tibble. Dados gerados pelo usuário com os rótulos
#' de respostas vinculadas aos identificadores de questão
#'
#' @import tidyr
#' @import dplyr
#'
apply_labels <- function(data, question_label, answer_label) {
  data %>%
    dplyr::left_join(question_label, by="question") %>%
    tidyr::separate(question, into=c("main_code", "sub_code")) %>%
    dplyr::mutate(main_code = str_remove(main_code, "q"),
           answer = as.character(answer)) %>%
    dplyr::left_join(answer_label, by=c("main_code", "answer")) %>%
    dplyr::mutate(
      main_code = as.integer(main_code),
      sub_code = as.integer(sub_code),
      answer = as.integer(answer),
      main_question = reorder(main_question, main_code),
      sub_question = reorder(sub_question, sub_code),
      answer_label = reorder(answer_label, answer),
      knowledge = factor(knowledge, labels = c("Good knowledge", "Some knowledge", "Total"))
    ) %>%
    dplyr::select(main_question, sub_question, p_value, knowledge, answer_label, count, percent, total) %>%
    dplyr::arrange(sub_question, knowledge, answer_label)
}
