#' Relacionar tabelas
#'
#' Relaciona duas tabelas de frequências de respostas de questões relacionadas no survey
#'
#' Pega a tabela de uma questão que é considerada filha de outra, e seleciona apenas as contagens de
#' nível de conhecimento total, e cria uma nova coluna que é equivalente à opção equivalente da questão pai.
#' Essa função deve ser aplicada a todas as questões filhas de um mês pai para depois a lista de resultados
#' dessa função ser empilhada pela rowbindlist.
#' Para o bom funcionamento da questão a numeração das respostas da questão pai precisa seguir
#' a ordem da questão filha na lista de perguntas. Ou seja se, a opção 1 é equivalente à questão imediamente
#' após a questão pai.
#'
#' @param data List. A lista com todos os dados nomeados pelo nome da questão
#' @param qfather Numeric. O número da questão pai
#' @param nson Numeric. O número da questão filha
#'
#' @return Tibble. O dado da filha com a coluna respectiva ao nome da pai
#'
#' @author Leonardo Rocha
#'
#' @import dplyr
#'
#' @export
#'
link_tabs <- function(data, qfather, nson) {

  # pega o numero do filho e do pai
  father <- data[[paste("q", qfather, sep = "")]]
  son <- data[[paste("q", qfather + nson, sep = "")]]
  link <- nson

  # gera uma tabela do filho vinculada ao pai
  son %>%
    dplyr::filter(knowledge == "Total") %>%
    dplyr::select( answer_label, count) %>%
    dplyr::rename("group2" = 1,
           "count" = 2) %>%
    dplyr::mutate(group1 = levels(father$answer_label)[link],
           group2 = as.character(group2)) %>%
    dplyr::select(group1, group2, count)
}
