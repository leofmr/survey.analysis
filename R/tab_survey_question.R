#' Tabulação de questão do Survey Monkey
#'
#' Realiza as tabulações para uma questão do survey monkey,
#' calculando o p-valor para testar aderência entre nível de conhecimento.
#'
#' Essa função irá fazer as tabulações com percepção das escolhas para cada resposta, de questões principais.
#' Dado um número de questão principal, as respostas de todas as subquestão são também calculadas.
#'
#' @param dt Tibble. dataset composto por todas as respostas do survey
#' @param question_number Numeric. número da questão que vai ser tabulada
#' @param question_label Tibble. tabela com a relação de indice de questão e legendas de questão.
#' @param answer_label Tibble. tabela com a relação entre questão principal e legendas de respostas.
#'
#' @author Leonardo Rocha
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#'
#' @export
#'
#' @return Tibble com as frequências por nível de conhecimento, resposta e subquestões calculadas,
#' com as questões e opções rotuladas, e o p-valor do teste de aderência por nível de conhecimento
tab_suvey_question <- function(dt, question_number, question_label, answer_label) {

  # geração do string que vai ser utilizado para poder selecionar as colunas de dt
  # que serão utilizadas para a tabulação
  question_start <- paste("q", question_number, "_", sep = "")

  # tabulação principal e simples
  # agrupando por subquestão (answer)
  # eliminando as linhas sem respostas
  # transformando as respostas e o conhecimento em fator
  # calculando as contagens por conhecimento e resposta (mas mantendo a questão para adicionar rotulo)
  # por fim completando aquelas respostas que não tiveram frequência para algum grupo com 0
  qtab <- dt %>%
    dplyr::select("q1_1", starts_with(question_start)) %>%
    dplyr::rename(knowledge = 1) %>%
    tidyr::gather(question, answer, -1) %>%
    dplyr::filter(answer != "") %>%
    dplyr::mutate(answer = factor(answer),
           knowledge = factor(knowledge)) %>%
    dplyr::group_by(knowledge, question, answer) %>%
    dplyr::summarise(count = n()) %>%
    tidyr::complete(answer, fill = list(count = 0)) %>%
    dplyr::ungroup()

  #calculando o p-valor do teste qui-quadrado por questão.
  # isso é para ver se tem alguma diferença por nível de conhecimento
  # para isso são geradas tabelas de contigência, agrupadas por questão
  # são então cada tabela de contigência é agrupada para questao em uma variável
  # data onde fica localizada a tabela de contigência
  p_values <- qtab %>%
    tidyr::spread(answer, count) %>%
    dplyr::select(-knowledge) %>%
    dplyr::group_by(question) %>%
    tidyr::nest() %>%
    dplyr::mutate(p_value = purrr::as_vector(lapply(data, extract_p))) %>%
    dplyr::select(-data) %>%
    dplyr::ungroup()

  # geração de uma tabela com os totais, dos dois níveis de conhecimento
  # é adicionado o valor 3 para conhecimento que será representante de total
  qtab_total <- qtab %>%
    dplyr::group_by(question, answer) %>%
    dplyr::summarise(count = sum(count),
              knowledge = "3") %>%
    dplyr::ungroup()

  # agrugem dos valores de p_valor por questão
  # nível de conhecimento total
  # também são contabilizados os totais por questão e nível de conhecimento,
  # e contabilizados os valores percentuais de cada resposta
  # ao final são aplicadas as labels às variáveis categóricas representadas por números:
    # questão principal
    # sub questão
    # resposta
  qtab %>%
    dplyr::bind_rows(qtab_total) %>%
    dplyr::group_by(question, knowledge) %>%
    dplyr::mutate(total = sum(count),
           percent = count / total) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(p_values, by = "question") %>%
    apply_labels(question_label = question_label, answer_label = answer_label)
}
