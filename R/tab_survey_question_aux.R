# funções auxiliares ao tab_survey_question

# extração do p-valor das tabelas de contingência aninhadas
extract_p <- function(x) {
  chisq.test(x, simulate.p.value = TRUE)$p.value
}

# geração do nome da questão - esse daqui talvez seja desnecesário
gen_names <- function(number) {
  paste("q", number, sep = "")
}

# função de suporte que aplica label ao dataframe
# aplicada ao final da geração do tab_survey para atribuir os rótulos
# nos arquivos de rótulo de questão e rótulo de resposta
# aos dados do arquivo de dados.
apply_labels <- function(data, question_label, answer_label) {
  data %>%
    left_join(question_label, by="question") %>%
    separate(question, into=c("main_code", "sub_code")) %>%
    mutate(main_code = str_remove(main_code, "q"),
           answer = as.character(answer)) %>%
    left_join(answer_label, by=c("main_code", "answer")) %>%
    mutate(
      main_code = as.integer(main_code),
      sub_code = as.integer(sub_code),
      answer = as.integer(answer),
      main_question = reorder(main_question, main_code),
      sub_question = reorder(sub_question, sub_code),
      answer_label = reorder(answer_label, answer),
      knowledge = factor(knowledge, labels = c("Good knowledge", "Some knowledge", "Total"))
    ) %>%
    select(main_question, sub_question, p_value, knowledge, answer_label, count, percent, total) %>%
    arrange(sub_question, knowledge, answer_label)
}
