# função que gera as tabulações para questões principais,
# com os percentuais divididos por nível de conhecimento

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
    select("q1_1", starts_with(question_start)) %>%
    rename(knowledge = 1) %>%
    gather(question, answer, -1) %>%
    filter(answer != "") %>%
    mutate(answer = factor(answer),
           knowledge = factor(knowledge)) %>%
    group_by(knowledge, question, answer) %>%
    summarise(count = n()) %>%
    complete(answer, fill = list(count = 0)) %>%
    ungroup()

  #calculando o p-valor do teste qui-quadrado por questão.
  # isso é para ver se tem alguma diferença por nível de conhecimento
  # para isso são geradas tabelas de contigência, agrupadas por questão
  # são então cada tabela de contigência é agrupada para questao em uma variável
  # data onde fica localizada a tabela de contigência
  p_values <- qtab %>%
    spread(answer, count) %>%
    select(-knowledge) %>%
    group_by(question) %>%
    nest() %>%
    mutate(p_value = as_vector(lapply(data, extract_p))) %>%
    select(-data) %>%
    ungroup()

  # geração de uma tabela com os totais, dos dois níveis de conhecimento
  # é adicionado o valor 3 para conhecimento que será representante de total
  qtab_total <- qtab %>%
    group_by(question, answer) %>%
    summarise(count = sum(count),
              knowledge = "3") %>%
    ungroup()

  # agrugem dos valores de p_valor por questão
  # nível de conhecimento total
  # também são contabilizados os totais por questão e nível de conhecimento,
  # e contabilizados os valores percentuais de cada resposta
  # ao final são aplicadas as labels às variáveis categóricas representadas por números:
    # questão principal
    # sub questão
    # resposta
  qtab %>%
    bind_rows(qtab_total) %>%
    group_by(question, knowledge) %>%
    mutate(total = sum(count),
           percent = count / total) %>%
    ungroup() %>%
    left_join(p_values, by = "question") %>%
    apply_labels(question_label = question_label, answer_label = answer_label)
}
