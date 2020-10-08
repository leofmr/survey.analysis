# função de suporte ao gráfico de treemap de vinculamento de uma questão com outras
# deve ser aplicado caso uma questão seja depentente de outra por contrução do questionário
# nesse caso a questão independente é a pai enquanto que as dependentes são os filhos
# como as questões dependentes ocorrem após a questão independente.
# é necessário forneceder o número da questão independente
# acompanhada da quantidade de questões dependentes posteriores.


# preciso alterar essa função para incorporar a parte de rbindlist do data.table
link_tabs <- function(data, qfather, nson) {

  # pega o numero do filho e do pai
  father <- data[[paste("q", qfather, sep = "")]]
  son <- data[[paste("q", qfather + nson, sep = "")]]
  link <- nson

  # gera uma tabela do filho vinculada ao pai
  son %>%
    filter(knowledge == "Total") %>%
    select( answer_label, count) %>%
    rename("group2" = 1,
           "count" = 2) %>%
    mutate(group1 = levels(father$answer_label)[link],
           group2 = as.character(group2)) %>%
    select(group1, group2, count)
}
