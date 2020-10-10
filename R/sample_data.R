#' Exemplo de dataset de survey no formato adequado
#'
#' O dataset de exemplo é uma lista composta pela tabela, e os labels de questão e respostas
#'
#' O dataset contém as respostas no individuais, onde cada resposta é representada por um número inteiro
#' A resposta base começa como 1 e vai subindo até a quantidade total de possibilidades de resposta
#' Existem três tipos de questões no survey:
#'  1. Questões de múltipla escolha,
#'  2. Questões compostas por subquestões de multipla escolha,
#'  3. Questões de múltipla escolha condicionadas a outras múltipla escolhas.
#' Cada coluna do dataset é contém uma questão/subquestão.
#' Os nomes da colunas são compostos por dois números qX_Y,
#'  X -> é o indicador da questão
#'  Y -> é o indicador da subquestão
#' Questões que não possuem subquestões tem o Y=1
#' Por padrão a primeira questão é a de nível de conhecimento

