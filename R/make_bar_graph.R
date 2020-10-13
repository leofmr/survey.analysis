#' Geração de gráfico de barras
#'
#' longo
#'
#' descrição
#'
#' @param data Tibble. survey dataset completo
#' @param negative_col Character. Nome da questão que possui a conotação negativa
#' @param fill_color Character. Paleta de cores do preenchimento do treemap e da barra
#' @param title_max_size Numeric. Tamanho máximo da linha do texto título do gráfico
#' @param legend_max_size Numeric. Tamanho máximo da linha do texto legenda do gráfico
#' @param axis_max_size Numeric. Tamanho máximo da linha do texto do eixo x do gráfico
#' @param sort_by_value Logical. Indicativo se as categorias utilizadas no fill,
#' devem ser adicionadas ordenadas por valor do percent
#' @param legend_position Character. Indicativo da posição da legenda de fill
#' @param base_text_size Numeric. Tamanho base do texto a ser utilizada no tema
#' @param title Logical. indicativo de se deve incluir o título na figura
#' @param grey_scale Logical. indicativo se deve ser utilizada escala de cinza
#' @param with_labels Logical. indicativo se desemv ser adicionados os percentuais no gráfico
#'
#' @return Gráfico customizado de barras na horizontal gerado pelo ggplot
#'
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import scales
#'
#' @author Leonardo Rocha
#'
make_bar_graph <- function(data,
                           negative_col = FALSE,
                           fill_color = "Oranges",
                           title_max_size = 60,
                           legend_max_size = 10,
                           axis_max_size = 25,
                           sort_by_value = FALSE,
                           legend_position = "bottom",
                           base_text_size = 9,
                           title = FALSE,
                           grey_scale = FALSE,
                           with_labels = TRUE) {

  # se a opção for para ter título ele deve ser gerado pela main_question,
  # o título vai ser quebrado com quebras de linha através da função break_text
  # o tamanho máximo é determinado pelo parâmetro title_max_size
  if (title) {
    graph_title <- break_text(first(data$main_question), max_size = title_max_size)
  } else {
    graph_title <- NULL
  }

  # geração de um objeto de escala de preencimento de um fill para o gráfico de barras
  # caso o gráfico seja na escala de cinza, será utilizado como base a função scale_fill_grey
  # caso contrário será utilizado o scale_fill_brewer com a paleta de cores definida pelo parâmetro fill_color
  if (grey_scale) {
    fill_brewer <- scale_fill_grey(start = 0.7, end = 0.3)
  } else {
    fill_brewer <- scale_fill_brewer(palette = fill_color, direction = 1)
  }

  # definição do limite inferior do eixo x
  # se o gráfico tiver alguma opção com contexto negativo, que
  # seja definida para o sentido negativo do gráfico
  # o limite inferior do eixo x será definido como -1
  # caso contrário, será definido como 0
  if (typeof(negative_col) == "character") {
    axis_x_limit <- -1
  } else {
    axis_x_limit <- 0
  }

  # preparação dos dados para a geração do gráfico
  # Apenas todos os conhecimentos serão selecionados
  # os valores percentuais das linhas com a escolha de conotação negativa
  # são transformados para o negativo
  # foi feito de uma certa forma que, caso negative_col seja equivalente a FALSE
  # o percent não será alterado
  # como a resposta (answer_label) vai para o legenda do gráfico
  # quebra do answer_label em multiplas linhas com a função break_text com o parâmetro legend_max_size
  # caso a opção de ordenar as respostas por frequência, a resposta será ordenada pela percent
  # como a subquestion vai para o eixo x (que invertido fica como se fosse o eixo y)
  # quebra das subquestions coma função break_text utilizando como parâmetro axis_max_size
  tidy_data <- data %>%
    filter(knowledge == "Total") %>%
    select(sub_question, answer_label, percent) %>%
    mutate(percent = ifelse(answer_label == negative_col, -percent, percent),
           answer_label = factor(answer_label, labels = lapply(levels(answer_label), break_text, max_size = legend_max_size)),
           answer_label = ifelse(sort_by_value, reorder(answer_label, percent), answer_label),
           sub_question = factor(sub_question, labels = lapply(levels(sub_question), break_text, max_size = axis_max_size)))

  # geração do gráfico de barras a partir dos dados gerados anteriormente
  graph <-  tidy_data %>%
    ggplot(aes(x = sub_question, y = percent, fill = answer_label)) +
    geom_col(color = "white") + coord_flip() +
    labs(x = NULL, y = NULL, fill = NULL, title = graph_title) +
    fill_brewer +
    scale_y_continuous(limits=c(axis_x_limit, 1), breaks = seq(axis_x_limit, 1, 0.25),
                       labels = scales::percent(abs(seq(axis_x_limit, 1 , 0.25)))) +
    theme_bw(base_size = base_text_size) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(legend.position = legend_position,
          panel.grid = element_blank()) +
    guides(fill = guide_legend(reverse = TRUE, nrow = 1))

  # caso a opção por adicionar labels aos dados, adicionar ao gráfico
  # gerado anteriormente labels
  # as labels só serão adicionadas caso a opção tenha percent superior à 5%
  if (with_labels) {
    graph <- graph +
      geom_text(data = tidy_data,
                aes(x = sub_question,
                    y = percent,
                    group = answer_label,
                    label = ifelse(abs(percent) > 0.05, scales::percent(abs(percent), accuracy = 1), "")),
                position = position_stack(vjust = 0.5),
                size = 2)
  }

  # caso a main question não tenha subquestion,
  # os elementos gráficos referentes ao eixo y serão eliminados
  # isso é necessário para não ficar um NA ou um tick no eixo.
  if (is.na(first(data$sub_question))) {
    graph <- graph +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  }

  return(graph)
}
