#' Geração de gráfico de barras
#'
#' longo
#'
#' descrição
#'
#' @param question_data Tibble.Tabulações relativas à questão principal
#' @param negative_col Character. Nome da questão que possui a conotação negativa
#' @param fill_color Character. Paleta de cores do preenchimento do treemap e da barra
#' @param title_max_size Numeric. Tamanho máximo da linha do texto título do gráfico
#' @param legend_max_size Numeric. Tamanho máximo da linha do texto legenda do gráfico
#' @param axis_max_size Numeric. Tamanho máximo da linha do texto do eixo x do gráfico
#' @param sort_by_value Logical. Indicativo se as categorias utilizadas no fill,
#' devem ser adicionadas ordenadas por valor do percent
#' @param legend_position Character. Indicativo da posição da legenda de fill
#' @param base_text_size Numeric. Tamanho base do texto a ser utilizada no tema
#' @param has_title Logical. indicativo de se deve incluir o título na figura
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
make_bar_graph <- function(question_data,
                           negative_col = FALSE,
                           fill_color = "Oranges",
                           title_max_size = 60,
                           legend_max_size = 10,
                           axis_max_size = 25,
                           sort_by_value = FALSE,
                           legend_position = "bottom",
                           base_text_size = 9,
                           has_title = FALSE,
                           grey_scale = FALSE,
                           with_labels = TRUE) {


  # Preparação dos dados para a geração do gráfico
  tidy_data <- tidy_data_for_bar_graph(data = question_data,
                                       negative_col = negative_col,
                                       sort_by_value = sort_by_value,
                                       legend_max_size = legend_max_size,
                                       axis_max_size = axis_max_size)


  graph_title <- gen_title(data = question_data, max_size = title_max_size, has_title = has_title)
  fill_color_scale <- gen_fill_scale(grey_scale = grey_scale, palette_color = fill_color)
  bar_labels <- gen_bar_label(tidy_data = tidy_data, with_labels = with_labels)
  y_axis_labels <- gen_axis_y_scale(negative_col = negative_col)
  bar_theme <- gen_graph_theme(base_text_size = base_text_size, legend_position = legend_position, data = question_data)


  # geração do gráfico de barras a partir dos dados gerados anteriormente
  tidy_data %>%
    ggplot2::ggplot(aes(x = sub_question, y = percent, fill = answer_label)) +
    ggplot2::geom_col(color = "white") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL, title = graph_title) +
    ggplot2::guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
    fill_color_scale +
    bar_labels +
    y_axis_labels +
    bar_theme
}
