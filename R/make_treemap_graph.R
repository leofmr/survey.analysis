# Função para fazer gráficos de treemap

make_treemap_graph <- function(data,
                               legend_max_size = 15,
                               fill_color = "Set2",
                               grey_scale = FALSE,
                               with_labels = TRUE) {

  # geração de um objeto de escala de preencimento de um fill para o gráfico de barras
  # caso o gráfico seja na escala de cinza, será utilizado como base a função scale_fill_grey
  # caso contrário será utilizado o scale_fill_brewer com a paleta de cores definida pelo parâmetro fill_color
  if (grey_scale) {
    fill_brewer <- scale_fill_grey(start = 0.3, end = 0.7)
  } else {
    fill_brewer <- scale_fill_brewer(palette = fill_color, direction = 1)
  }

  # caso a opção seja por utilizar rotulos de valor dentro das áreas de gráficos
  # será necessário adicionar ao grupo2 o valor percentual entre parênteses após uma quebra de linha
  # esse rótulo irá ser utilizado dentro do treemap
  if (with_labels) {
    data <- data %>%
      mutate(percent = count / sum(count),
             group2 = paste(group2, "\n(", scales::percent(percent, accuracy = 1), ")", sep = ""))
  }

  # quebra dos textos do group1 para aparecer na legenda do gráfico
  data$group1 <- sapply(data$group1, break_text, max_size = legend_max_size)

  # geração do dataframe de totais do group1
  # essa tabela será utilizada para a ordenação das categorias do grupo1
  # e também serão utilizadas para a confecção do gráfico de barra com
  # a distribuição de respostas pelo grupo1
  group1_totals <- data %>%
    group_by(group1) %>%
    summarise(total = sum(count)) %>%
    ungroup() %>%
    mutate(percent = total / sum(total))

  # geração da ordem para o grupo1 a partir do valor total:
  # isso pode vir a ser melhorado no futuro com um reorder pelo total.
  # e emseguida o vetor com a ordem pode ser extraido com a função levels()
  group1_order <- group1_totals %>%
    arrange(total) %>%
    .$group1

  # aplicando a ordem -> no futuro modificar isso
  data$group1 <- factor(data$group1, levels = group1_order)
  group1_totals$group1 <- factor(group1_totals$group1, levels = group1_order)

  # gerando o gráfico de treemap
  treemap <- data %>%
    ggplot(aes(area = count, subgroup = group1, subgroup2 = group2, fill = group1, label = group2)) +
    geom_treemap(start = "topleft", layout = "srow") +
    geom_treemap_text(grow = FALSE, reflow = TRUE, min.size = 6, size = 10, place = "centre", start = "topleft", layout = "srow") +
    geom_treemap_subgroup_border(color = "white", size = 2, start="topleft", layout = "srow") +
    geom_treemap_subgroup2_border(color = "white", size= 0.001, start="topleft", layout = "srow", linetype="dotdash") +
    theme_bw(base_size = 9) +
    fill_brewer +
    theme(legend.position = "bottom") +
    labs(fill = NULL) +
    guides(fill = guide_legend(reverse = TRUE, nrow = 1))

  # gerando o gráfico de barra
  bar_graph <- group1_totals %>%
    ggplot(aes(x = "", y = percent, fill = group1)) +
    geom_col(color = "white") + coord_flip() +
    scale_y_continuous(limits=c(0, 1), expand = c(0, 0)) +
    labs(x = NULL, y = NULL, fill = NULL, title = NULL) +
    fill_brewer +
    theme_bw(base_size = 9) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          panel.grid = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank())

  # caso a opção por adicionar labels aos dados, adicionar ao gráfico
  # gerado anteriormente labels
  # as labels só serão adicionadas caso a opção tenha percent superior à 5%
  if (with_labels) {
    bar_graph <- bar_graph +
      geom_text(data = group1_totals,
                aes(x = "",
                    y = percent,
                    group = group1,
                    label = ifelse(abs(percent) > 0.05, scales::percent(abs(percent), accuracy = 1), "")),
                position = position_stack(vjust = 0.5),
                size = 2)
  }

  # gerando o gráfico final composto pelo treemap, e o gráfico de barras
  ggdraw() +
    draw_plot(treemap, x = 0, y = 0.1, width = 1, height = 0.9) +
    draw_plot(bar_graph, x = 0, y = 0, width = 1, height = 0.1)
}
