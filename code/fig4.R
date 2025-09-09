# 先處理：轉換標籤
df$自評是否網路沉迷 <- factor(df$自評是否網路沉迷,
                          levels = c(0, 1),
                          labels = c("無自評沉迷", "自評有沉迷"))
df$target <- factor(df$target,
                    levels = c(0, 1),
                    labels = c("無沉迷傾向", "有沉迷傾向"))

# 製作兩題的甜甜圈圖
make_doughnut <- function(varname, title_text) {
  temp <- df %>%
    filter(!is.na(.data[[varname]])) %>%
    count(分類 = .data[[varname]]) %>%
    mutate(
      prop = n / sum(n),
      label = paste0(分類, "\n", percent(prop))
    )

  n_colors <- nrow(temp)
  fill_colors <- met.brewer("Cassatt1")[c(5, 2)]

  ggplot(temp, aes(x = 2, y = prop, fill = 分類)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    scale_fill_manual(values = fill_colors) +
    labs(title = title_text, fill = NULL) +
    theme_void(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "right"
    )
}

# 圖片產出
p1 <- make_doughnut("自評是否網路沉迷", "自評是否網路沉迷")
p2 <- make_doughnut("target", "實際是否網路沉迷（target）")

# 並排顯示
library(patchwork)
p1 + p2
