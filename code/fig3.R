library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(MetBrewer)
library(patchwork)  # ✅ 方便多張圖組合

# 人口變項
demo_vars <- c("年齡", "性別", "職業", "教育程度", "居住地區", "是否居住直轄市")

# 各變項選項順序
answer_levels <- list(
  "年齡" = c("12-17歲", "18-19歲", "20-29歲", "30-39歲", "40-49歲", "50-59歲", "60-64歲", "65歲以上"),
  "性別" = c("男", "女"),
  "職業" = c("白領", "藍領", "服務業", "非工作人口", "其他"),
  "教育程度" = c("國中以下", "高中高職", "專科大學", "研究所以上"),
  "居住地區" = c("北部", "中部", "南部", "東部與離島"),
  "是否居住直轄市" = c("否", "是")
)

# 對應轉換
df$`是否居住直轄市` <- ifelse(df$`是否居住直轄市` == 1, "是", "否")

# 用來存每張圖
plots <- list()

# 為每一題畫出甜甜圈圖
for (var in demo_vars) {
  temp <- df %>%
    filter(!is.na(.data[[var]])) %>%
    count(選項 = .data[[var]]) %>%
    mutate(
      prop = n / sum(n),
      angle = cumsum(prop) - prop / 2,
      label = paste0(選項, "\n", percent(prop)),
      選項 = factor(選項, levels = answer_levels[[var]])
    )
  
  n_colors <- nrow(temp)
  fill_colors <- colorRampPalette(met.brewer("Cassatt1"))(n_colors)
  
  p <- ggplot(temp, aes(x = 2, y = prop, fill = 選項)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    scale_fill_manual(values = fill_colors) +
    xlim(0.5, 2.5) +
    labs(title = var, fill = NULL) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "right"
    )
  
  plots[[var]] <- p
}

# 使用 patchwork 將圖拼接起來（兩排三圖）
wrap_plots(plots, ncol = 2)
