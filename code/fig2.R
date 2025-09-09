# 套件載入
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(MetBrewer)
library(purrr)

# 變項與回答順序設定
vars <- c(
  "擔心居家隔離檢疫.失去他人陪伴情況",
  "擔心外出確診.減少外出情況",
  "因疫情失去工作或工作減量的情況",
  "疫情是否有居家上班或上課的情況"
)

answer_levels <- list(
  "擔心居家隔離檢疫.失去他人陪伴情況" = c("幾乎總是", "經常", "有時", "很少", "從來沒有", "未回答/拒答"),
  "擔心外出確診.減少外出情況"         = c("幾乎總是", "經常", "有時", "很少", "從來沒有", "未回答/拒答"),
  "因疫情失去工作或工作減量的情況"     = c("很少", "從來沒有", "未回答/拒答"),
  "疫情是否有居家上班或上課的情況"     = c("從來沒有", "很少", "未回答/拒答")
)

# 資料轉換，每題分別處理 levels，只保留有出現過的回答選項
plot_data <- map_dfr(vars, function(varname) {
  temp <- df %>%
    select(target, !!sym(varname)) %>%
    rename(回答 = !!sym(varname)) %>%
    filter(!is.na(回答))
  
  actual_levels <- intersect(answer_levels[[varname]], unique(temp$回答))
  
  temp %>%
    group_by(回答, target) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(回答) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup() %>%
    mutate(
      變項 = varname,
      回答 = factor(回答, levels = actual_levels)
    )
})

# 轉換目標變數標籤
plot_data$target <- factor(plot_data$target,
                           levels = c(0, 1),
                           labels = c("無沉迷傾向", "有沉迷傾向"))

# 美觀配色（Cassatt1 首尾）
cassatt_palette <- met.brewer("Cassatt1")[c(5, 2)]

# 繪製圖表
ggplot(plot_data, aes(x = 回答, y = prop, fill = target)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  facet_wrap(~變項, ncol = 1, scales = "free_x") +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = cassatt_palette) +
  labs(
    x = "回答選項",
    y = "比例",
    fill = "網路沉迷傾向"
  ) +
  theme_minimal(base_size = 8) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "top"
  )
