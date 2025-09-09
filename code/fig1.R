library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(MetBrewer)

psych_vars <- c("難以集中精神", "生活無聊感", "課業或工作壓力")

long_df <- df %>%
  select(all_of(psych_vars), target) %>%
  pivot_longer(
    cols = all_of(psych_vars),
    names_to = "變項",
    values_to = "回答"
  ) %>%
  group_by(變項, 回答, target) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(變項, 回答) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# 👉 指定回答順序（Likert 類型）
levels_order <- c("非常同意", "同意", "普通", "不同意", "非常不同意", "未回答/拒答")
long_df$回答 <- factor(long_df$回答, levels = levels_order)

# 👉 轉換 target 為分類標籤
long_df$target <- factor(long_df$target,
                         levels = c(0, 1),
                         labels = c("無沉迷傾向", "有沉迷傾向"))

# ✅ 改用 Cassatt1 色盤中的首尾顏色（增加視覺對比）
cassatt_palette <- met.brewer("Cassatt1")[c(5, 2)]

# ✅ 畫圖
ggplot(long_df, aes(x = 回答, y = prop, fill = target)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  facet_wrap(~變項, ncol = 1, scales = "free_x") +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = cassatt_palette) +
  labs(
    title = "心理健康因子與網路沉迷傾向的關係",
    x = "回答選項",
    y = "比例",
    fill = "網路沉迷傾向"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 20),
    legend.position = "top"
  )

