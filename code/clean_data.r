library(dplyr)

data <- read.csv("data111.csv", header = TRUE, sep = ",")
head(data)

# 使用網路設備的經驗（全部都有）
table(data$v1)



# 因為Covid所以使用增加、減少、不變、拒答
table(data$v2)

# 在工作或上課日，使用的狀況，
#(99994) 沒有工作也沒有上學【跳問Q5】
#(99995) 超過一個月沒上網【跳問Q31】
#(99997) 不知道【續問 Q4】
#(99998) 未回答/拒答【續問Q4】
table(clean_data$v3)

#---------------------------------------#
# 這部分選擇刪除99995
clean_data <- data[data$v3 != 99995, ]
#---------------------------------------#

# 非工作或上課日，使用的狀況，時間、不知道、拒答
table(data$v4)

# 沒工作沒上學的人，使用狀況（續3）
table(data$v5)

# 第六題是，使用的網路活動，複選題


# 量表
# 刪除第三題，超過一個月沒有上網的部分（他會跳過這部分直接回答個人資訊）
table(clean_data$v8)

# 因為是關鍵的目標變數，如過量表中拒絕回答，會無法計算是否網路成癮
#---------------------------------------#
# 這部分選擇刪除回答為8的資料
clean_data <- clean_data[clean_data$v8 != 8, ]
#---------------------------------------#


table(clean_data$v9)
#---------------------------------------#
# 這部分選擇刪除回答為8的資料
clean_data <- clean_data[clean_data$v9 != 8, ]
#---------------------------------------#


table(clean_data$v10)
#---------------------------------------#
# 這部分選擇刪除回答為8的資料
clean_data <- clean_data[clean_data$v10 != 8, ]
#---------------------------------------#


table(clean_data$v11)
#---------------------------------------#
# 這部分選擇刪除回答為8的資料
clean_data <- clean_data[clean_data$v11 != 8, ]
#---------------------------------------#


table(clean_data$v12)
#---------------------------------------#
# 這部分選擇刪除回答為8的資料
clean_data <- clean_data[clean_data$v12 != 8, ]
#---------------------------------------#


table(clean_data$v13)
#---------------------------------------#
# 這部分選擇刪除回答為8的資料
clean_data <- clean_data[clean_data$v13 != 8, ]
#---------------------------------------#


table(clean_data$v14)
#---------------------------------------#
# 這部分選擇刪除回答為8的資料
clean_data <- clean_data[clean_data$v14 != 8, ]
#---------------------------------------#


table(clean_data$v15)
#---------------------------------------#
# 這部分選擇刪除回答為8的資料
clean_data <- clean_data[clean_data$v15 != 8, ]
#---------------------------------------#


table(clean_data$v16)
#---------------------------------------#
# 這部分選擇刪除回答為8的資料
clean_data <- clean_data[clean_data$v16 != 8, ]
#---------------------------------------#


table(clean_data$v17)
#---------------------------------------#
# 這部分選擇刪除回答為8的資料
clean_data <- clean_data[clean_data$v17 != 8, ]
#---------------------------------------#


# 新增一個欄位叫做「total」，如果8~17的分數加總就好
clean_data$total <- rowSums(clean_data[, c("v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17")], na.rm = TRUE)

# 如果total大於等於28，就在[target]欄位標記為1，否則標記為0
clean_data$target <- ifelse(clean_data$total >= 28, 1, 0)

table(clean_data$total)
table(clean_data$target)

# 是否成癮自評區

# 有、無、拒答（拒答的人有三）
table(clean_data$v18)

# 沒有回答的人佔一千二（資料總數約一千四）
table(clean_data$v19)
table(clean_data$v20)



table(clean_data$v21)
table(clean_data$v22)
table(clean_data$v23)
table(clean_data$v24)
table(clean_data$v25)
table(clean_data$v26)
table(clean_data$v27)

# 憂鬱
table(clean_data$v28)
# 無聊感
table(clean_data$v29)
# 課業或工作壓力
table(clean_data$v30)


# 年齡都有回答
# 分群
table(clean_data$v31)

table(clean_data$target, clean_data$v31)

# 工作，有六個沒回答
table(clean_data$v32)
table(clean_data$target, clean_data$v32)


clean_data$v32 <- as.numeric(clean_data$v32)  # 假設工作代碼欄位為字串，要先轉數字

clean_data$job_group <- dplyr::case_when(
  clean_data$v32 %in% c(2, 3, 4, 5) ~ "白領",
  clean_data$v32 %in% c(6, 8) ~ "服務業",
  clean_data$v32 %in% c(7, 9, 10) ~ "藍領",
  clean_data$v32 %in% c(11, 12, 13, 14) ~ "非工作人口",
  clean_data$v32 %in% c(1, 96, 98) ~ "其他",
  TRUE ~ "未知"
)

table(clean_data$target, clean_data$job_group)



# 教育程度 1個未答，但只有九個為第一類
table(clean_data$v33)

# 移除拒答
clean_data <- clean_data %>% filter(v33 != 98)

# 合併教育層級
clean_data$edu_group <- dplyr::case_when(
  clean_data$v33 %in% c(1, 2, 3) ~ "國中以下",
  clean_data$v33 == 4 ~ "高中高職",
  clean_data$v33 %in% c(5, 6) ~ "專科大學",
  clean_data$v33 == 7 ~ "研究所以上",
  TRUE ~ "未知"
)

table(clean_data$target, clean_data$edu_group)



# 這個是教育程度在國小到高中細分
# 我覺得可以跳過
table(clean_data$v34)
#---------------------------------------#
# 刪除該筆欄位
clean_data <- clean_data[!is.na(clean_data$v34), ]
#---------------------------------------#


# 縣市
# 分群
table(clean_data$v35)

# 假設你的資料框叫 df，v35 是編號欄位
# 首先，將 v35 欄位轉為數值（去除前綴 0）
clean_data$v35_num <- as.numeric(clean_data$v35)

# 建立對應邏輯
# 把東部和離島放在一起（樣本太少，加上結果太不平衡）
clean_data$region <- dplyr::case_when(
  clean_data$v35_num %in% c(1, 2, 3, 4, 5, 6, 7) ~ "北部",
  clean_data$v35_num %in% c(8, 9, 10, 11, 12) ~ "中部",
  clean_data$v35_num %in% c(13, 14, 15, 16, 17) ~ "南部",
  clean_data$v35_num %in% c(18, 19, 20, 21, 22) ~ "東部與離島",
  TRUE ~ "未知"
)

table(clean_data$target,clean_data$region)

# 是否為直轄市
clean_data$is_metro <- clean_data$v35_num %in% c(1,2,5,9,15,16)

table(clean_data$target,clean_data$is_metro)

# 性別
table(clean_data$v36)


#########

clean_data <- clean_data %>% select(-v1)

clean_data <- clean_data %>% select(-v8:-v17)
clean_data <- clean_data %>% select(-wei)
clean_data <- clean_data %>% select(-total)
clean_data <- clean_data %>% select(-v35_num)


# 自評沈迷 有 無
table(clean_data$v18)

# 自評沈迷程度1~5
# 填答人數少，所以刪除（續下題）
table(clean_data$v19)
# 續上題，是否尋求幫助，少 所以刪除
table(clean_data$v20)

clean_data <- clean_data %>% select(-v19, -v20)

clean_data <- clean_data %>% select(-v32:-v35)

table(clean_data$v3)
table(clean_data$v4)

# 特指沒有工作和上學的受訪者
table(clean_data$v5)
# 直接合併到v3!
# clean_data <- clean_data %>% select(-v5)

# 我選擇刪掉v4 
# 我覺得造成沈迷他是一個長期而成的狀態，而上班上課日是每個人較長活動的時段
# v4的結果是依照v3結果跳轉的題目，有些受訪者會沒有回答，資料上不好處理
clean_data <- clean_data %>% select(-v4)


##################

table(clean_data$v2)

library(dplyr)

# 步驟 1：重新命名欄位
clean_data <- clean_data %>%
  rename(`疫情後上網狀況` = `近六個月上網時間`)

# 步驟 2：將數值 1, 2, 3, 8 轉成文字標籤
clean_data <- clean_data %>%
  mutate(`近六個月上網時間` = case_when(
    `近六個月上網時間` == 1 ~ "增加",
    `近六個月上網時間` == 2 ~ "減少",
    `近六個月上網時間` == 3 ~ "不變",
    `近六個月上網時間` == 8 ~ "未回答/拒答",
    TRUE ~ NA_character_  # 處理其他未知值
  ))

library(dplyr)

# 步驟 1：重新命名欄位 v3 -> 近六個月上網時間
clean_data <- clean_data %>%
  rename(`近六個月上網時間` = v3)

# 步驟 2：處理 99994 -> v4，99997/99998 -> "未回答/拒答"
clean_data <- clean_data %>%
  mutate(`近六個月上網時間` = case_when(
    `近六個月上網時間` == 99994 ~ as.character(v5),  # 用 v4 的值補上
    `近六個月上網時間` %in% c(99997, 99998) ~ "未回答/拒答",
    TRUE ~ as.character(`近六個月上網時間`)  # 其他保留原值（先當字串處理）
  ))

table(clean_data$`近六個月上網時間`)

clean_data <- clean_data %>% select(-v5)



library(dplyr)

# 先將 v6_1 到 v6_7 包成一個 list
v6_vars <- paste0("v6_", 1:7)

# 建立每個行為的對應欄位（1 表示該列任一欄含有該類型）
clean_data <- clean_data %>%
  mutate(
    追劇 = if_else(apply(across(all_of(v6_vars)), 1, function(x) 1 %in% x), 1, 0),
    看社群類的影片 = if_else(apply(across(all_of(v6_vars)), 1, function(x) 2 %in% x), 1, 0),
    看網路社群 = if_else(apply(across(all_of(v6_vars)), 1, function(x) 3 %in% x), 1, 0),
    玩遊戲 = if_else(apply(across(all_of(v6_vars)), 1, function(x) 4 %in% x), 1, 0),
    使用通訊軟體 = if_else(apply(across(all_of(v6_vars)), 1, function(x) 5 %in% x), 1, 0),
    網路購物或看商品 = if_else(apply(across(all_of(v6_vars)), 1, function(x) 6 %in% x), 1, 0),
    只有工作或學校課業目的 = if_else(apply(across(all_of(v6_vars)), 1, function(x) 95 %in% x), 1, 0),
    其他 = if_else(apply(across(all_of(v6_vars)), 1, function(x) any(x %in% c(96, 97))), 1, 0)
  )


old_names <- c(
  "追劇",
  "看社群類的影片",
  "看網路社群",
  "玩遊戲",
  "使用通訊軟體",
  "網路購物或看商品",
  "只有工作或學校課業目的",
  "其他"
)

# 新欄位名稱加上前綴
new_names <- paste0("疫情後網路活動時間增加的項目_", old_names)

# 批次重新命名欄位
names(clean_data)[names(clean_data) %in% old_names] <- new_names

# 刪除原始的 v6_1 到 v6_7 欄位
clean_data <- clean_data %>% select(-all_of(v6_vars))


library(dplyr)

# v7 的欄位名稱
v7_vars <- paste0("v7_", 1:7)

# 對應網路活動代碼與欄位名稱（含前綴）
activity_map <- list(
  "追劇" = 1,
  "看社群類的影片" = 2,
  "看網路社群" = 3,
  "玩遊戲" = 4,
  "使用通訊軟體" = 5,
  "網路購物或是看網購商品" = 6,
  "上網只有工作或學校課業目的" = 95,
  "其他" = c(96, 97)
)

# 依序建立 dummy 欄位
for (label in names(activity_map)) {
  colname <- paste0("無法節制時間的網路活動項目_", label)
  codes <- activity_map[[label]]
  
  clean_data[[colname]] <- apply(clean_data[v7_vars], 1, function(x) {
    if (any(x %in% codes)) 1 else 0
  })
}

# 刪除原始的 v7_1 到 v7_7 欄位
clean_data <- clean_data %>% select(-all_of(v7_vars))


library(dplyr)

clean_data <- clean_data %>%
  rename(`難以集中精神` = v28) %>%
  mutate(`難以集中精神` = case_when(
    `難以集中精神` == 1 ~ "非常不同意",
    `難以集中精神` == 2 ~ "不同意",
    `難以集中精神` == 3 ~ "普通",
    `難以集中精神` == 4 ~ "同意",
    `難以集中精神` == 5 ~ "非常同意",
    `難以集中精神` == 8 ~ "未回答/拒答",
    TRUE ~ NA_character_
  ))


library(dplyr)

clean_data <- clean_data %>%
  rename(`生活無聊感` = v29) %>%
  mutate(`生活無聊感` = case_when(
    `生活無聊感` == 1 ~ "非常不同意",
    `生活無聊感` == 2 ~ "不同意",
    `生活無聊感` == 3 ~ "普通",
    `生活無聊感` == 4 ~ "同意",
    `生活無聊感` == 5 ~ "非常同意",
    `生活無聊感` == 8 ~ "未回答/拒答",
    TRUE ~ NA_character_
  ))

library(dplyr)

clean_data <- clean_data %>%
  rename(`課業或工作壓力` = v30) %>%
  mutate(`課業或工作壓力` = case_when(
    `課業或工作壓力` == 1 ~ "非常不同意",
    `課業或工作壓力` == 2 ~ "不同意",
    `課業或工作壓力` == 3 ~ "普通",
    `課業或工作壓力` == 4 ~ "同意",
    `課業或工作壓力` == 5 ~ "非常同意",
    `課業或工作壓力` == 8 ~ "未回答/拒答",
    TRUE ~ NA_character_
  ))


library(dplyr)

clean_data <- clean_data %>%
  rename(`年齡` = v31) %>%
  mutate(`年齡` = case_when(
    年齡 == 1 ~ "12-17歲",
    年齡 == 2 ~ "18-19歲",
    年齡 == 3 ~ "20-29歲",
    年齡 == 4 ~ "30-39歲",
    年齡 == 5 ~ "40-49歲",
    年齡 == 6 ~ "50-59歲",
    年齡 == 7 ~ "60-64歲",
    年齡 == 8 ~ "65歲以上",
    年齡 == 98 ~ "未回答/拒答",
    TRUE ~ NA_character_
  ))

library(dplyr)

clean_data <- clean_data %>%
  rename(`性別` = v36) %>%
  mutate(`性別` = case_when(
    性別 == 1 ~ "男",
    性別 == 2 ~ "女",
    TRUE ~ NA_character_  # 其他值處理為 NA
  ))

clean_data <- clean_data %>%
  rename(`職業` = job_group)

clean_data <- clean_data %>%
  rename(`教育程度` = edu_group)

clean_data <- clean_data %>%
  rename(`居住地區` = region)

clean_data <- clean_data %>%
  rename(`是否居住直轄市` = is_metro) %>%
  mutate(`是否居住直轄市` = if_else(`是否居住直轄市`, 1, 0))


###

library(dplyr)

clean_data <- clean_data %>%
  rename(`自評是否網路沉迷` = v18) %>%
  mutate(`自評是否網路沉迷` = if_else(`自評是否網路沉迷` == 1, 1, 0))


library(dplyr)

clean_data <- clean_data %>%
  rename(`擔心確診情況` = v21) %>%
  mutate(`擔心確診情況` = case_when(
    `擔心確診情況` == 1 ~ "從來沒有",
    `擔心確診情況` == 2 ~ "很少",
    `擔心確診情況` == 3 ~ "有時",
    `擔心確診情況` == 4 ~ "經常",
    `擔心確診情況` == 5 ~ "幾乎總是",
    `擔心確診情況` == 8 ~ "未回答/拒答",
    TRUE ~ NA_character_
  ))

library(dplyr)

clean_data <- clean_data %>%
  rename(`擔心居家隔離檢疫，失去他人陪伴情況` = v22) %>%
  mutate(`擔心居家隔離檢疫，失去他人陪伴情況` = case_when(
    `擔心居家隔離檢疫，失去他人陪伴情況` == 1 ~ "從來沒有",
    `擔心居家隔離檢疫，失去他人陪伴情況` == 2 ~ "很少",
    `擔心居家隔離檢疫，失去他人陪伴情況` == 3 ~ "有時",
    `擔心居家隔離檢疫，失去他人陪伴情況` == 4 ~ "經常",
    `擔心居家隔離檢疫，失去他人陪伴情況` == 5 ~ "幾乎總是",
    `擔心居家隔離檢疫，失去他人陪伴情況` == 8 ~ "未回答/拒答",
    TRUE ~ NA_character_
  ))

library(dplyr)

clean_data <- clean_data %>%
  rename(`擔心外出確診，減少外出情況` = v23) %>%
  mutate(`擔心外出確診，減少外出情況` = case_when(
    `擔心外出確診，減少外出情況` == 1 ~ "從來沒有",
    `擔心外出確診，減少外出情況` == 2 ~ "很少",
    `擔心外出確診，減少外出情況` == 3 ~ "有時",
    `擔心外出確診，減少外出情況` == 4 ~ "經常",
    `擔心外出確診，減少外出情況` == 5 ~ "幾乎總是",
    `擔心外出確診，減少外出情況` == 8 ~ "未回答/拒答",
    TRUE ~ NA_character_
  ))

library(dplyr)

clean_data <- clean_data %>%
  rename(`因疫情失去工作或工作減量的情況` = v24) %>%
  mutate(`因疫情失去工作或工作減量的情況` = case_when(
    `因疫情失去工作或工作減量的情況` == 1 ~ "從來沒有",
    `因疫情失去工作或工作減量的情況` == 2 ~ "很少",
    `因疫情失去工作或工作減量的情況` == 3 ~ "有時",
    `因疫情失去工作或工作減量的情況` == 4 ~ "經常",
    `因疫情失去工作或工作減量的情況` == 5 ~ "幾乎總是",
    `因疫情失去工作或工作減量的情況` == 8 ~ "未回答/拒答",
    TRUE ~ NA_character_
  ))

library(dplyr)

clean_data <- clean_data %>%
  rename(`疫情是否有居家上班或上課的情況` = v25) %>%
  mutate(`疫情是否有居家上班或上課的情況` = case_when(
    `疫情是否有居家上班或上課的情況` == 1 ~ "從來沒有",
    `疫情是否有居家上班或上課的情況` == 2 ~ "很少",
    `疫情是否有居家上班或上課的情況` == 3 ~ "有時",
    `疫情是否有居家上班或上課的情況` == 4 ~ "經常",
    `疫情是否有居家上班或上課的情況` == 5 ~ "幾乎總是",
    `疫情是否有居家上班或上課的情況` == 8 ~ "未回答/拒答",
    TRUE ~ NA_character_
  ))

library(dplyr)

clean_data <- clean_data %>%
  rename(`是否有染疫的經歷` = v26) %>%
  mutate(`是否有染疫的經歷` = case_when(
    `是否有染疫的經歷` == 1 ~ "沒有",
    `是否有染疫的經歷` == 2 ~ "有，無症狀",
    `是否有染疫的經歷` == 3 ~ "有，輕度症狀",
    `是否有染疫的經歷` == 4 ~ "有，中度症狀",
    `是否有染疫的經歷` == 5 ~ "有，重度症狀",
    `是否有染疫的經歷` == 8 ~ "未回答/拒答",
    TRUE ~ NA_character_
  ))

library(dplyr)

clean_data <- clean_data %>%
  rename(`有居家隔離或檢疫經驗` = v27) %>%
  mutate(`有居家隔離或檢疫經驗` = case_when(
    `有居家隔離或檢疫經驗` == 1 ~ "沒有",
    `有居家隔離或檢疫經驗` == 2 ~ "7 天內",
    `有居家隔離或檢疫經驗` == 3 ~ "8-14 天",
    `有居家隔離或檢疫經驗` == 4 ~ "15-21 天",
    `有居家隔離或檢疫經驗` == 5 ~ "超過21 天",
    `有居家隔離或檢疫經驗` == 8 ~ "未回答/拒答",
    TRUE ~ NA_character_
  ))


write.csv(clean_data, "data0606.csv", row.names = FALSE)

df <- read.csv("data0606.csv", header = TRUE, sep = ",")

# 查看每個欄位的所有獨一無二的類別
lapply(df, unique)

table(df$target)
