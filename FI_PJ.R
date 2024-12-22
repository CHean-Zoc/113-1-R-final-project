#加載指定套件
library(tidyverse)

#用glimpse的套件
library(dplyr)
# 要讀取CSV的套件
library(readr)

# 讀取 CSV 檔案
data_109 <- read_csv("/cloud/project/1131222_FIPJ/109年11月行政區電信信令人口統計資料_鄉鎮市區.csv")
data_112 <- read_csv("/cloud/project/1131222_FIPJ/112年11月行政區電信信令人口統計資料_鄉鎮市區.csv")

glimpse(data_109)
glimpse(data_112)


# 為兩個資料框添加虛擬 ID 並確保轉換成可計算的數值型
data_109 <- data_109 %>%
  mutate(
    ID = row_number(),
    NIGHT_WORK = as.numeric(gsub("[^0-9.]", "", NIGHT_WORK)) # 清理並轉為數值
  )

data_112 <- data_112 %>%
  mutate(
    ID = row_number(),
    NIGHT_WORK = as.numeric(gsub("[^0-9.]", "", NIGHT_WORK)) # 清理並轉為數值
  )

# 合併數據並計算差異
comparison_data <- data_109 %>%
  rename(NIGHT_WORK_109 = NIGHT_WORK) %>%
  inner_join(
    data_112 %>% rename(NIGHT_WORK_112 = NIGHT_WORK),
    by = "ID"
  ) %>%
  mutate(
    Highlight = ifelse(NIGHT_WORK_109 > NIGHT_WORK_112, "109", "112"),
    Difference = abs(NIGHT_WORK_109 - NIGHT_WORK_112)
  )

# 查看結果
glimpse(comparison_data)


library(tidyverse)

# 定義需要刪除的欄位名稱（加上後綴）
columns_to_remove <- c(
  "MORNING_WORK.x", "MIDDAY_WORK.x", "AFTERNOON_WORK.x", "EVENING_WORK.x",
  "MORNING_WEEKEND.x", "MIDDAY_WEEKEND.x", "AFTERNOON_WEEKEND.x", "EVENING_WEEKEND.x",
  "MORNING_WORK.y", "MIDDAY_WORK.y", "AFTERNOON_WORK.y", "EVENING_WORK.y",
  "MORNING_WEEKEND.y", "MIDDAY_WEEKEND.y", "AFTERNOON_WEEKEND.y", "EVENING_WEEKEND.y"
)

# 刪除這些欄位
comparison_data <- comparison_data %>%
  select(-all_of(columns_to_remove))

# 查看刪除後的結果
glimpse(comparison_data)








