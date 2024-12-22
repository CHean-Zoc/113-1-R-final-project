library(tidyverse)

# 1. 讀取 CSV 檔案
data_109 <- read_csv("/cloud/project/1131222_FIPJ/109年11月行政區電信信令人口統計資料_鄉鎮市區.csv")
data_112 <- read_csv("/cloud/project/1131222_FIPJ/112年11月行政區電信信令人口統計資料_鄉鎮市區.csv")

# 2. 定義清理數值欄位的函數
clean_numeric <- function(column) {
  column |> 
    as.character() |> 
    str_remove_all("[^0-9.]") |> 
    as.numeric()
}

# 3. 處理 109 年資料
data_109 <- data_109 |>
  mutate(
    id = row_number(),                       # 新增虛擬 ID
    `109NIGHT_WORK` = clean_numeric(NIGHT_WORK)  # 清理 NIGHT_WORK 數據
  )

# 4. 處理 112 年資料
data_112 <- data_112 |>
  mutate(
    id = row_number(),                       # 新增虛擬 ID
    `112NIGHT_WORK` = clean_numeric(NIGHT_WORK)  # 清理 NIGHT_WORK 數據
  )

# 5. 考量手機信令資料取樣需透過附近基地台3角定位方式，因離島基礎設施相較本島不健全，故排除離島資料(金門縣、連江縣及澎湖縣數據)
excluded_counties <- c("金門縣", "連江縣", "澎湖縣")

data_109 <- data_109 |>
  filter(!COUNTY %in% excluded_counties)

data_112 <- data_112 |>
  filter(!COUNTY %in% excluded_counties)

# 6. 合併數據，保留 `COUNTY` 和 `TOWN`，並計算 highlight、difference、ROC(%)
comparison_data <- data_109 |>
  select(id, COUNTY, TOWN, `109NIGHT_WORK`) |>
  inner_join(
    data_112 |>
      select(id, `112NIGHT_WORK`),
    by = "id"
  ) |>
  mutate(
    highlight  = if_else(`109NIGHT_WORK` > `112NIGHT_WORK`, "109", "112"),  # 哪一年數據較大
    difference = abs(`109NIGHT_WORK` - `112NIGHT_WORK`),                   # 絕對差異
    `ROC(%)`   = ((`112NIGHT_WORK` - `109NIGHT_WORK`) / `109NIGHT_WORK`) * 100  # 變化率 (%)
  )

# 7. 查看結果
glimpse(comparison_data)

# 8. 找出變化率正成長最多 (ROC(%) 最大) 與負成長最多 (ROC(%) 最小) 的那一筆
top_positive <- comparison_data |>
  arrange(desc(`ROC(%)`)) |>
  head(1)

top_negative <- comparison_data |>
  arrange(`ROC(%)`) |>
  head(1)

# 9. 查看結果
cat("=== 變化率正成長最多（最大 ROC(%)） ===\n")
print(top_positive)

cat("\n=== 變化率負成長最多（最小 ROC(%)） ===\n")
print(top_negative)

