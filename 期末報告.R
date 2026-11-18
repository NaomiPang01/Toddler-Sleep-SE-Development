library(dplyr) ; library(readxl) ; library(lavaan)
dt <- read.csv("dt.csv", fileEncoding = "Big5")
names(dt)[names(dt) == "退縮行."] <- "退縮行為"
names(dt)[names(dt) == "攻擊行."] <- "攻擊行為"
dt$睡眠規律性 <- as.factor(dt$睡眠規律性)
dt$性別 <- as.factor(dt$性別)
dt <- dt[, c(
  # 基本資料
  "baby_sex", "int_months",
  
  # 睡眠變數
  "sleep01", "sleep01a", "sleep01d",
  "slequa1", "slequa2",
  
  # 家庭教育與教養背景
  paste0("famedu", sprintf("%02d", 1:9)),
  paste0("fatherinvo", sprintf("%02d", 1:5)),
  paste0("motherinvo", sprintf("%02d", 1:5)),
  paste0("famc", sprintf("%02d", 1:20)),
  paste0("famevic", sprintf("%02d", 1:7)),
  
  # 行爲情緒發展
  paste0("socb", sprintf("%02d", c(1:30))),
  paste0("socc", sprintf("%02d", c(1:22)))
)]

dt <- dt %>% filter(sleep01 != 8888)
dt <- dt %>%
  mutate(across(everything(), ~replace(., . %in% c(7777, 8888, 9996, 9999), NA)))

dt$baby_sex <- factor(dt$baby_sex, levels = c(1,2), labels = c("Boy", "Girl"))
dt$sleep01  <- factor(dt$sleep01, levels = c(1,2), labels = c("Irregular", "Regular"))
dt <- dt %>%
  mutate(sleep_hours = coalesce(sleep01a, sleep01d))

reverse_code <- function(data, vars, max_value) {
  data[vars] <- lapply(data[vars], function(x)
    ifelse(!is.na(x), (max_value + 1) - as.numeric(x), NA))
  return(data)
}

# 用法：
dt <- reverse_code(dt, c("famc17", "famc18", "famc19", "slequa1"), 4)
dt <- reverse_code(dt, c("famedu01", "famedu02", "famedu03", "famedu06", 
                         "famedu07", "famedu08", "famedu09"), 4)
dt <- reverse_code(dt, c(
  # Negative emotionality
  "socb04", "socb05", "socc01", "socc06",
  # Withdrawal
  "socb24", "socb25", "socb26",
  # Aggression
  "socb27", "socb28", "socb29", "socb30",
  # Shyness & Shame
  "socc07", "socc08"
), 5)

dt$夜醒頻率 <- factor(dt$夜醒頻率,
                     levels = 1:4,
                     labels = c("Frequently", "Sometimes", "Rarely", "No"),
                     ordered = TRUE)
dt$主觀睡眠品質 <- factor(dt$主觀睡眠品質,
                     levels = 1:4,
                     labels = c("Terrible", "Not great", "Acceptable", "Very good"),
                     ordered = TRUE)

famedu_vars <- paste0("famedu", sprintf("%02d", 1:9))
dt <- dt %>%
  mutate(across(
    all_of(famedu_vars),
    ~factor(.,
            levels = 1:4,
            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"),
            ordered = TRUE)
  ))

invo_vars <- c(paste0("fatherinvo", sprintf("%02d", 1:5)),
               paste0("motherinvo", sprintf("%02d", 1:5)))
dt <- dt %>%
  mutate(across(
    all_of(invo_vars),
    ~factor(.,
            levels = 1:4,
            labels = c("Rarely", "Sometimes", "Often", "Very often"),
            ordered = TRUE)
  ))

famc_vars <- c(paste0("famc", sprintf("%02d", 1:20)), 
               paste0("famevic", sprintf("%02d", 1:7)))
dt <- dt %>%
  mutate(across(
    all_of(famc_vars),
    ~factor(.,
            levels = 1:4,
            labels = c("Rarely", "Sometimes", "Often", "Very often"),
            ordered = TRUE)
  ))

soc_vars <- c(paste0("socb", sprintf("%02d", c(1:30))),
              paste0("socc", sprintf("%02d", c(1:22))))
dt <- dt %>%
  mutate(across(
    all_of(soc_vars),
    ~factor(.,
            levels = 1:5,
            labels = c("Never", "Rarely", "Sometimes", "Often", "Always"),
            ordered = TRUE)
  ))

dt <- dt[,c(1:3,106,6:105)]
summary(dt)
set.seed(5014)
library(psych)
domains <- list(
  # ====== 家庭教育：Chinese Parenting ======
  Parenting_Shame = c("famedu08","famedu09"),
  
  # ====== 家庭教育：Parental Involvement ======
  Mother_Involvement = c("motherinvo01","motherinvo02","motherinvo03",
                         "motherinvo04","motherinvo05"),
  
  # ====== 家庭學習環境：Learning Environment ======
  HOME_Parental_Responsiveness = c("famc14","famc15","famc16"),

  # ====== 社會情緒發展（Social–Emotional Development） ======
  Temperament_EffortfulControl = c("socb02","socb03","socc16","socc17"),
  Temperament_NegativeEmotionality = c("socb04","socb05","socc01","socc06"),
  
  Emotional_Expression = c("socb07","socb08","socb09"),
  Emotional_Understanding = c("socb10","socb11","socb12","socc13","socc18"),
  Emotional_Regulation = c("socb13","socb14","socb15","socb16"),
  
  Social_Independence = c("socb17", "socb18"),
  Social_Assertiveness = c("socb19", "socc14", "socc19", "socc20"),
  Social_Sociability = c("socb20", "socb21", "socc15", "socc21"),
  Social_Compliance = c("socb22", "socb23", "socc22"),
  
  Withdrawal = c("socb24","socb25","socb26"),
  Aggression = c("socb27","socb28","socb29","socb30"),
  Shyness = c("socc07"),
  Shame = c("socc08")
)

dt1 <- NULL
dt1 <- dt[, c(1:6,14,15,21:25,39:41,54:57,59:83,88:90,95:104)] %>% mutate(across(everything(), as.numeric))
dt1 <- na.omit(dt1)

dt2 <- dt1[,c(7:58)]

compute_alpha <- function(df, vars) {
  # 只保留存在的變數
  vars_exist <- vars[vars %in% names(df)]
  if (length(vars_exist) < 2) return(NA)  # 少於 2 個有效變數就回 NA
  
  # 選變數並轉 numeric
  sub_df <- df[vars_exist]
  
  # 移除全常數或全 NA 的變數
  valid_vars <- sub_df[, sapply(sub_df, function(x) !all(is.na(x)) & var(x, na.rm = TRUE) != 0), drop = FALSE]
  
  if (ncol(valid_vars) < 2) return(NA)  # 少於 2 個題目仍回 NA
  
  # 計算 alpha
  alpha_res <- psych::alpha(valid_vars, warnings = FALSE)
  return(alpha_res$total$raw_alpha)
}

alpha_results <- lapply(domains, function(vars) compute_alpha(dt2, vars))

# 整理成 data.frame
alpha_df <- data.frame(
  Cronbach_Alpha = unlist(alpha_results)
)
alpha_df

domain_means <- lapply(domains, function(vars) {
  # 只選 df 中存在的變數
  vars_exist <- vars[vars %in% names(dt2)]
  
  # 轉成 numeric
  dt_num <- dt2[vars_exist]
  
  # 計算每列平均值
  rowMeans(dt_num, na.rm = TRUE)
})

domains <- as.data.frame(domain_means)

# step 3: 將 domain 分數合併回原本 dt
dt3 <- cbind(dt1[,c(1:6)], domains)
dt3$baby_sex <- factor(dt3$baby_sex, levels = c(1,2), labels = c("Boy", "Girl"))
dt3$sleep01  <- factor(dt3$sleep01, levels = c(1,2), labels = c("Irregular", "Regular"))
summary(dt3)
names(dt3)

library(table1)

# 自訂顯示p值的render function
render.pvalue <- function(x, ...) {
  p <- NA
  if (is.numeric(x)) {
    p <- t.test(x ~ group, data = parent.frame())$p.value
  } else if (is.factor(x)) {
    tbl <- table(x, parent.frame()$group)
    if (all(dim(tbl) > 1)) p <- chisq.test(tbl)$p.value
  }
  c("", sprintf("p = %.3f", p))
}

# 使用 table1 產生表格（帶p值）
table1(~ baby_sex + int_months + Parenting_Shame + Mother_Involvement + 
         HOME_Parental_Responsiveness + sleep_hours + slequa1 + slequa2 + 
         Temperament_EffortfulControl + Temperament_NegativeEmotionality + 
         Emotional_Expression + Emotional_Understanding + Emotional_Regulation + 
         Social_Independence + Social_Assertiveness + Social_Sociability + Social_Compliance +
         Withdrawal + Aggression + Shyness + Shame + sleep01, data = dt)

vars <- c("baby_sex", "int_months", "sleep01", "sleep_hours", "slequa1", "slequa2", 
          "Parenting_Shame", "Mother_Involvement", "HOME_Parental_Responsiveness", 
          "Temperament_EffortfulControl", "Temperament_NegativeEmotionality", 
          "Emotional_Expression", "Emotional_Understanding", "Emotional_Regulation", 
          "Social_Independence", "Social_Assertiveness", "Social_Sociability", 
          "Social_Compliance", "Withdrawal", "Aggression", "Shyness", "Shame" )

for (v in vars) {
  cat("-----", v, "-----\n")
  x <- dt[[v]]
  
  if (is.numeric(x)) {
    p <- t.test(x ~ dt$睡眠規律性)$p.value
    cat("t-test p-value =", round(p, 3), "\n")
  } else if (is.factor(x)) {
    tbl <- table(x, dt$睡眠規律性)
    if (all(dim(tbl) > 1)) {
      p <- chisq.test(tbl)$p.value
      cat("Chi-square p-value =", round(p, 3), "\n")
    } else {
      cat("⚠️ 無法進行卡方檢定（某組缺資料）\n")
    }
  }
}

library(ggplot2)
library(patchwork) # 用於將多個圖表並排顯示

# --- 1. 主觀睡眠品質 (slequa1) ---
p1 <- ggplot(dt2, aes(x = sleep01, y = slequa1, fill = sleep01)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(title = "主觀睡眠品質",
       x = "睡眠規律性分組",
       y = "得分 (1=糟糕, 4=非常好)",
       fill = "分組") +
  theme_minimal() +
  scale_fill_manual(values = c("不規律組" = "#FF9999", "規律組" = "#99CCFF"))

# --- 2. 總體睡眠時數 (sleep_hours) ---
p2 <- ggplot(dt2, aes(x = sleep01, y = sleep_hours, fill = sleep01)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(title = "總體睡眠時數",
       x = "睡眠規律性分組",
       y = "時數 (小時)",
       fill = "分組") +
  theme_minimal() +
  scale_fill_manual(values = c("不規律組" = "#FF9999", "規律組" = "#99CCFF"))

# --- 3. 奮力控制氣質 (Temperament_EffortfulControl) ---
p3 <- ggplot(dt2, aes(x = sleep01, y = Temperament_EffortfulControl, fill = sleep01)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(title = "奮力控制氣質",
       x = "睡眠規律性分組",
       y = "得分 (1-5)",
       fill = "分組") +
  theme_minimal() +
  scale_fill_manual(values = c("不規律組" = "#FF9999", "規律組" = "#99CCFF"))

# --- 4. 恥感 (Shame) ---
p4 <- ggplot(dt2, aes(x = sleep01, y = Shame, fill = sleep01)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(title = "恥感 (Shame)",
       x = "睡眠規律性分組",
       y = "得分 (1-5)",
       fill = "分組") +
  theme_minimal() +
  scale_fill_manual(values = c("不規律組" = "#FF9999", "規律組" = "#99CCFF"))

# --- 組合圖表 (可選，讓排版更美觀) ---
(p1 + p2) / (p3 + p4)

library(corrplot)

# 選擇核心連續/順序變數 (排除名義變數如性別)
# 確保所有變數都已轉為數值型
core_vars <- dt2[, c("sleep_hours", "slequa1", "slequa2", 
                     "Parenting_Shame", "Mother_Involvement", 
                     "HOME_Parental_Responsiveness", "Temperament_EffortfulControl", 
                     "Temperament_NegativeEmotionality", "Emotional_Expression", 
                     "Emotional_Understanding", "Emotional_Regulation", 
                     "Social_Independence", "Social_Assertiveness", 
                     "Social_Sociability", "Social_Compliance", 
                     "Withdrawal", "Aggression", "Shyness", "Shame")]

# 計算相關矩陣 (使用 Spearman 或 Pearson，取決於變數性質，此處用 Pearson)
M <- cor(dt[,4:22], use = "pairwise.complete.obs") 

# 繪製相關係數圖 (使用 'color' 顯示強度和方向，並將不顯著的係數空白化)
corrplot(M, 
         method = "color",       # 顏色和圓圈大小表示相關程度
         type = "upper",         # 只顯示上三角
         order = "hclust",       # 根據相關性聚類排序
         tl.col = "black",       # 變數名稱顏色
         tl.srt = 45,            # 變數名稱傾斜角度 (建議 45 或 90)
         tl.cex = 0.6,           # <--- **新增/修改此行：控制變數名稱字體大小**
         addCoef.col = "black",  # 在圖上顯示係數值
         number.cex = 0.6,       # 係數大小
         diag = FALSE)           # 不顯示對角線

# 1. 繪製 Boxplot，但設定 xaxt = "n" 以隱藏預設的 X 軸標籤
boxplot(dt[, 5:22], main = "各變項數據分佈概覽", col = "lightblue")

# 2. 獲取變數名稱
labs <- c("夜醒頻率", "主觀睡眠品質", "教養羞愧感", "母親親職參與度", "父母回應性", 
          "奮力控制氣質", "負向情緒性氣質", "情緒表達", "情緒理解", "情緒調節", 
          "獨立性", "主動性", "合群性", "順從性", "退縮行爲", "攻擊行爲", "害羞", "恥感")

# 3. 使用 text() 函數手動添加旋轉標籤
# x = 1:length(labs) 是標籤的橫座標
# y = par("usr")[3] - 0.5 是將標籤放在圖表底端稍微下方（數值可依需求調整）
# srt = 45 設定傾斜角度
# adj = 1 設定標籤對齊方式
# xpd = TRUE 確保標籤超出繪圖區時仍能顯示
text(x = 1:length(labs), 
     y = par("usr")[3] - 0.2, 
     labels = labs, 
     srt = 45, 
     adj = 1, 
     xpd = TRUE, 
     cex = 0.8)


dt[,-c(1,3)] <- scale(dt[,-c(1,3)])
summary(dt)
# 1. 定義英文變數名稱（嚴格對應您的 22 個變數順序）
en_names <- c(
  "Gender",          # [1] 性別
  "Age_Months",      # [2] 月齡
  "Sleep_Reg",       # [3] 睡眠規律性
  "Sleep_Hrs",       # [4] 總體睡眠時數
  "Sleep_Wake",      # [5] 夜醒頻率
  "Sleep_Qual",      # [6] 主觀睡眠品質
  "Parent_Shame",    # [7] 教養羞愧感
  "Mom_Involve",     # [8] 母親親職參與度
  "Parent_Resp",     # [9] 父母回應性
  "Effort_Ctrl",     # [10] 奮力控制氣質
  "Neg_Emo",         # [11] 負向情緒性氣質
  "Emo_Exp",         # [12] 情緒表達
  "Emo_Und",         # [13] 情緒理解
  "Emo_Reg",         # [14] 情緒調節
  "Indep",           # [15] 獨立性
  "Init",            # [16] 主動性
  "Social",          # [17] 合群性
  "Comply",          # [18] 順從性
  "Withdraw",        # [19] 退縮行為
  "Aggress",         # [20] 攻擊行為
  "Shy",             # [21] 害羞
  "Shame_Soc"        # [22] 恥感
)

# 2. 正式修改 dt_scaled 的欄位名稱
names(dt) <- en_names

# 3. 檢查更名是否成功
print(names(dt))

sem_model <- '
  # 1. 測量模型 (Latent Variables)
  Sleep_Health =~ Sleep_Hrs + Sleep_Wake + Sleep_Qual
  Parenting =~ Parent_Shame + Mom_Involve + Parent_Resp
  Soc_Emo =~ Effort_Ctrl + Neg_Emo + Emo_Exp + Emo_Und + Emo_Reg + 
             Indep + Init + Social + Comply + Withdraw + Aggress + 
             Shy + Shame_Soc

  # 2. 結構路徑 (Regression Paths)
  Soc_Emo ~ Sleep_Health + Sleep_Reg + Gender + Age_Months + Parenting
  
  # 3. 殘差相關 (根據 MI 挑選最具理論意義的)
  Withdraw ~~ Shy
  Neg_Emo ~~ Shame_Soc
  Neg_Emo ~~ Withdraw
  Emo_Exp ~~ Emo_Und
  Emo_Exp ~~ Init
  Withdraw ~~ Aggress
'

# 執行模型
fit <- sem(sem_model, data = dt)
fit
# 查看結果
summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

library(semPlot)

semPaths(fit, 
         whatLabels = "std",        # 顯示標準化係數
         layout = "tree2",           # 樹狀佈局
         edge.label.cex = 0.8,      # 字體大小
         curvePivot = TRUE,         # 線條平滑
         fade = FALSE)              # 顏色深淺不隨顯著性改變

windowsFonts(JP = windowsFont("Microsoft JhengHei"))
par(family = "Microsoft JhengHei")
labels <- c("總體睡眠時數", "半夜哭鬧頻率", "主觀睡眠品質", 
            "教養羞愧感", "母親親職參與度", "父母回應性",
            "奮力控制氣質", "負向情緒氣質", 
            "情緒表達", "情緒理解", "情緒調節",
            "獨立性", "主動性", "合群性", "順從性", 
            "退縮行爲", "攻擊行爲", "害羞", "恥感",
            "睡眠規律性", "性別", "月齡",
            "睡眠健康", "家庭教育與教養", "社會情緒")

semPaths(fit, 
         whatLabels = "std", 
         layout = "spring",
         rotation = 2,            # 旋轉方向，讓自變項在左，依變項在右
         label.cex = 1,      # 數值越大文字越大 (預設通常是 1.0)
         label.scale = FALSE,  # 設為 FALSE 可以讓文字大小不隨方框縮放
         edge.label.cex = 1, # 同時放大線條上的係數數字
         sizeMan = 10,         # 放大觀測變項 (長方形) 的寬度
         sizeMan2 = 5,         # 調整長方形的高度
         sizeLat = 10,         # 放大潛在變項 (圓形) 的大小
         nodeLabels = labels, 
         font = 1,                # 設定字型樣式
         family = "JP",           # 使用剛才定義的微軟正黑體
         color = "lightblue",
         edge.color = "black",
         style = "lisrel",
         mar = c(3, 10, 3, 10))
# 1. 確保字型設定正確 (若還是有警告，請嘗試將 family 設為 "sans")
windowsFonts(JP = windowsFont("Microsoft JhengHei"))

# 2. 重新繪圖
semPaths(fit, 
         whatLabels = "std", 
         style = "lisrel", 
         rotation = 2, 
         nodeLabels = labels,
         nCharNodes = 0, 
         # --- 解決遮擋的核心參數 ---
         edge.label.position = 0.25, 
         # --- 文字與方框大小優化 ---
         label.cex = 0.75,           # 放大框框內的文字
         label.scale = FALSE,
         edge.label.cex = 0.9,      # 放大線上的路徑係數數值
         sizeMan = 7,              # 增加方框寬度，避免文字擠壓
         sizeMan2 = 1.5,              # 縮減方框高度，騰出更多垂直空間給線條
         sizeLat = 7,              # 橢圓大小
         # --- 視覺優化 ---
         color = "lightblue",
         edge.color = "black",
         mar = c(3, 12, 3, 12))
