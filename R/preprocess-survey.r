library(here)
library(tidyverse)

# * read raw data
path <- "D:/JMDP2018サーベイ"
raw <- read_csv(
  here(path, "donordata.csv"),
  locale = locale(encoding = "cp932"),
  na = c("", "NA")
)

# * cleaning
data <- raw %>%
  mutate(
    #gender
    q1 = if_else(q1 == ".", NA_character_, q1),
    q1 = as.numeric(q1),
    female = q1 - 1,

    #age
    q2 = if_else(str_detect(q2, "[.,]"), NA_character_, q2),
    q2 = as.numeric(q2),
    age20 = if_else(q2 < 3, 1, 0),
    age30 = if_else(2 < q2 & q2 <= 4, 1, 0),
    age40 = if_else(q2 == 5, 1, 0),

    #prefecture when donating
    move = if_else(str_detect(q8, "提供|当時"), 1, 0),
    q8 = case_when(
      str_detect(q8, "ドナー提供時は愛知県") ~ "愛知県",
      str_detect(q8, "提供時、京都府") ~ "京都府",
      str_detect(q8, "当時は山口") ~ "山口県",
      str_detect(q8, "提供当時は愛知在住") ~ "愛知県",
      str_detect(q8, "当時は兵庫県") ~ "兵庫県",
      str_detect(q8, "奈良県.") ~ "奈良県",
      TRUE ~ q8
    ),
    q8 = if_else(str_detect(q8, "、"), ".", q8),
    q8 = str_remove(q8, "都|府|県"),
    pref = if_else(str_detect(q8, "[.]"), NA_character_, q8),

    #working
    emp_self = if_else(str_detect(q10, "1"), 1, 0),
    emp_priv = if_else(str_detect(q10, "2"), 1, 0),
    emp_pub = if_else(str_detect(q10, "3"), 1, 0),
    emp_prof = if_else(str_detect(q10, "4"), 1, 0),
    emp_part = if_else(str_detect(q10, "5"), 1, 0),
    not_work = if_else(str_detect(q10, "6|7|8"), 1, 0),
    emp_other = if_else(str_detect(q10, "9"), 1, 0),

    #job
    job_med = if_else(str_detect(q11, "1"), 1, 0),
    job_fire = if_else(str_detect(q11, "2"), 1, 0),
    job_npo = if_else(str_detect(q11, "3"), 1, 0),
    job_sdf = if_else(str_detect(q11, "4"), 1, 0),
    job_care = if_else(str_detect(q11, "5"), 1, 0),
    job_other = if_else(str_detect(q11, "6"), 1, 0),

    #marry
    q13 = if_else(str_detect(q13, "[.]"), NA_character_, q13),
    q13 = as.numeric(q13),
    married = if_else(q13 == 1, 1, 0),

    #child
    q14 = if_else(str_detect(q14, "[.]"), NA_character_, q14),
    q14 = as.numeric(q14),
    with_child = if_else(q14 == 1, 0, 1),

    #have those who need a care
    q15 = if_else(str_detect(q15, "[.,]"), NA_character_, q15),
    q15 = as.numeric(q15),
    with_care = 2 - q15,

    #provide or not
    q5 = if_else(str_detect(q5, "[.,]"), NA_character_, q5),
    q5 = as.numeric(q5),
    q5_1 = if_else(str_detect(q5_1, "[.,]"), NA_character_, q5_1),
    q5_1 = as.numeric(q5_1),
    donate = if_else(q5 == 1, 1, 0),
    donate_BM = if_else(donate == 1 & q5_1 == 1, 1, 0),
    donate_PB = if_else(donate == 1 & q5_1 == 2, 1, 0),
    donate_unknown = if_else(donate == 1 & is.na(q5_1), 1, 0),
    ongoing = if_else(q5 == 3, 1, 0),

    #stop before CT
    q6_num = if_else(str_detect(`q6'`, "[.]"), NA_character_, `q6'`),
    q6_num = as.numeric(q6_num),
    stop_CT = case_when(
      donate == 0 & q6_num < 4 ~ 1,
      donate == 0 & 4 <= q6_num ~ 0,
      donate == 1 ~ 0
    ),

    #distance to the nearest medical institution
    q9 = if_else(str_detect(q9, "[.,]"), NA_character_, q9),
    q9 = as.numeric(q9),
    inst_less_1hour = if_else(q9 < 3, 1, 0),
    inst_less_2hour = if_else(3 <= q9 & q9 < 5, 1, 0),
    inst_more_2hour = if_else(q9 == 5, 1, 0)
  )

# * Time discount factor
first_2_position <- function(x) {
  if (is.na(x)) {
    return(NA_real_)
  }
  if (str_detect(x, "2")) {
    str_locate(x, "2")[1, 1]
  } else {
    10
  }
}

today_df <- c(
  3005 / 3014,
  3003 / 3297,
  3008 / 3037,
  3000 / 3000,
  3005 / 5951,
  3009 / 3068,
  3001 / 3119,
  3002 / 2996,
  3008 / 3011
)

order_today_df <- order(today_df, decreasing = TRUE)
order_q54 <- paste0("q54_", order_today_df)

day90_df <- c(
  3000 / 3018,
  3006 / 3000,
  3000 / 3009,
  3007 / 3301,
  3006 / 3035,
  3002 / 3005,
  3007 / 5955,
  3001 / 3001,
  3007 / 3066
)

order_day90_df <- order(day90_df, decreasing = TRUE)
order_q55 <- paste0("q55_", order_day90_df)

data2 <- data %>%
  mutate(
    #sequence of response
    seq_q54 = paste0(
      get(order_q54[1]), get(order_q54[2]), get(order_q54[3]),
      get(order_q54[4]), get(order_q54[5]), get(order_q54[6]),
      get(order_q54[7]), get(order_q54[8]), get(order_q54[9])
    ),
    seq_q54 = if_else(str_detect(seq_q54, "[.,]"), NA_character_, seq_q54),

    seq_q55 = paste0(
      get(order_q55[1]), get(order_q55[2]), get(order_q55[3]),
      get(order_q55[4]), get(order_q55[5]), get(order_q55[6]),
      get(order_q55[7]), get(order_q55[8]), get(order_q55[9])
    ),
    seq_q55 = if_else(str_detect(seq_q55, "[.,]"), NA_character_, seq_q55),

    #number of switch
    switch_q54 = map_dbl(
      seq_q54,
      function(x) length(str_split(x, "(?<=(.))(?!\\1)")[[1]]) - 2
    ),
    switch_q54 = if_else(switch_q54 == -1, NA_real_, switch_q54),

    switch_q55 = map_dbl(
      seq_q55,
      function(x) length(str_split(x, "(?<=(.))(?!\\1)")[[1]]) - 2
    ),
    switch_q55 = if_else(switch_q55 == -1, NA_real_, switch_q55),

    #rational response
    rational_today = if_else(switch_q54 <= 1, 1, 0),
    rational_day90 = if_else(switch_q55 <= 1, 1, 0),

    #first switch point
    pos_q54 = map_dbl(seq_q54, first_2_position),
    pos_q55 = map_dbl(seq_q55, first_2_position),

    #lower df
    lower_df_today = today_df[order_today_df[pos_q54]],
    lower_df_day90 = day90_df[order_day90_df[pos_q55]],

    #upper df
    upper_df_today = c(NA_real_, today_df[order_today_df])[pos_q54],
    upper_df_day90 = c(NA_real_, day90_df[order_day90_df])[pos_q55],

    #bound
    bound_df_today = if_else(is.na(lower_df_today) | is.na(upper_df_today), 1, 0),
    bound_df_day90 = if_else(is.na(lower_df_day90) | is.na(upper_df_day90), 1, 0),

    #df
    lower_df_today = if_else(is.na(lower_df_today) & !is.na(seq_q54), 0.50, lower_df_today),
    lower_df_day90 = if_else(is.na(lower_df_day90) & !is.na(seq_q55), 0.50, lower_df_day90),

    upper_df_today = if_else(is.na(upper_df_today) & !is.na(seq_q54), 1.01, upper_df_today),
    upper_df_day90 = if_else(is.na(upper_df_day90) & !is.na(seq_q55), 1.01, upper_df_day90),

    df_today = (lower_df_today + upper_df_today) / 2,
    df_day90 = (lower_df_day90 + upper_df_day90) / 2,

    #present bias
    p_bias = if_else(df_today < df_day90, 1, 0),
    beta = if_else(p_bias == 1, df_today / df_day90, 1),

    #procrastination of homework
    q47 = if_else(str_detect(q47, "[.,]"), NA_character_, q47),
    q48 = if_else(str_detect(q48, "[.,]"), NA_character_, q48),
    q47 = as.numeric(q47),
    q48 = as.numeric(q48),
    q48 = if_else(q48 == 6, NA_real_, q48),
    proc_hw = if_else(q48 < q47, 1, 0)
  ) %>%
  select(-starts_with("seq"), -starts_with("pos"), -starts_with("switch"))

# * Altruistic preference
data3 <- data2 %>%
  mutate(
    q51 = if_else(str_detect(q51, "[.]"), NA_character_, q51),
    q52 = if_else(str_detect(q52, "[.]"), NA_character_, q52),
    q51 = as.numeric(q51),
    q52 = as.numeric(q52),

    selfish = if_else(q51 == 5 & q52 <= 2, 1, 0),
    warm_glow = if_else(q51 < 5 & q52 == 2, 1, 0),
    impure = if_else(q51 < 5 & q52 != 2, 1, 0)
  )

# * output
output <- data3 %>%
  select(-starts_with("q"))

write.csv(
  output,
  here(path, "donordata_clean.csv"),
  fileEncoding = "cp932"
)
