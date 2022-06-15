#+ include = FALSE
library(here)
source(here("R", "_library.r"))

#+ include = FALSE
root <- "D:/JMDPフィールド実験"
raw <- "raw.csv"
schedule <- "schedule.csv"

#+ include = FALSE
rawdt <- read_csv(
  here(root, "original", raw),
  locale = locale(encoding = "cp932")
)

schedule_dt <- read_csv(
  here(root, "original", schedule),
  locale = locale(encoding = "cp932")
)

#+ include = FALSE
shape_rawdt <- rawdt %>%
  rename(
    ym = 発送年月,
    treat = パターン,
    prefecture = ﾄﾞﾅｰ都道府県,
    sex = ﾄﾞﾅｰ性別,
    age = `ﾄﾞﾅｰ年齢(発送時)`,
    coordinate = コーディネート回数,
    reply = 返信有無,
    days_reply = 返信までの日数,
    intention = 希望の有無,
    test = 確認検査実施の有無,
    candidate = 第一候補選定の有無,
    consent = 最終同意の有無,
    donate = 採取の有無,
    reasonBM = 終了理由BM,
    reasonPB = 終了理由PB,
    plan_method = 開始時採取方法,
    method = 終了時採取方法
  ) %>%
  mutate(
    year = year(ym(ym)),
    month = month(ym(ym)),
    male = if_else(sex == "M", 1, 0),
    reply = if_else(reply == "有", 1, 0),
    days_reply = if_else(days_reply != "-", as.numeric(days_reply), NA_real_),
    intention = if_else(intention == "希望する", 1, 0),
    test = if_else(test == "有", 1, 0),
    candidate = if_else(candidate == "有", 1, 0),
    consent = if_else(consent == "有", 1, 0),
    donate = if_else(donate == "有", 1, 0),
    sentenceB = if_else(treat %in% c("B", "D"), 1, 0),
    sentenceC = if_else(treat %in% c("C", "D"), 1, 0),
  ) %>%
  select(
    year,
    month,
    treat,
    sentenceB,
    sentenceC,
    prefecture,
    male,
    age,
    coordinate,
    reply,
    days_reply,
    intention,
    test,
    candidate,
    consent,
    donate,
    reasonBM,
    reasonPB,
    plan_method,
    method
  )

#+ include = FALSE
shape_schedule_dt <- schedule_dt %>%
  select(
    ymd = 日付,
    treat = パターン
  ) %>%
  mutate(
    ymd = ymd(ymd),
    month = month(ymd(ymd))
  ) %>%
  group_by(month, treat) %>%
  summarize(
    year = year(ymd),
    start_date = min(ymd),
    end_date = max(ymd)
  ) %>%
  ungroup() %>%
  arrange(start_date) %>%
  distinct() %>%
  group_by(month) %>%
  mutate(week = 1:n()) %>%
  ungroup() %>%
  mutate(RCTweek = 1:n()) %>%
  select(
    year,
    month,
    treat,
    week,
    RCTweek,
    start_date,
    end_date
  )

#+ include = FALSE
combine <- shape_schedule_dt %>%
  right_join(shape_rawdt, by = c("year", "month", "treat"))

#+ include = FALSE
write.csv(
  combine,
  file = here(root, "shaped.csv"),
  fileEncoding = "CP932",
  quote = FALSE,
  row.names = FALSE
)

write_csv(
  shape_schedule_dt,
  file = here(root, "RCT-schedule.csv")
)
