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

#' コーディネート回数は1人のドナーに対しての多人数の患者がマッチングされているという認識でよい？
#' 採取の有無が「無」なのに、終了理由が「採取後フォローアップ」になっているのはなぜ?
#' - 両方がCo中となっているサンプル N = 68
#' - PBのみがCo中となっているサンプル N = 8
#' - BMのみがCo中となっているサンプル N = 28
#' 採取の有無が「有」となっている人の終了理由は採取かCo中
#' 開始時採取方法がPB(BM)ならば、終了理由BM(PB)は欠損値
#' 開始時接種方法がPB/BMのとき、二つの終了理由が一致しないことがある。これはなぜ？
#' 開始時接種方法がPB/BMのとき、どちらかの終了理由が欠損値になっていることがある。これはなぜ？
#' 確認検査なしに第一候補に選定されることはあるのか（N = 2）？
#' - 検査省略ケースを除く
#' 第一候補に選定されていないのに、最終同意を取ることはあるのか（N = 2）
#' 最終同意なしに採血することはあるのか（N = 1）
#'
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
    id = 1:n(),
    year = year(ym(ym)),
    month = month(ym(ym)),
    male = if_else(sex == "M", 1, 0),
    reply = if_else(reply == "有", 1, 0),
    days_reply = if_else(days_reply != "-", as.numeric(days_reply), NA_real_),
    intention = if_else(intention == "希望する", 1, 0),
    test = if_else(test == "有", 1, 0),
    test = if_else(days_reply == 0 & intention == 1, 1, test),
    candidate = if_else(candidate == "有", 1, 0),
    consent = if_else(consent == "有", 1, 0),
    donate = if_else(donate == "有", 1, 0),
    stage = case_when(
      reply == 0 ~ 0,
      reply == 1 & intention == 0 ~ 1,
      intention == 1 & test == 0 ~ 2,
      test == 1 & candidate == 0 ~ 3,
      candidate == 1 & consent == 0 ~ 4,
      consent == 1 & donate == 0 ~ 5,
      donate == 1 ~ 6
    ),
    sentenceB = if_else(treat %in% c("B", "D"), 1, 0),
    sentenceC = if_else(treat %in% c("C", "D"), 1, 0),
    reasonBM = if_else(is.na(reasonBM), "PB単独コーディネート", reasonBM),
    reasonPB = if_else(is.na(reasonPB), "BM単独コーディネート", reasonPB),
    exg_stop_reply = case_when(
      reply == 1 ~ 0,
      method == "PB" & reasonPB == "患者理由" ~ 1,
      method == "MB" & reasonBM == "患者理由" ~ 1,
      reasonPB == "患者理由" & reasonBM == "患者理由" ~ 1,
      TRUE ~ 0
    ),
    exg_stop_intention = case_when(
      intention == 1 ~ 0,
      method == "PB" & reasonPB == "患者理由" ~ 1,
      method == "MB" & reasonBM == "患者理由" ~ 1,
      reasonPB == "患者理由" & reasonBM == "患者理由" ~ 1,
      TRUE ~ 0
    ),
    exg_stop_test = case_when(
      test == 1 ~ 0,
      method == "PB" & reasonPB == "患者理由" ~ 1,
      method == "MB" & reasonBM == "患者理由" ~ 1,
      reasonPB == "患者理由" & reasonBM == "患者理由" ~ 1,
      TRUE ~ 0
    ),
    exg_stop_candidate = case_when(
      candidate == 1 ~ 0,
      method == "PB" & (reasonPB == "患者理由" | reasonPB == "健康上理由") ~ 1,
      method == "MB" & (reasonPB == "患者理由" | reasonPB == "健康上理由") ~ 1,
      (reasonPB == "患者理由" | reasonPB == "健康上理由") &
        (reasonBM == "患者理由" | reasonBM == "健康上理由") ~ 1,
      TRUE ~ 0
    ),
    exg_stop_consent = case_when(
      consent == 1 ~ 0,
      method == "PB" & (reasonPB == "患者理由" | reasonPB == "健康上理由") ~ 1,
      method == "MB" & (reasonPB == "患者理由" | reasonPB == "健康上理由") ~ 1,
      (reasonPB == "患者理由" | reasonPB == "健康上理由") &
        (reasonBM == "患者理由" | reasonBM == "健康上理由") ~ 1,
      TRUE ~ 0
    ),
    exg_stop_donate = case_when(
      donate == 1 ~ 0,
      method == "PB" & (reasonPB == "患者理由" | reasonPB == "健康上理由") ~ 1,
      method == "MB" & (reasonPB == "患者理由" | reasonPB == "健康上理由") ~ 1,
      (reasonPB == "患者理由" | reasonPB == "健康上理由") &
        (reasonBM == "患者理由" | reasonBM == "健康上理由") ~ 1,
      TRUE ~ 0
    ),
    ongoing = if_else(
      donate == 0 & (reasonPB == "Co中" | reasonBM == "Co中"), 1, 0
    )
  ) %>%
  select(
    id,
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
    stage,
    exg_stop_reply,
    exg_stop_intention,
    exg_stop_test,
    exg_stop_candidate,
    exg_stop_consent,
    exg_stop_donate,
    ongoing,
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
  right_join(shape_rawdt, by = c("year", "month", "treat")) %>%
  dplyr::filter(prefecture != "海外")

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
