dataset <- function(rawdt)
{
  use <- rawdt %>%
    dplyr::filter(ongoing == 0) %>%
    mutate(
      treat = factor(treat, levels = LETTERS[1:4]),
      plan_two_methods = if_else(plan_method == "BM/PB", 1, 0),
      age_less30 = if_else(age < 30, 1, 0)
    )
  
  outcome_label <- list(
    reply = "Reply",
    positive = "Positive intention",
    negative = "Negative intention",
    test = "CT",
    candidate = "Candidate",
    consent = "Consent",
    donate = "Donation"
  )

  stock <- use %>%
    dplyr::filter(exg_stop_reply == 0) %>%
    rename(positive = intention) %>%
    mutate(
      negative = reply * (1 - positive),
      age_demean = age - mean(rawdt$age),
    ) %>%
    select(reply, positive, negative, everything()) %>%
    pivot_longer(reply:negative, names_to = "outcome") %>%
    mutate(outcome = factor(
      outcome,
      levels = unlist(names(outcome_label)[1:3]),
      labels = unlist(outcome_label[1:3])
    ))
  
  flow <- stock %>%
    mutate(
      days_reply = if_else(is.na(days_reply), 10000, days_reply),
      days4 = if_else(days_reply <= 4, value, 0),
      days7 = if_else(days_reply <= 7, value, 0),
      days10 = if_else(days_reply <= 10, value, 0),
      days14 = if_else(days_reply <= 14, value, 0),
      days21 = if_else(days_reply <= 21, value, 0),
      days28 = if_else(days_reply <= 28, value, 0)
    ) %>%
    select(-value) %>%
    pivot_longer(days4:days28, names_to = "within", names_prefix = "days") %>%
    mutate(within = as.numeric(within))
  
  exclude <- use %>%
    select(id, starts_with("exg_stop")) %>%
    pivot_longer(
      -id,
      names_to = "outcome", values_to = "exclude",
      names_prefix = "exg_stop_"
    )

  coordination <- use %>%
    select(test, candidate, consent, donate, everything()) %>%
    pivot_longer(test:donate, names_to = "outcome") %>%
    dplyr::left_join(exclude, by = c("id", "outcome")) %>%
    mutate(
      age_demean = age - mean(rawdt$age),
      outcome = factor(
        outcome,
        levels = unlist(names(outcome_label)[4:7]),
        labels = unlist(outcome_label[4:7])
      )
    )
  
  list(
    cs = use,
    stock = stock,
    flow = flow,
    coordination = coordination
  )
}