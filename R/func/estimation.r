estimation_model <- function(is_fe = params$is_fe){
  mod <- list(
    unctrl = value ~ treat,
    ctrl = value ~ treat + age_demean + I(age_demean^2) + male + coordinate +
      hospital_per_area + PB_per_area + BM_per_area
  )

  if (is_fe) {
    mod[[2]] <- update(mod[[2]], . ~ . + factor(month) + factor(week))
  }

  mod
}

lm_all_stock <- function(data,
                          cluster = params$is_cluster,
                          se_type = params$se_type,
                          fe = params$is_fe)
{
  mod <- estimation_model(fe)

  est_stock <- data %>%
    group_by(outcome) %>%
    nest() %>%
    mutate(
      fit1 = map(
        data,
        ~ lm_robust(
          mod$unctrl,
          data = .,
          cluster = if (cluster) .$RCTweek,
          se_type = se_type
        )
      ),
      fit2 = map(
        data,
        ~ lm_robust(
          mod$ctrl,
          data = .,
          cluster = if (cluster) .$RCTweek,
          se_type = se_type
        )
      )
    ) %>%
    pivot_longer(
      fit1:fit2,
      names_prefix = "fit",
      names_to = "model",
      values_to = "fit"
    )

  ctrl_avg <- data %>%
    dplyr::filter(treat == "A") %>%
    group_by(outcome) %>%
    summarize(mean = sprintf("%1.4f", mean(value)))

  add_table <- tibble::tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
    "Control average", ctrl_avg$mean[1], ctrl_avg$mean[1],
    ctrl_avg$mean[2], ctrl_avg$mean[2],
    ctrl_avg$mean[3], ctrl_avg$mean[3],
    "Covariates", "", "X", "", "X", "", "X"
  )

  attr(add_table, "position") <- 7:8

  tbl_est_stock <- est_stock %>%
    pull(fit) %>%
    setNames(paste0("(", seq_len(length(.)), ")")) %>%
    modelsummary(
      coef_map = c(
        "treatB" = "Treatment B",
        "treatC" = "Treatment C",
        "treatD" = "Treatment D"
      ),
      stars = c("***" = .01, "**" = .05, "*" = .1),
      fmt = 4,
      gof_omit = "R2|AIC|BIC|Log|Std|FE|se_type",
      add_rows = add_table,
    )
  
  class(tbl_est_stock) <- append(class(tbl_est_stock), "lm_all_stock")
  
  return(tbl_est_stock)
}

lm_subset_stock <- function(data,
                            cluster = params$is_cluster,
                            se_type = params$se_type,
                            fe = params$is_fe)
{
  mod <- estimation_model(fe)
  
  est_stock_sub <- use$stock %>%
    group_by(outcome, male, age_less30) %>%
    nest() %>%
    mutate(est = map(data, ~ lm_robust(
      update(mod$ctrl, . ~ . - male - age_demean),
      cluster = if (cluster) .$RCTweek,
      se_type = se_type,
      data = .x
    )))

  plotdt_stock_sub <- est_stock_sub %>%
    mutate(
      fit = map(est, tidy),
      fit = map(fit, ~ subset(.x, str_detect(term, "treat"))),
      fit = map(fit, ~ dplyr::select(.x, -outcome)),
      N = map_chr(est, ~ paste("N =", nobs(.x))),
      mean = map_dbl(data, ~ with(subset(., treat == "A"), mean(value))),
      mean = sprintf("Control avg = %1.3f", mean)
    ) %>%
    select(-data, -est) %>%
    unnest(cols = fit) %>%
    mutate(
      pos = paste0(male, age_less30),
      pos = factor(
        pos,
        levels = c("01", "00", "11", "10"),
        labels = c(
          "Female\u00d7\nAge<30",
          "Female\u00d7\n30\u2264Age",
          "Male\u00d7\nAge<30",
          "Male\u00d7\n30\u2264Age"
        )
      ),
      term = str_replace(term, "treat", ""),
      term = factor(term, LETTERS[2:4])
    )

  text <- plotdt_stock_sub %>%
    select(male, age_less30, pos, N, mean) %>%
    distinct()
  
  res <- list(
    plotdt = plotdt_stock_sub,
    text = text
  )
  class(res) <- append(class(res), "lm_subset_stock")
  res
}                                