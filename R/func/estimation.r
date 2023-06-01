library(rlang)

estimation_model <- function(is_fe = params$is_fe){
  mod <- list(
    unctrl = value ~ treat,
    ctrl = value ~ treat + age_demean + I(age_demean^2) + male + coordinate +
      hospital_per_area + PB_per_area + BM_per_area
  )

  if (is_fe) {
    mod$ctrl <- update(mod$ctrl, . ~ . + factor(month) + factor(week))
  }

  mod
}

lm_all_stock <- function(data,
                          cluster = params$is_cluster,
                          se_type = params$main_se_type,
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

lm_subset_stock <- function(dt,
                            cluster = params$is_cluster,
                            se_type = params$main_se_type,
                            fe = params$is_fe)
{
  mod <- estimation_model(fe)
  
  est_stock_sub <- dt %>%
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

lm_subset_flow <- function( dt,
                            cluster = params$is_cluster,
                            se_type = params$main_se_type,
                            fe = params$is_fe)
{
  mod <- estimation_model(fe)
  
  est_flow_sub <- dt %>%
    group_by(outcome, within, male, age_less30) %>%
    nest() %>%
    mutate(
      fit = map(data, ~ lm_robust(
        update(mod$ctrl, . ~ . - male - age_demean),
        cluster = if (cluster) .$RCTweek,
        se_type = se_type,
        data = .x
      )),
      avg = map_chr(data, ~ sprintf(
        "%1d\n(%1.3f)", within, with(subset(.x, treat == "A"), mean(value))
      ))
    ) %>%
    mutate(
      tidy = map(fit, tidy),
      tidy = map(tidy, ~ subset(.x, str_detect(term, "treat"))),
      tidy = map(tidy, ~ dplyr::select(.x, -outcome))
    ) %>%
    dplyr::select(-data, -fit) %>%
    unnest(cols = tidy) %>%
    mutate(
      term = str_replace(term, "treat", ""),
      term = factor(term, LETTERS[2:4])
    )
  
  res <- list(plotdt = est_flow_sub)
  class(res) <- append(class(res), "lm_subset_flow")
  res
}

lm_all_coordination <- function(data,
                                cluster = params$is_cluster,
                                se_type = params$main_se_type,
                                fe = params$is_fe)
{
  mod <- estimation_model(fe)

  est_coordination <- data %>%
    group_by(outcome) %>%
    nest() %>%
    mutate(
      fit1 = map(
        data,
        ~ lm_robust(
          mod$unctrl,
          data = subset(., exclude == 0),
          cluster = if(cluster) .$RCTweek,
          se_type = se_type
        )
      ),
      fit2 = map(
        data,
        ~ lm_robust(
          mod$ctrl,
          data = subset(., exclude == 0),
          cluster = if(cluster) .$RCTweek,
          se_type = se_type
        )
      ),
      avg = map_chr(
        data,
        ~ with(
          subset(., exclude == 0 & treat == "A"),
          sprintf("%.4f", mean(value))
        )
      )
    ) %>%
    pivot_longer(
      fit1:fit2,
      names_prefix = "fit",
      names_to = "model",
      values_to = "fit"
    ) %>%
    select(-data)

  add_table <- c("Control average", est_coordination$avg) %>%
    rbind(c("Covariates", "", "X", "", "X", "", "X", "", "X")) %>%
    data.frame()

  attr(add_table, "position") <- c(9, 10)

  tbl_est_coordination <- est_coordination %>%
    pull(fit) %>%
    modelsummary(
      coef_map = c(
        "(Intercept)" = "Constant",
        "treatB" = "Treatment B",
        "treatC" = "Treatment C",
        "treatD" = "Treatment D"
      ),
      stars = c("***" = .01, "**" = .05, "*" = .1),
      fmt = 4,
      gof_omit = "R2|AIC|BIC|Log|Std|FE|se_type",
      add_rows = add_table
    )
  
  class(tbl_est_coordination) <- append(class(tbl_est_coordination), "lm_all_coordination")
  tbl_est_coordination
}

lm_subset_coordination <- function( data,
                                    cluster = params$is_cluster,
                                    se_type = params$main_se_type,
                                    fe = params$is_fe)
{
  res <- lm_subset_stock(data, cluster, se_type, fe)
  class(res) <- c("list", "lm_subset_coordination")
  res
}

logit_all_stock <- function(data,
                            fe = params$is_fe)
{
  mod <- estimation_model(fe)
  
  est_stock_logit <- data %>%
    group_by(outcome) %>%
    nest() %>%
    mutate(
      fit1 = map(
        data,
        ~ glm(mod$unctrl, data = ., family = binomial())
      ),
      fit2 = map(
        data,
        ~ glm(mod$ctrl, data = ., family = binomial())
      )
    ) %>%
    pivot_longer(
      fit1:fit2,
      names_prefix = "fit",
      names_to = "model",
      values_to = "fit"
    )

  add_tables <- tibble::tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
    "Covariates", "", "X", "", "X", "", "X"
  )

  attr(add_tables, "position") <- 7

  tbl_stock_logit <- est_stock_logit %>%
    pull(fit) %>%
    setNames(paste0("(", seq_len(length(.)), ")")) %>%
    modelsummary(
      estimate = "{or}",
      statistic = "[{lower.or}, {upper.or}]",
      coef_map = c(
        "treatB" = "Treatment B",
        "treatC" = "Treatment C",
        "treatD" = "Treatment D"
      ),
      add_rows = add_tables,
      gof_omit = "R2|AIC|BIC|RMSE|Std|FE|se_type",
      fmt = fmt_sprintf("%.3f")
    )
  
  class(tbl_stock_logit) <- append(class(tbl_stock_logit), "logit_all_stock")
  tbl_stock_logit
}

logit_all_coordination <- function( data,
                                    fe = params$is_fe)
{
  mod <- estimation_model(fe)

  est_coordination_logit <- data %>%
    group_by(outcome) %>%
    nest() %>%
    mutate(
      fit1 = map(
        data,
        ~ glm(mod$unctrl, data = subset(., exclude == 0), family = binomial())
      ),
      fit2 = map(
        data,
        ~ glm(mod$ctrl, data = subset(., exclude == 0), family = binomial())
      )
    ) %>%
    pivot_longer(
      fit1:fit2,
      names_prefix = "fit",
      names_to = "model",
      values_to = "fit"
    )

  add_tables <- tibble::tribble(
    ~term, ~mod1, ~mod2, ~mod3, ~mod4, ~mod5, ~mod6, ~mod7, ~mod8,
    "Covariates", "", "X", "", "X", "", "X", "", "X"
  )

  attr(add_tables, "position") <- 7

  tbl_coordination_logit <- est_coordination_logit %>%
    pull(fit) %>%
    setNames(paste0("(", seq_len(length(.)), ")")) %>%
    modelsummary(
      estimate = "{or}",
      statistic = "[{lower.or}, {upper.or}]",
      coef_map = c(
        "treatB" = "Treatment B",
        "treatC" = "Treatment C",
        "treatD" = "Treatment D"
      ),
      add_rows = add_tables,
      gof_omit = "R2|AIC|BIC|RMSE|Std|FE|se_type",
      fmt = fmt_sprintf("%.3f")
    )
  
  class(tbl_coordination_logit) <- append(
    class(tbl_coordination_logit),
    "logit_all_coordination"
  )
  tbl_coordination_logit
}

wildbs_subset_stock <- function(data,
                                subset,
                                se_type = params$cluster_se_type,
                                fe = params$is_fe,
                                treat,
                                B = 1000)
{
  subset <- enquo(subset)
  subset_eval <- eval_tidy(subset, data)
  usedt <- data[subset_eval, , drop = FALSE]

  mod <- estimation_model(fe)
  mod$ctrl <- update(mod$ctrl, . ~ . - male - age_demean)

  est <- usedt %>%
    group_by(outcome) %>%
    nest() %>%
    mutate(
      fit1 = map(
        data,
        ~ lm_robust(
          mod$unctrl,
          data = .,
          cluster = RCTweek,
          se_type = se_type
        )
      ),
      fit2 = map(
        data,
        ~ lm_robust(
          mod$ctrl,
          data = .,
          cluster = RCTweek,
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
  
  bs <- usedt %>%
    group_by(outcome) %>%
    nest()
  
  if ("B" %in% treat) {
    bs <- bs %>%
      mutate(
        treatB_1 = map_dbl(data, ~ wildBS(
          mod$unctrl,
          data = .x,
          cluster = RCTweek,
          H0 = "treatB = 0",
          B = B
        )),
        treatB_2 = map_dbl(data, ~ wildBS(
          mod$ctrl,
          data = .x,
          cluster = RCTweek,
          H0 = "treatB = 0",
          B = B
        ))
      )
  }
  
  if ("C" %in% treat) {
    bs <- bs %>%
      mutate(
        treatC_1 = map_dbl(data, ~ wildBS(
          mod$unctrl,
          data = .x,
          cluster = RCTweek,
          H0 = "treatC = 0",
          B = B
        )),
        treatC_2 = map_dbl(data, ~ wildBS(
          mod$ctrl,
          data = .x,
          cluster = RCTweek,
          H0 = "treatC = 0",
          B = B
        ))
      )
  }

  if ("D" %in% treat) {
    bs <- bs %>%
      mutate(
        treatD_1 = map_dbl(data, ~ wildBS(
          mod$unctrl,
          data = .x,
          cluster = RCTweek,
          H0 = "treatD = 0",
          B = B
        )),
        treatD_2 = map_dbl(data, ~ wildBS(
          mod$ctrl,
          data = .x,
          cluster = RCTweek,
          H0 = "treatD = 0",
          B = B
        ))
      )
  }

  bs <- bs %>%
    pivot_longer(
      -(outcome:data),
      names_pattern = "(.*)_(.*)",
      names_to = c(".value", "model")
    )
  
  ctrl_avg <- usedt %>%
    group_by(outcome) %>%
    summarize(mean = mean(value))

  add_tabs <- rbind(
    c("Control average", sprintf("%1.4f", rep(ctrl_avg$mean, each = 2))),
    c("Covariates", rep(c("", "X"), nrow(ctrl_avg))),
    c("Bootstrap, p-value", rep("", nrow(ctrl_avg) * 2))
  )

  if ("B" %in% treat) {
    add_tabs <- rbind(
      add_tabs,
      c("B = 0", sprintf("%1.4f", bs$treatB))
    )
  }

  if ("C" %in% treat) {
    add_tabs <- rbind(
      add_tabs,
      c("C = 0", sprintf("%1.4f", bs$treatC))
    )
  }

  if ("D" %in% treat) {
    add_tabs <- rbind(
      add_tabs,
      c("D = 0", sprintf("%1.4f", bs$treatD))
    )
  }

  add_tabs <- data.frame(add_tabs)
  attr(add_tabs, "position") <- 7:(9 + length(treat))

  tbl <- est %>%
    pull(fit) %>%
    modelsummary(
      coef_map = c(
        "treatB" = "Treatment B",
        "treatC" = "Treatment C",
        "treatD" = "Treatment D"
      ),
      stars = c("***" = .01, "**" = .05, "*" = .1),
      fmt = 4,
      gof_omit = "R2|AIC|BIC|Log|Std|FE|se_type",
      add_rows = add_tabs
    )
  
  res <- list(
    table = tbl,
    check = length(treat)
  )  
  class(res) <- append(class(res), "wildbs_subset_stock")
  res
}

wildbs_subset_coordination <- function( data,
                                        subset,
                                        se_type = params$cluster_se_type,
                                        fe = params$is_fe,
                                        treat,
                                        B = 1000)
{
  subset <- enquo(subset)
  
  res <- wildbs_subset_stock( data,
                              !!subset,
                              se_type,
                              fe,
                              treat,
                              B)
  class(res) <- c("list", "wildbs_subset_coordination")
  res
}
