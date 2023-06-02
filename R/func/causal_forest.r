random_causal_forest <- function(data) {
  covariate <- ~ 0 + male + age + coordinate +
    hospital_per_area + PB_per_area + BM_per_area
  
  usedt <- subset(data, exg_stop_reply == 0)

  Y <- usedt$intention
  X <- model.matrix(covariate, data = usedt)
  W <- usedt$treat

  int_rcf <- multi_arm_causal_forest(X, Y, W)
  int_tau <- predict(int_rcf, X)

  predict_int_rcf <- data.frame(X) %>%
    cbind(int_tau$predictions) %>%
    rename(
      effect_B = `B - A.Y.1`,
      effect_C = `C - A.Y.1`,
      effect_D = `D - A.Y.1`
    ) %>%
    pivot_longer(
      effect_B:effect_D,
      names_to = c(".value", "treat"),
      names_sep = "_"
    ) %>%
    mutate(
      male = factor(male, labels = c("Females", "Males")),
      treat = factor(treat, labels = paste("Message", LETTERS[2:4]))
    )
  
  subset_cond <- list(
    X[, "male"] == 0 & X[, "age"] < 30,
    X[, "male"] == 0 & X[, "age"] >= 30,
    X[, "male"] == 1 & X[, "age"] < 30,
    X[, "male"] == 1 & X[, "age"] >= 30
  )

  ate_int_rcf <- subset_cond %>%
    purrr::map(~ average_treatment_effect(int_rcf, subset = .)) %>%
    reduce(bind_rows) %>%
    mutate(
      z = abs(estimate / std.err),
      p = 2 * pnorm(z, lower.tail = FALSE),
      gender = rep(c("Females", "Males"), each = 6),
      age = rep(c("Young", "Elder", "Young", "Elder"), each = 3)
    )

  tbl_int_rcf <- ate_int_rcf %>%
    select(gender, age, contrast, estimate, std.err, p) %>%
    pivot_wider(names_from = gender, values_from = estimate:p) %>%
    select(age, contrast, ends_with("Females"), ends_with("Males")) %>%
    mutate(contrast = str_extract(contrast, "[B-D]")) %>%
    mutate(age = recode(
      age,
      "Young" = "Age: Less than 30",
      "Elder" = "Age: More than or equal to 30"
    ))
  
  class(tbl_int_rcf) <- append(class(tbl_int_rcf), "cf_cate")

  res <- list(
    est = int_rcf,
    pred = predict_int_rcf,
    cate = tbl_int_rcf
  )

  res
}

boxplot_effect <- function(rcf) {
  rcf$pred %>%
    ggplot(aes(x = age, y = effect)) +
    geom_hline(aes(yintercept = 0), linetype = 2) +
    geom_boxplot(aes(group = age)) +
    stat_smooth(se = FALSE, color = "blue") +
    facet_grid(treat ~ male) +
    labs(x = "Age", y = "Predicted treatment effect") +
    simplegg()
}

cf_corr_effect <- function(rcf) {
  est <- rcf$pred %>%
    mutate(
      age_less30 = if_else(age < 30, 1, 0),
      id = rep(1:(n() / 3), each = 3)
    ) %>%
    pivot_wider(names_from = treat, values_from = effect) %>%
    rename(B = `Message B`, C = `Message C`, D = `Message D`) %>%
    group_by(male, age_less30) %>%
    nest() %>%
    mutate(
      fit1 = map(
        data,
        ~ lm_robust(
          D ~ I(B + C),
          data = .,
          se_type = "HC1"
        )
      ),
      fit2 = map(
        data,
        ~ lm_robust(
          D ~ B + C,
          data = .,
          se_type = "HC1"
        )
      )
    ) %>%
    pivot_longer(
      fit1:fit2,
      names_prefix = "fit",
      names_to = "model",
      values_to = "fit"
    ) %>%
    arrange(model, male, desc(age_less30)) %>%
    pull(fit)
  
  class(est) <- append(class(est), "cf_corr_effect")
  est
}