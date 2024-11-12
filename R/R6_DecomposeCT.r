library(R6)
source("R/misc.r")

Decompose <- R6::R6Class("Decompose",
  public = list(
    initialize = function(data, outcome) {
      private$data <- data
      private$outcome <- outcome
    },
    get_data = function() private$data,
    get_result = function() private$result,
    estimate = function(outcome, B = 500) {
      endpoints <- private$outcome
      before <- names(endpoints)[which(names(endpoints) == outcome) - 1]

      private$stage_label <- list(
        outcome = private$outcome[[outcome]],
        before = private$outcome[[before]]
      )

      res <- private$decompose_effect(outcome, before, private$data)

      boot <- private$data %>%
        modelr::bootstrap(B) %>%
        mutate(decompose = map(strap, ~ private$decompose_effect(outcome, before, as.data.frame(.))))

      imax <- nrow(res)
      jmax <- ncol(res)

      boot_se <- matrix(nrow = imax, ncol = jmax)

      for (i in 1:imax) {
        for (j in 1:jmax) {
          elements <- sapply(boot$decompose, function(x) x[i, j])
          boot_se[i, j] <- sd(elements)
        }
      }

      out <- matrix(nrow = imax * 2 + 1, ncol = jmax)
      out[seq(2, by = 2, length.out = imax), ] <- res
      out[seq(3, by = 2, length.out = imax), ] <- boot_se
      out[1, ] <- colSums(res)

      private$result <- out
      invisible(self)
    },
    kable = function(title = "", notes = "", font_size = 9, hold = FALSE){
      t_label <- levels(private$data$treat)[-1]
      res <- private$result
      colnames(res) <- t_label
      s_label <- private$stage_label
      s_label$before <- ifelse(
        s_label$before == "CT",
        "CT",
        str_to_lower(s_label$before)
      )

      term_lab <- c(
        paste0("Effect on ", s_label$outcome, ": (A) + (B) + (C) + (D) + (E)"),
        rep(
          c(
            "(A) Channel through preventing attrition due to exogenous reasons",
            "(B) Channel through preventing attrition due to endogenous reasons",
            paste("(C) Channel through increasing", s_label$before),
            "(D) Interaction of channel (A) and (C)",
            "(E) Interaction of channel (B) and (C)"
          ),
          each = 2
        )
      )

      out_tibbled <- as_tibble(res) %>%
        bind_cols(stat = c("estimate", rep(c("estimate", "se"), 5)), .) %>%
        bind_cols(term = term_lab, .) %>%
        mutate_at(
          vars(-term, -stat),
          list(~ ifelse(stat == "estimate", sprintf("%1.3f", .), sprintf("(%1.3f)", .)))
        ) %>%
        mutate(
          term = if_else(stat == "se", "", term)
        ) %>%
        select(-stat)

      kbl <- out_tibbled %>%
        knitr::kable(
          caption = title,
          col.names = c("", t_label),
          align = paste(c("l", rep("c", length(t_label))), collapse = ""),
          booktabs = TRUE,
          linesep = ""
        )

      if (hold) {
        kbl <- kbl %>% kable_styling(font_size = font_size, latex_options = "HOLD_position")
      } else {
        kbl <- kbl %>% kable_styling(font_size = font_size)
      }

      kbl %>%
        add_header_above(c(" " = 1, "Experimental Arms" = length(t_label))) %>%
        kableExtra::footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  ),
  private = list(
    result = NULL,
    data = NULL,
    outcome = NULL,
    stage_label = NULL,
    decompose_effect = function(point, before, data) {
      exg_stop_point <- paste0("exg_stop_", point)

      ctrl_dt1 <- subset(data, treat == levels(data$treat)[1])
      ctrl_dt2 <- ctrl_dt1[ctrl_dt1[, before, drop = TRUE] == 1, ]

      ctrl_avg <- list(
        before = mean(ctrl_dt1[, before, drop = TRUE]),
        exg_stop = mean(ctrl_dt2[, exg_stop_point, drop = TRUE]),
        end_stop = 1 -
          mean(ctrl_dt2[, point, drop = TRUE]) -
          mean(ctrl_dt2[, exg_stop_point, drop = TRUE])
      )

      outcome <- list(
        before = before,
        exg_stop = paste0("I(", point, " == 0 & ", exg_stop_point, "== 1)"),
        end_stop = paste0("I(", point, " == 0 & ", exg_stop_point, "== 0)")
      )

      mods <- outcome %>%
        lapply(function(x) reformulate("treat", x))

      est_mods <- mods
      est_mods[[1]] <- lm_robust(mods[[1]], data = data)

      estdt2 <- data[data[, before, drop = TRUE] == 1, ]
      est_mods[2:3] <- mods[2:3] %>%
        map(~ lm_robust(., data = estdt2))

      est_mods <- est_mods %>%
        map(~ broom::tidy(.))

      coef_mat <- levels(data$treat)[-1] %>%
        map(~ c(
          -coef_from_tidy(est_mods$exg_stop, .),
          -coef_from_tidy(est_mods$end_stop, .),
          coef_from_tidy(est_mods$before, .),
          -coef_from_tidy(est_mods$before, .) * coef_from_tidy(est_mods$exg_stop, .),
          -coef_from_tidy(est_mods$before, .) * coef_from_tidy(est_mods$end_stop, .)
        )) %>%
        reduce(cbind)

      coef_mat[1:2, ] <- coef_mat[1:2, ] * ctrl_avg$before
      coef_mat[3, ] <- coef_mat[3, ] * (1 - ctrl_avg$exg_stop - ctrl_avg$end_stop)
      coef_mat <- coef_mat * 100

      return(coef_mat)
    }
  )
)