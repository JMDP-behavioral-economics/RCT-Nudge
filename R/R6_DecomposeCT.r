library(R6)
source("R/misc.r")

DecomposeCT <- R6::R6Class("DecomposeCT",
  public = list(
    initialize = function(data) {
      private$data <- data
    },
    get_data = function() private$data,
    get_result = function() private$result,
    estimate = function(B = 500) {
      res <- private$decompose_effect(private$data)

      boot <- private$data %>%
        modelr::bootstrap(B) %>%
        mutate(decompose = map(strap, ~ private$decompose_effect(as.data.frame(.))))

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
      res <- private$result

      term_lab <- c(
        "Effect on CT: (A) + (B) + (C) + (D) + (E)",
        rep(
          c(
            "(A) channel through preventing attrition due to patient reasons",
            "(B) channel through preventing attrition due to donor reasons",
            "(C) channel through increasing positive intention to donate",
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

      label <- LETTERS[2:4]

      kbl <- out_tibbled %>%
        knitr::kable(
          caption = title,
          col.names = c("", label),
          align = paste(c("l", rep("c", length(label))), collapse = ""),
          booktabs = TRUE,
          linesep = ""
        )

      if (hold) {
        kbl <- kbl %>% kable_styling(font_size = font_size, latex_options = "HOLD_position")
      } else {
        kbl <- kbl %>% kable_styling(font_size = font_size)
      }

      kbl %>%
        add_header_above(c(" " = 1, "Experimental Arms" = length(label))) %>%
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
    decompose_effect = function(data) {
      ctrl <- subset(data, treat == "A")
      ctrl_avg <- list(
        positive = with(ctrl, mean(positive)),
        exg_stop = with(subset(ctrl, positive == 1), mean(exg_stop_test))
      )
      ctrl_avg <- append(
        ctrl_avg,
        c("end_stop" = 1 - with(subset(ctrl, positive == 1), mean(test)) - ctrl_avg$exg_stop)
      )

      mods <- list(
        positive = positive ~ treat,
        exg_stop = I(test == 0 & exg_stop_test == 1) ~ treat,
        end_stop = I(test == 0 & exg_stop_test == 0) ~ treat
      )

      est_mods <- mods
      est_mods[[1]] <- lm_robust(mods[[1]], data = data)
      est_mods[2:3] <- mods[2:3] %>%
        map(~ lm_robust(., data = subset(data, positive == 1)))

      est_mods <- est_mods %>%
        map(~ broom::tidy(.))

      coef_mat <- LETTERS[2:4] %>%
        map(~ c(
          -coef_from_tidy(est_mods$exg_stop, .),
          -coef_from_tidy(est_mods$end_stop, .),
          coef_from_tidy(est_mods$positive, .),
          -coef_from_tidy(est_mods$positive, .) * coef_from_tidy(est_mods$exg_stop, .),
          -coef_from_tidy(est_mods$positive, .) * coef_from_tidy(est_mods$end_stop, .)
        )) %>%
        reduce(cbind)

      coef_mat[1:2, ] <- coef_mat[1:2, ] * ctrl_avg$positive
      coef_mat[3, ] <- coef_mat[3, ] * (1 - ctrl_avg$exg_stop - ctrl_avg$end_stop)
      coef_mat <- coef_mat * 100

      return(coef_mat)
    }
  )
)