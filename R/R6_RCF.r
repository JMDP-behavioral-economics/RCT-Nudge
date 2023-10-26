library(here)
library(R6)
library(grf)
library(rlang)
library(kableExtra)
source(here("R/misc.r"))

RCF <- R6::R6Class("RCF",
  public = list(
    initialize = function(Y, D, X, X_label) {
      rcf <- multi_arm_causal_forest(X, Y, D)
      tau <- predict(rcf, X)$predictions[, , 1]

      ctrl <- levels(D)[1]
      lab <- str_remove(colnames(tau), paste(" -", ctrl))
      lab <- paste0("effect_", lab)
      colnames(tau) <- lab

      private$X <- X
      private$tau <- tau
      private$rcf <- rcf
      private$ctrl_arm <- ctrl
      private$X_label <- X_label
    },
    get_rcf = function() private$rcf,
    get_X = function() private$X,
    get_tau = function() private$tau,
    subset_boxplot = function() {
      dt <- cbind(private$tau, private$X) %>%
        data.frame() %>%
        pivot_longer(
          all_of(colnames(private$tau)),
          names_to = c(".value", "treat"),
          names_sep = "_"
        ) %>%
        mutate(
          male = factor(male, labels = c("Females", "Males")),
          treat = factor(treat)
        )
      
      dt %>%
        ggplot(aes(x = age, y = effect)) +
        geom_hline(aes(yintercept = 0), linetype = 2) +
        geom_boxplot(aes(group = age)) +
        stat_smooth(se = FALSE, color = "blue") +
        facet_grid(treat ~ male) +
        labs(x = "Age", y = "Predicted treatment effect") +
        my_theme_classic(strip_hjust = 0.5)
    },
    cate = function(...) {
      cond <- private$list_subset(...)
      pattern <- private$bool_pattern(length(cond))
      combine_cond <- private$combine_subset(cond, pattern)

      cate <- seq(length(combine_cond)) %>%
        map(function(i) {
          average_treatment_effect(private$rcf, subset = combine_cond[[i]]) %>%
            mutate(
              i = i,
              z = abs(estimate / std.err),
              p = 2 * pnorm(z, lower.tail = FALSE),
              treat = str_remove(contrast, " - A")
            ) %>%
            select(-contrast, -outcome)
        }) %>%
        map(function(tbl) {
          col_label <- colnames(pattern)
          for (i in col_label) {
            tbl[, col_label] <- pattern[tbl$i, col_label, drop = TRUE]
          }
          tbl
        }) %>%
        reduce(bind_rows) %>%
        select(i, starts_with("cond"), treat, everything())
      
      RCFCate$new(cate, pattern)
    },
    decompose_effect = function(multi_intervention_arm,
                                one_intervention_arm,
                                ...
    ) {
      lhs <- multi_intervention_arm
      rhs <- one_intervention_arm
      sum_rhs <- paste0("I(", paste(rhs, collapse = " + "), ")")
      model <- list(
        reformulate(sum_rhs, lhs),
        reformulate(rhs, lhs)
      )

      lh1 <- paste(sum_rhs, "- 1")
      combn_rhs <- combn(rhs, 2)
      lh2 <- sapply(
        seq(ncol(combn_rhs)),
        function(i) paste(combn_rhs[1, i], "-", combn_rhs[2, i])
      )

      tau <- private$tau
      colnames(tau) <- str_remove(colnames(tau), "effect_")
      dt <- data.frame(tau)
      
      cond <- private$list_subset(...)
      for (i in seq(length(cond))) {
        label <- paste0("cond", i)
        dt[, label] <- cond[[i]]
      }

      est <- dt %>%
        group_by(across(starts_with("cond"))) %>%
        nest() %>%
        mutate(
          fit1 = map(data, ~ lh_robust(
            model[[1]],
            data = .,
            se_type = "stata",
            linear_hypothesis = lh1
          )),
          fit2 = map(data, ~ lh_robust(
            model[[2]],
            data = .,
            se_type = "stata",
            linear_hypothesis = lh2
          ))
        ) %>%
        select(-data) %>%
        ungroup() %>%
        pivot_longer(
          fit1:fit2,
          names_prefix = "fit",
          names_to = "model",
          values_to = "fit"
        ) %>%
        arrange(desc(across(starts_with("cond")))) %>%
        arrange(model)

      DecomposeEffect$new(
        est,
        lhs,
        c(sum_rhs, rhs),
        c(lh1, lh2)
      )
    },
    effect_characteristics = function(target, effect = 0, ...) {
      X <- private$X
      Y <- private$tau
      bool <- rep(TRUE, nrow(X))

      if (!missing(...)) {
        cond <- enquos(...)
        for (x in cond) {
          add_bool <- eval_tidy(x, data.frame(X))
          bool <- as.logical(bool * add_bool)
        }
      }

      dt <- data.frame(X[bool, ])
      bool_useX <- apply(dt, 2, var) != 0
      dt <- dt[, bool_useX]
      useX <- colnames(dt)

      label_target <- paste0("effect_", target)
      tau_target <- Y[, label_target, drop = TRUE]
      positive_effect <- ifelse(tau_target > effect, TRUE, FALSE)
      dt[, "positive_effect"] <- positive_effect[bool]

      stats <- dt %>%
        group_by(positive_effect) %>%
        summarize_all(mean) %>%
        pivot_longer(-positive_effect, names_to = "var") %>%
        pivot_wider(values_from = value, names_from = positive_effect) %>%
        mutate_at(vars(`FALSE`, `TRUE`), list(~sprintf("%1.3f", .)))
      
      args <- list(data = dt, se_type = "stata")

      p <- sapply(useX, function(y) {
        mod <- reformulate("positive_effect", y)
        args <- append(args, list(formula = mod))
        est <- do.call(lm_robust, args)
        p <- sprintf("%1.3f", tidy(est)[2, "p.value"])
        p <- ifelse(p == "0.000", "< 0.001", p)
      })

      p_table <- data.frame(
        p.value = p,
        var = names(p)
      )

      cov_label <- data.frame(
        var = c(private$X_label, "N"),
        label = c(names(private$X_label), "N")
      )

      tbl <- stats %>%
        bind_rows(c(var = "N", "FALSE" = sum(!bool), "TRUE" = sum(bool))) %>%
        dplyr::left_join(p_table, by = "var") %>%
        dplyr::left_join(cov_label, by = "var") %>%
        select(label, everything()) %>%
        select(-var)
      
      EffectCharacteristics$new(tbl)
    }
  ),
  private = list(
    X = NULL,
    tau = NULL,
    rcf = NULL,
    ctrl_arm = NULL,
    X_label = NULL,
    list_subset = function(...) {
      cond <- enquos(...)
      sub <- list()
      for (i in 1:length(cond)) {
        bool <- eval_tidy(cond[[i]], data.frame(private$X))
        sub <- append(sub, list(bool))
      }
      sub
    },
    bool_pattern = function(length_cond) {
      pattern <- data.frame(cond1 = c(TRUE, FALSE))
      for (i in seq(length_cond - 1)) {
        col_lab <- paste0("cond", i + 1)
        pattern[, col_lab] <- c(TRUE, FALSE)
      }
      tibble(expand.grid(pattern))
    },
    combine_subset = function(list_subset, bool_pattern) {
      lapply(
        seq(nrow(bool_pattern)),
        function(i) {
          bool <- rep(TRUE, nrow(private$X))
          for (j in seq(length(list_subset))) {
            bool <- bool * c(list_subset[[j]] == bool_pattern[i, j, drop = TRUE])
          }
          as.logical(bool)
        }
      )
    }
  )
)

RCFCate <- R6::R6Class("RCFCate",
  public = list(
    initialize = function(cate, pattern) {
      private$cate <- cate
      private$pattern <- pattern
    },
    get_est = function() private$cate,
    flextable = function(label, title = "", notes = "", font_size = 9) {
      tbl <- private$table()

      header <- as.list(c("", paste0("(", seq(ncol(tbl) - 1), ")")))
      names(header) <- colnames(tbl)

      flex <- flextable(tbl) %>%
        set_caption(title) %>%
        set_header_labels(values = header)

      if (!missing(label)) {
        names(label) <- paste0("cond", seq(length(label)))
        for (i in names(label)) {
          label[[i]] <- factor(private$pattern[, i, drop = TRUE], labels = label[[i]])
          label[[i]] <- as.character(label[[i]])
        }
        for (i in label) {
          rle1 <- rle(i)
          rle1$lengths <- c(1, rle1$lengths)
          rle1$values <- c("", rle1$values)
          flex <- flex %>%
            add_header_row(values = rle1$values, colwidths = rle1$lengths)
        }
      }

      flex %>%
        align(j = -1, align = "center", part = "all") %>%
        add_footer_lines(paste(
          "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01.",
          "Standard errors are in parentheses.",
          "See Athey and Wager (2019) for estimation method",
          "of conditional average treatment effect (CATE).",
          "Since these estimates are asymptotically normal,",
          "we calculate z-score under the null hypothesis that CATE is zero,",
          "and obtain p-value.",
          notes
        )) %>%
        width(j = 1, 1) %>%
        fontsize(size = font_size, part = "all") %>%
        ft_theme()
    },
    kable = function(label, title = "", notes = "", font_size = 9) {
      tbl <- private$table()

      header <- as.list(c("", paste0("(", seq(ncol(tbl) - 1), ")")))

      kbl <- tbl %>%
        knitr::kable(
          caption = title,
          col.names = header,
          align = paste(c("l", rep("c", ncol(tbl) - 1)), collapse = ""),
          booktabs = TRUE,
          linesep = ""
        ) %>%
        kableExtra::kable_styling(font_size = font_size)
      
      if (!missing(label)) {
        names(label) <- paste0("cond", seq(length(label)))
        for (i in names(label)) {
          label[[i]] <- factor(private$pattern[, i, drop = TRUE], labels = label[[i]])
          label[[i]] <- c(as.character(label[[i]]))
        }
        for (i in label) {
          rle1 <- rle(i)
          new_header <- c(1, rle1$lengths)
          names(new_header) <- c(" ", rle1$values)
          kbl <- kbl %>%
            kableExtra::add_header_above(new_header)
        }
      }

      kbl %>%
        kableExtra::footnote(
          general_title = "",
          general = paste(
            "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01.",
            "Standard errors are in parentheses.",
            "See Athey and Wager (2019) for estimation method",
            "of conditional average treatment effect (CATE).",
            "Since these estimates are asymptotically normal,",
            "we calculate z-score under the null hypothesis that CATE is zero,",
            "and obtain p-value.",
            notes
          ),
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  ),
  private = list(
    cate = NULL,
    pattern = NULL,
    table = function() {
      private$cate %>%
        mutate(
          estimate = case_when(
            p < 0.01 ~ sprintf("%1.4f***", estimate),
            p < 0.05 ~ sprintf("%1.4f**", estimate),
            p < 0.1 ~ sprintf("%1.4f*", estimate),
            TRUE ~ sprintf("%1.4f", estimate)
          ),
          std.err = sprintf("(%1.4f)", std.err)
        ) %>%
        select(-z, -p) %>%
        pivot_longer(estimate:std.err, names_to = "stats") %>%
        pivot_wider(
          names_from = starts_with(c("i", "cond")),
          values_from = value
        ) %>%
        mutate(treat = if_else(stats == "std.err", "", treat)) %>%
        select(-stats)
    }
  )
)

DecomposeEffect <- R6::R6Class("DecomposeEffect",
  public = list(
    initialize = function(est, response, vars, lh, ctrl_arm) {
      private$est <- est
      private$vars <- vars
      private$lh <- lh
      private$response <- response
    },
    get_est = function() private$est,
    print_msummary = function() private$reg_tab,
    flextable = function( subset_label,
                          title = "",
                          notes = "",
                          font_size = 9
                          ) {
      private$msummary("data.frame")
      tbl <- private$reg_tab %>%
        mutate(
          part = if_else(
            term %in% str_remove(private$lh, "I"),
            "Linear combination test (F-test)",
            ""
          ),
          term = if_else(statistic == "std.error", "", term)
        ) %>%
        select(-statistic)
      
      flex <- tbl %>%
        as_grouped_data("part") %>%
        as_flextable(hide_grouplabel = TRUE) %>%
        set_caption(title)
      
      est <- private$est
      names(subset_label) <- paste0("cond", seq(length(subset_label)))
      for (i in names(subset_label)) {
        est[, i] <- factor(est[, i, drop = TRUE], labels = subset_label[[i]])
      }

      label_col <- est %>%
        select(starts_with("cond")) %>%
        with(rev(colnames(.)))

      for (i in label_col) {
        new_header <- c(" ", as.character(est[, i, drop = TRUE]))
        rle1 <- rle(new_header)
        flex <- flex %>%
          add_header_row(values = rle1$values, colwidths = rle1$lengths)
      }

      flex <- flex %>%
        add_header_row(
          values = c("", paste("Treatment effect of", private$response)),
          colwidths = c(1, nrow(est))
        ) %>%
        align(j = -1, align = "center", part = "all") %>%
        add_footer_lines(paste(
          "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01.",
          "The robust standard errors are in parentheses.",
          notes
        )) %>%
        width(j = 1, 1) %>%
        fontsize(size = font_size, part = "all") %>%
        ft_theme()
      
      num_vars_line <- (1 + length(private$vars)) * 2
      num_lh_line <- length(private$lh) * 2
      pos_lh <- c(num_vars_line + 2, num_vars_line + num_lh_line + 1)

      flex <- flex %>%
        hline(num_vars_line + num_lh_line + 2, border = fp_border()) %>%
        padding(pos_lh[1]:pos_lh[2], padding.left = 10)
      
      flex
    },
    kable = function( subset_label,
                      title = "",
                      notes = "",
                      font_size = 9,
                      ...) {
      private$msummary(
        "kableExtra",
        title = title,
        escape = FALSE,
        align = paste(c("l", rep("c", nrow(private$est))), collapse = ""),
        ...
      )

      kbl <- private$reg_tab %>%
        kableExtra::kable_styling(font_size = font_size)
      
      est <- private$est
      names(subset_label) <- paste0("cond", seq(length(subset_label)))
      for (i in names(subset_label)) {
        est[, i] <- factor(est[, i, drop = TRUE], labels = subset_label[[i]])
      }

      label_col <- est %>%
        select(starts_with("cond")) %>%
        with(rev(colnames(.)))

      for (i in label_col) {
        new_header <- c(" ", as.character(est[, i, drop = TRUE]))
        rle1 <- rle(new_header)
        reduce_new_header <- rle1$lengths
        names(reduce_new_header) <- rle1$values
        kbl <- kbl %>%
          kableExtra::add_header_above(reduce_new_header)
      }

      outcome_header <- c(1, nrow(est))
      names(outcome_header) <- c(" ", paste("Treatment effect of", private$response))

      kbl <- kbl %>%
        kableExtra::add_header_above(outcome_header)

      num_vars_line <- (1 + length(private$vars)) * 2
      num_lh_line <- length(private$lh) * 2
      pos_lh <- c(num_vars_line + 1, num_vars_line + num_lh_line)

      kbl %>%
        kableExtra::group_rows(
          "Linear combination test (F-test)",
          pos_lh[1], pos_lh[2],
          bold = FALSE, italic = TRUE
        ) %>%
        kableExtra::footnote(
          general_title = "",
          general = paste(
            "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01.",
            "The robust standard errors are in parentheses.",
            notes
          ),
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  ),
  private = list(
    est = NULL,
    reg_tab = NULL,
    vars = NULL,
    lh = NULL,
    response = NULL,
    msummary = function(output, ...) {
      fit <- private$est %>% pull(fit)
      stars <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)
      gof_omit <- "R2|AIC|BIC|Log|Std|FE|se_type"

      label <- c(
        str_remove(private$vars, "I"),
        str_remove(private$lh, "I")
      )
      label <- c("(Intercept)", label)
      names(label) <- c("(Intercept)", private$vars, private$lh)

      args <- list(
        models = fit,
        coef_map = label,
        output = output,
        stars = stars,
        gof_omit = gof_omit
      )

      if(!missing(...)) args <- append(args, list(...))
      private$reg_tab <- do.call("modelsummary", args)
      invisible(self)
    }
  )
)

EffectCharacteristics <- R6::R6Class("EffectCharacteristics",
  public = list(
    table = NULL,
    initialize = function(table) self$table <- table,
    flextable = function( title = "",
                          notes = "", 
                          font_size = 9,
                          group_label = c("Non-positive", "Positive")) {
      self$table %>%
        flextable() %>%
        set_caption(title) %>%
        set_header_labels(label = "", `FALSE` = "(1)", `TRUE` = "(2)", p.value = "(3)") %>%
        add_header_row(
          values = c("", group_label, "P-value"),
          colwidths = rep(1, 4)
        ) %>%
        add_header_row(
          values = c("", "Predicted treatment effect", ""),
          colwidths = c(1, 2, 1)
        ) %>%
        align(j = -1, align = "center", part = "all") %>%
        width(j = 1, 2) %>%
        add_footer_lines(paste(
          "Notes: Column (1) and (2) show average sample characteristics.",
          "Column (3) shows p-values of difference-in-means test.",
          notes
        )) %>%
        fontsize(size = font_size, part = "all") %>%
        ft_theme()
    },
    kable = function( title = "",
                      notes = "",
                      font_size = 9,
                      group_label = c("Non-positive", "Positive"),
                      escape = TRUE) {
      self$table %>%
        knitr::kable(
          caption = title,
          col.names = c("", paste0("(", 1:3, ")")),
          align = "lcccc",
          booktabs = TRUE,
          linesep = ""
        ) %>%
        kable_styling(font_size = font_size) %>%
        add_header_above(c(" ", group_label, "P-value"), escape = escape) %>%
        add_header_above(c(" ", "Predicted treatment effect" = 2, " ")) %>%
        kableExtra::footnote(
          general_title = "",
          general = paste(
            "Notes: Column (1) and (2) show average sample characteristics.",
            "Column (3) shows p-values of difference-in-means test.",
            notes
          ),
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  ),
  private = list()
)