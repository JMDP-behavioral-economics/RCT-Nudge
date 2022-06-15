lincom_fixest <- function(L,
                          model) {
  df1 <- Matrix::rankMatrix(L)[1]
  df2 <- fitstat(model, type = "f")$f$df2

  f <- t(L) %*% coef(model) %*%
    solve(t(L) %*% vcov(model) %*% L) %*%
    t(t(L) %*% coef(model))

  f <- f / df1

  pf(f, df1, df2, lower.tail = FALSE)
}