library(here)
library(rmarkdown)

render_inference_opt <- function( fe = TRUE,
                                  cluster = TRUE,
                                  main_se = "CR2",
                                  cluster_se = "CR2")
{
  out <- paste0(
    "body_fe-", fe,
    "_cluster-", cluster,
    "_main-se-", main_se,
    "_cluster-se-", cluster_se,
    ".docx"
  )
  
  render(
    here("body.rmd"),
    output_file = out,
    params = list(
      is_fe = fe,
      is_cluster = cluster,
      main_se_type = main_se,
      cluster_se_type = cluster_se
    ),
    envir = parent.frame()
  )
}

render_inference_opt(TRUE, FALSE, "stata", "stata")
render_inference_opt(FALSE, FALSE, "stata", "stata")
render_inference_opt(TRUE, TRUE, "stata", "stata")
render_inference_opt(FALSE, TRUE, "stata", "stata")
render_inference_opt(TRUE)
render_inference_opt(FALSE)