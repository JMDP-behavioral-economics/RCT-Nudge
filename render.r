library(here)
library(rmarkdown)

render_inference_opt <- function( fe = TRUE,
                                  cluster = TRUE,
                                  se_type = "CR2",
                                  output = NULL)
{
  out <- output
  
  if (is.null(out)) {
    out <- paste0(
      "body_fe-", fe,
      "_cluster-", cluster,
      "_se-", se_type,
      ".docx"
    )
  }
  
  render(
    here("body.rmd"),
    output_file = out,
    params = list(
      is_fe = fe,
      is_cluster = cluster,
      se_type = se_type
    ),
    envir = parent.frame()
  )
}

render_inference_opt(TRUE, FALSE, "stata", "body-ja.docx") #main
render_inference_opt(TRUE, TRUE, "stata", "robustness-body-ja.docx") # robustness
# render_inference_opt(FALSE, FALSE, "stata")
# render_inference_opt(FALSE, TRUE, "stata")
# render_inference_opt()
# render_inference_opt(FALSE)