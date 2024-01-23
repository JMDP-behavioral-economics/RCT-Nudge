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
    here("JMDP RCT - Main Document.rmd"),
    output_file = out,
    params = list(
      is_fe = fe,
      is_cluster = cluster,
      se_type = se_type
    ),
    envir = parent.frame()
  )
}

# Main manuscript
render_inference_opt(
  TRUE, FALSE, "stata",
  "JMDP RCT - Main Document.pdf"
)

# Robust analysis
render_inference_opt(
  TRUE, TRUE, "stata",
  "JMDP RCT - Main Document (Robust).pdf"
)

# Appendix
render(
  here("JMDP RCT - Appendix.rmd"),
  output_file = "JMDP RCT - Appendix.pdf"
)
