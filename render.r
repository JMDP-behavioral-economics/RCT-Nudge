library(here)
library(rmarkdown)

render_with_inference_opt <- function(  input,
                                        fe = TRUE,
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
    input,
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
render_with_inference_opt(
  here("JMDP RCT - Main Document.rmd"),
  fe = TRUE,
  cluster = FALSE,
  se_type = "stata",
  output = "JMDP RCT - Main Document.pdf"
)

# Main manuscript (Robust analysis)
render_with_inference_opt(
  here("JMDP RCT - Main Document.rmd"),
  fe = TRUE,
  cluster = TRUE,
  se_type = "stata",
  output = "JMDP RCT - Main Document (Robust).pdf"
)

# Online Supplementary Material
render_with_inference_opt(
  here("JMDP RCT - Online Supplementary Material.rmd"),
  fe = TRUE,
  cluster = FALSE,
  se_type = "stata",
  output = "JMDP RCT - Online Supplementary Material.pdf"
)
