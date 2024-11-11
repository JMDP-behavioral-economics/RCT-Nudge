library(here)
library(rmarkdown)

render_with_inference_opt <- function(  input,
                                        cluster = TRUE,
                                        se_type = "CR2",
                                        output = NULL)
{
  out <- output

  if (is.null(out)) {
    out <- paste0(
      "body",
      "_cluster-", cluster,
      "_se-", se_type,
      ".docx"
    )
  }

  render(
    input,
    output_file = out,
    params = list(
      is_cluster = cluster,
      se_type = se_type
    ),
    envir = parent.frame()
  )
}

# Main manuscript
render_with_inference_opt(
  here("JMDP RCT - Main Document.rmd"),
  cluster = FALSE,
  se_type = "stata",
  output = "JMDP RCT - Main Document.pdf"
)

# Main manuscript (Robust analysis)
render_with_inference_opt(
  here("JMDP RCT - Main Document.rmd"),
  cluster = TRUE,
  se_type = "stata",
  output = "JMDP RCT - Main Document (cluster se).pdf"
)

# Online Supplementary Material
render_with_inference_opt(
  here("JMDP RCT - Online Supplementary Material.rmd"),
  cluster = FALSE,
  se_type = "stata",
  output = "JMDP RCT - Online Supplementary Material.pdf"
)

# Online Supplementary Material (Robust analysis with cluster)
render_with_inference_opt(
  here("JMDP RCT - Online Supplementary Material.rmd"),
  cluster = TRUE,
  se_type = "stata",
  output = "JMDP RCT - Online Supplementary Material (cluster se).pdf"
)
