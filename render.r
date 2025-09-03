library(here)
library(rmarkdown)
Sys.setlocale(category = "LC_TIME", "C")

render_with_inference_opt <- function(  input,
                                        output = NULL)
{
  out <- output

  if (is.null(out)) {
    out <- "body.docx"
  }

  render(
    input,
    output_file = out,
    envir = parent.frame()
  )
}

# Main manuscript
render_with_inference_opt(
  here("JMDP RCT - Main Document.rmd"),
  output = "JMDP RCT - Main Document.pdf"
)

# Online Supplementary Material
render_with_inference_opt(
  here("JMDP RCT - Online Supplementary Material.rmd"),
  output = "JMDP RCT - Online Supplementary Material.pdf"
)
