# About this repository

This repository provides code to reproduce findings of papers: "Exploring Information Provision to Promote Stem Cell Donation: Evidence from a Field Experiment of the Japan Marrow Donor Program" published in *Journal of Economic Behavior & Organization*.

**IMPORTANT:** The data are confidential since we do not have permission to share data.

# Structure

Manuscripts including tables and figures were prepared in [Rmarkdown](https://bookdown.org/yihui/rmarkdown-cookbook/).

- All codes to output figures and tables are in `JMDP RCT - Main Document.rmd` (main manuscript) and `JMDP RCT - Online Supplementary Material.rmd` (supplementary material).
- `R` folder contains analysis and other internal codes.
- `image` folder contains images used in the main manuscript (Figure 1).

# Codes

We use object-oriented programming with the [`R6` package](https://r6.r-lib.org/index.html). All R files in the R folder define classes (R6 classes). `misc.r` defines custom functions such as graph templates. Analytical code such as regression analysis is defined as methods of objects. To reproduce, you should run codes in the `render.r`.
