## R code to render RMarkdown file appropriately: ST 558 Project1
rmarkdown::render('./ST558-Project1-Vignette.Rmd' ,
                  output_format = 'github_document',
                  output_file = "README",
                  output_dir = './',
                  output_options = list(
                    toc = TRUE,
                    toc_depth= 2,
                    number_sections= TRUE)
)
