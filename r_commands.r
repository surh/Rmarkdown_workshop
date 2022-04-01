rmarkdown::render("report.Rmd", output_file = "myfile.html")

options(knitr.purl.inline = TRUE)
knitr::purl("report.Rmd", documentation = 2, output = "report.r")


