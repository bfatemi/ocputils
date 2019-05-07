
# landing page texts ------------------------------------------------------


odir <- system.file("page_oops", package = "ocputils")
oops_html <- paste0(readLines(paste0(odir, "/index.html")), collapse = "\n")

# usethis::use_data(oops_html, overwrite = TRUE)
