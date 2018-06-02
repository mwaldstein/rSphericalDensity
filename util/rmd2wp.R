#!/usr/bin/env Rscript
#' Takes an .Rmd file path, knitr it, render as html and publish to WordPress
#'
#' This function is based on these blog posts:
#' http://yihui.name/knitr/demo/wordpress/
#' http://francojc.github.io/publishing-rmarkdown-to-wordpress-or-jekyll/
#' http://thinktostart.com/analyze-instagram-r/
#' The last one is actually not about RMarkdown nor publishing to WordPress
#' but I found there a solution to the SSL connection problem in Windows.
#' Another blog post worth checking out is:
#' http://fredhasselman.com/?p=303
#' @param rmdfile path to an .Rmd file 
#' @param publish publish the post automatically?, or leave it as draft
#' @param shortcode syntax highlight?
#' @param ... other params to be passed to knitr::knit2wp()
#' @return the return of knitr::knit2wp()
#' @keywords RWordPress, knitr, rmarkdown, WordPress
#' @export
#' @examples
#' later ...

WPUrl <- ifelse(Sys.getenv("WP_URL") == '',
                "https://micah.waldste.in/blog/xmlrpc.php",
                Sys.getenv("WP_URL"))
WPUser <- ifelse(Sys.getenv("WP_USER") == '', "mwaldstein", Sys.getenv("WP_USER"))
WPPass <- ifelse(Sys.getenv("WP_PASS") == '', "", Sys.getenv("WP_PASS"))

rmd2wp <- function(rmdfile, publish = FALSE, shortcode = TRUE, ...){
    # devtools::install_github(c("duncantl/XMLRPC", "duncantl/RWordPress"))
    library(RWordPress)
    library(knitr)
    library(yaml)

    # Get the title from the .rmd file, contained as yaml parameter ############
    # This code comes from:
    # http://stackoverflow.com/questions/30153194/access-name-of-rmd-file-and-use-in-r
    # Read in the lines of your file
    lines <- readLines(rmdfile)
    # Find the header portion contained between the --- lines. 
    header_line_nums <- which(lines == "---") + c(1, -1)
    # Create a string of just that header portion
    header <- paste(lines[seq(header_line_nums[1], 
                              header_line_nums[2])], 
                    collapse = "\n")
    # parse it as yaml, which returns a list of property values
    title <- yaml::yaml.load(header)$title

    # knit the .rmd file and publish to WordPress ##############################
    # Here, set the blog parameters
    message(paste0(WPUser,' [',WPPass,']'))
    options(WordpressLogin = c(mwaldstein = WPPass),
            WordpressURL = WPUrl)

    # opts_knit$set(base.url = NULL)
    # Set upload.fun for figures to upload to imgur.com
    opts_knit$set(upload.fun = imgur_upload, base.url = NULL)
    opts_chunk$set(fig.width=10, dpi = 74, cache=FALSE)

    #opts_knit$set(upload.fun = RWordPress::uploadFile, base.url = NULL) # upload all images to imgur.com
    # knit the .rmd file and publish to WordPress
    knitr::knit2wp(input = rmdfile, title = title,
                   encoding="utf8",
                   publish = publish, ...)
    message(title)
}

args <- commandArgs(TRUE)
x <- as.character(args[1])

if (length(args) == 0) {
  message("No file passed")
  message("Usage:")
  message("  rmd2wp.R <filename.Rmd>")
  message("\nEnv Variables:")
  message("  WP_URL  - default: 'https://micah.waldste.in/blog/xmlrpc.php'")
  message("  WP_USER - default: 'mwaldstein'")
  message("  WP_PASS - default: ''")
} else {
  rmd2wp(x)
}
