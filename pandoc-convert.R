library(bookdown)
setwd("/home/john/stats-corner")
blogdown::build_site()
blogdown::serve_site()



library(rmarkdown)
pandoc_convert("http://rpubs.com/StatsCorner/125519", 
               to = "markdown", 
               from = "html",
               output = '/home/john/stats-corner/content/post/2015/calm-down-tim.md')
?pandoc_convert
