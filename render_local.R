#### Render all the plots ####

source("prerequisites.R")
setwd("analyses")

for (file in dir(".", pattern = ".Rmd")){
  rmarkdown::render(file)
}

system(command = "rsync -avz *html *_files /srv/stats.jemu.name/theincomparable/")
