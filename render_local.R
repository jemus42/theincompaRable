#### Render all the plots ####

source("prerequisites.R")
setwd("analyses")

for (file in dir(".", pattern = ".Rmd")){
  rmarkdown::render(file)
}

setwd("..")

system(command = "cd analyses; rsync -avz *html assets /srv/stats.jemu.name/theincomparable/")
