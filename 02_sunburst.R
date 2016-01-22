library(sunburstR)
library(htmlwidgets)

# read in sample visit-sequences.csv data provided in source
#   https://gist.github.com/kerryrodden/7090426#file-visit-sequences-csv
# sequence_data <- read.csv(
#   paste0(
#     "https://gist.githubusercontent.com/kerryrodden/7090426/"
#     ,"raw/ad00fcf422541f19b70af5a8a4c5e1460254e6be/visit-sequences.csv"
#   )
#   ,header=F
#   ,stringsAsFactors = FALSE
# )
# sunburst(sequence_data, count = T)
#
# sunburst(
#   sequence_data
#   # apply sort order to the legendS
#   ,legendOrder = unique(unlist(strsplit(sequence_data[,1],"-")))[1:3]
#   # just provide the name in the explanation in the center
#   ,explanation = "function(d){return d.name}"
# )
#
#

library(TraMineR)
library(sunburstR)
library(pipeR)

# use example from TraMineR vignette
data("mvad")
mvad.alphab <- c(
  "employment", "FE", "HE", "joblessness",
  "school", "training"
)
mvad.seq <- seqdef(mvad, 17:86, xtstep = 6, alphabet = mvad.alphab)

# to make this work, we'll compress the sequences with seqdss
#   could also aggregate with dply later
seqt <- seqtab( seqdss(mvad.seq), tlim = 0, format = "SPS" ) %>>%
  attr("freq") %>>%
  (
    data.frame(
      # appending "-end" is necessary for this to work
      sequence = paste0(
        gsub(
          x = names(.$Freq)
          , pattern = "(/[0-9]*)"
          , replacement = ""
          , perl = T
        )
        ,"-end"
      )
      ,freq = as.numeric(.$Freq)
      ,stringsAsFactors = FALSE
    )
  ) #%>>%

seqt$sequence <- gsub("employment", "em ployment", seqt$sequence)
schart <- seqt %>>% sunburst(count = T)

schart$sizingPolicy$browser$padding <- 30
schart$sizingPolicy$browser$defaultWidth <- "100%"
schart$sizingPolicy$browser$defaultHeight <- 500

saveWidget(schart, file = "sunburst_test.html", selfcontained = FALSE, libdir = "js")
