library(arulesSequences)
x <- read_baskets(con = system.file("misc", "zaki.txt", package = "arulesSequences"), 
                  info = c("sequenceID","eventID","SIZE"))
inspect(x)
sequnces = cspade(x,parameter = list(support = 0.4,maxgap=5))
inspect(sequnces)
