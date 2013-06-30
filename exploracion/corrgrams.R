# visualizing correlations with corrgrams
library(corrgram)
library(ellipse)
library(FactoMineR)
data(decathlon)
str(decathlon)
help(decathlon)
R = cor(decathlon[, 1:10])
round(R, 3)
corrgram(R)
# corrgram (lower triangular)
corrgram(R, order = NULL, lower.panel = panel.shade, upper.panel = NULL, text.panel = panel.txt, main = "Decathlon Data")
# corrgram with pie charts
corrgram(R, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt, main = "Decathlon Data")
# default corrgram plotcorr()
plotcorr(R)
# colored corrgram
plotcorr(R, col = colorRampPalette(c("firebrick3", "white", "navy"))(10))
# another colored corrgram
plotcorr(R, col = colorRampPalette(c("#E08214", "white", "#8073AC"))(10), type = "lower")






