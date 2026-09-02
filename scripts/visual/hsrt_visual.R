### HSRT New or Worsening Visual Field Defect

library(meta)
library(metafor)
library(readr)
library(readxl)

table_HSRTvisual <- read_delim(
  "C:/Users/lucas/Desktop/SRS-NFPA/Outcomes/HSRT New or worsening of a visual field defect.csv",
  delim = ",",
  escape_double = FALSE,
  trim_ws = TRUE
)

out_HSRTvisual <- metaprop(
  event = Events,
  n = Total,
  studlab = Author,
  data = table_HSRTvisual,
  method = "Inverse",
  method.tau = "DL"
)

out_HSRTvisual


forest(out_HSRTvisual,
       xlim = c(0.00, 0.3),
       pooled.events = TRUE,
       pooled.totals = TRUE,
       fixed = TRUE,
       random = TRUE,
       test.overall = TRUE,
       digits = 2,
       digits.pval = 2,
       print.Q = FALSE,
       print.pval.Q = TRUE,
       print.tau.ci = TRUE,
       print.tau2 = TRUE,
       print.tau2.ci = FALSE,
       print.Q.subgroup = FALSE,
       layout = "meta",
       colgap = "3mm",
       colgap.forest.left = "30mm",
       col.diamond.random = "grey",
       col.diamond.lines.random = "black",
       col.diamond.common = "lightgrey",
       col.diamond.lines.common = "grey",
       col.diamond.subgroup = "grey",
       col.square = "grey",
       col.square.lines = "white",
       col.line = "black",
       col.inside = "black",
       col.study = "black",
       col.pval = "black",
       col.hetstat = "black",
       fs.heading = 12,
       fs.study = 12,
       fs.hetstat = 12,
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Study", "Events", "Total"),
       rightcols = c("effect", "ci", "w.fixed", "w.random"),
       print.df = FALSE,
       subgroup = FALSE,
       print.subgroup.labels = FALSE,
       subgroup.name = "",
       print.subgroup.name = FALSE,
       sort.subgroup = FALSE,
       test.effect.subgroup.random = FALSE,
       prediction = TRUE,
       col.predict = "darkgrey",
       col.predict.lines = "darkgrey")
funnel(out_HSRTvisual,
       studlab = TRUE,
       bg = "black",
       col = "black",
       pch = 16,
       xlab = "Effect Size",
       ylab = "Standard Error",
       main = "",
       refline = 0,
       shade = TRUE,
       col.shade = "gray90",
       grid = TRUE,
       lwd = 1,
       cex = 0.8,
       cex.studlab = 0.8,
       cex.axis = 0.8,
       cex.main = 1)

metabias(out_HSRTvisual,
         method.bias = "Egger",
         k.min = 10,
         studlab = FALSE)

l1o_out_HSRTvisual <- metainf(out_HSRTvisual, pooled = "random")

forest(l1o_out_HSRTvisual,
       col.square = "black",
       col.inside = "black",
       col.line = "black",
       col.square.lines = "black",
       col.diamond = "darkgray",
       col.diamond.lines.random = "darkgray",
       xlim = c(0.5, 2),
       rightcols = c("effect", "ci", "I2"))

baujat(out_HSRTvisual,
       bg = "grey",
       studlab = FALSE,
       Xlim = c(0, 10))

labbe(out_HSRTvisual,
      bg = "grey",
      studlab = TRUE,
      Xlim = c(0, 0.25),
      ylim = c(0, 0.5))

