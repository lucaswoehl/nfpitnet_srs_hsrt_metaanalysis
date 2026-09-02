### SRS 3-Year Tumor Control

library(meta)
library(metafor)
library(readr)
library(readxl)

table_SRS3tc <- read_delim(
  "C:/Users/lucas/Desktop/SRS-NFPA/Outcomes/SRS 3-year tumor control.csv",
  delim = ",",
  escape_double = FALSE,
  trim_ws = TRUE
)

out_SRS3tc <- metaprop(
  event = Events,
  n = Total,
  studlab = Study,
  data = table_SRS3tc,
  method = "Inverse",
  method.tau = "DL"
)

out_SRS3tc

out_SRS3tc_metareg_sample <- metareg(out_SRS3tc, sample)
out_SRS3tc_metareg_year <- metareg(out_SRS3tc, year5)
out_SRS3tc_metareg_volume <- metareg(out_SRS3tc, tumor_volume)

bubble(out_SRS3tc_metareg_sample,
       studlab = FALSE,
       xlab = "Sample Size",
       ylab = "SRS 3-Year Tumor Control",
       col = "black",
       bg = "white",
       pch = 21,
       cex = 2,
       lwd = 2)

bubble(out_SRS3tc_metareg_volume,
       studlab = FALSE,
       xlab = "Mean Tumor Volume (cc)",
       ylab = "SRS 3-Year Tumor Control",
       col = "black",
       bg = "white",
       pch = 21,
       cex = 2,
       lwd = 2)

bubble(out_SRS3tc_metareg_year,
       studlab = FALSE,
       xlab = "Publication Year",
       ylab = "SRS 3-Year Tumor Control",
       col = "black",
       bg = "white",
       pch = 21,
       cex = 2,
       lwd = 2)


forest(out_SRS3tc,
       xlim = c(0.8, 1),
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
funnel(out_SRS3tc,
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

metabias(out_SRS3tc,
         method.bias = "Egger",
         k.min = 10,
         studlab = FALSE)

l1o_out_SRS3tc <- metainf(out_SRS3tc, pooled = "random")

forest(l1o_out_SRS3tc,
       col.square = "black",
       col.inside = "black",
       col.line = "black",
       col.square.lines = "black",
       col.diamond = "darkgray",
       col.diamond.lines.random = "darkgray",
       xlim = c(0.5, 2),
       rightcols = c("effect", "ci", "I2"))

baujat(out_SRS3tc,
       bg = "grey",
       studlab = FALSE,
       Xlim = c(0, 10))

labbe(out_SRS3tc,
      bg = "grey",
      studlab = TRUE,
      Xlim = c(0, 0.25),
      ylim = c(0, 0.5))

