### SRS New Hypopituitarism

library(meta)
library(metafor)
library(readr)
library(readxl)

table_SRSnhyp <- read_delim(
  "C:/Users/lucas/Desktop/SRS-NFPA/Outcomes/SRS New hypopituitarism.csv",
  delim = ",",
  escape_double = FALSE,
  trim_ws = TRUE
)

out_SRSnhyp <- metaprop(
  event = Events,
  n = Total,
  studlab = Author,
  data = table_SRSnhyp,
  method = "Inverse",
  method.tau = "DL",
  incr = 0.5,
  allincr = TRUE,
  comb.fixed = FALSE,
  comb.random = TRUE,
  subgroup = bias
)

out_SRSnhyp

out_SRShypo_metareg_sample <- metareg(out_SRSnhyp, Total)
out_SRShypo_metareg_volume <- metareg(out_SRSnhyp, volume)
out_SRShypo_metareg_year <- metareg(out_SRSnhyp, year5)
out_SRShypo_metareg_pstalk_dose <- metareg(out_SRSnhyp, pstalk_dose)

bubble(out_SRShypo_metareg_volume,
       studlab = FALSE,
       xlab = "Mean Tumor Volume (cc)",
       ylab = "SRS New Hypopituitarism",
       col = "black",
       bg = "white",
       pch = 21,
       cex = 2,
       lwd = 2)

bubble(out_SRShypo_metareg_sample,
       studlab = FALSE,
       xlab = "Sample Size",
       ylab = "SRS New Hypopituitarism",
       col = "black",
       bg = "white",
       pch = 21,
       cex = 2,
       lwd = 2)

bubble(out_SRShypo_metareg_year,
       studlab = FALSE,
       xlab = "Publication Period (5-Year Epochs)",
       ylab = "SRS New Hypopituitarism",
       col = "black",
       bg = "white",
       pch = 21,
       cex = 2,
       lwd = 2)

bubble(out_SRShypo_metareg_pstalk_dose,
       studlab = FALSE,
       xlab = "Maximum Dose to Pituitary Stalk (Gy)",
       ylab = "SRS New Hypopituitarism",
       col = "black",
       bg = "white",
       pch = 21,
       cex = 2,
       lwd = 2)


forest(out_SRSnhyp,
       xlim = c(0.0, 0.6),
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
       subgroup = TRUE,
       print.subgroup.labels = TRUE,
       subgroup.name = "",
       print.subgroup.name = TRUE,
       sort.subgroup = FALSE,
       test.effect.subgroup.random = FALSE,
       prediction = TRUE,
       col.predict = "darkgrey",
       col.predict.lines = "darkgrey")
funnel(out_SRSnhyp,
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

metabias(out_SRSnhyp,
         method.bias = "Egger",
         k.min = 10,
         studlab = FALSE)

l1o_out_SRSnhyp <- metainf(out_SRSnhyp, pooled = "random")

forest(l1o_out_SRSnhyp,
       col.square = "black",
       col.inside = "black",
       col.line = "black",
       col.square.lines = "black",
       col.diamond = "darkgray",
       col.diamond.lines.random = "darkgray",
       xlim = c(0.5, 2),
       rightcols = c("effect", "ci", "I2"))

baujat(out_SRSnhyp,
       bg = "grey",
       studlab = FALSE,
       Xlim = c(0, 10))

labbe(out_SRSnhyp,
      bg = "grey",
      studlab = TRUE,
      Xlim = c(0, 0.25),
      ylim = c(0, 0.5))

