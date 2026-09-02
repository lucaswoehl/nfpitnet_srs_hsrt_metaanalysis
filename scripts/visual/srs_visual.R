### SRS New or Worsening Visual Field Defect

library(meta)
library(metafor)
library(readr)
library(readxl)

table_SRSvisual <- read_delim(
  "C:/Users/lucas/Desktop/SRS-NFPA/Outcomes/SRS New or worsening of a visual field defect.csv",
  delim = ",",
  escape_double = FALSE,
  trim_ws = TRUE
)

out_SRSvisual <- metaprop(
  event = Events,
  n = Total,
  studlab = Author,
  data = table_SRSvisual,
  method = "Inverse",
  method.tau = "DL"
)

out_SRSvisual

out_SRSvisual_metareg_sample <- metareg(out_SRSvisual, sample)
out_SRSvisual_metareg_volume <- metareg(out_SRSvisual, volume)
out_SRSvisual_metareg_year <- metareg(out_SRSvisual, year5)
out_SRSvisual_metareg_optic_dose <- metareg(out_SRSvisual, optic_dose)

bubble(out_SRSvisual_metareg_volume,
       studlab = FALSE,
       xlab = "Mean Tumor Volume (cc)",
       ylab = "SRS New or Worsening of Visual Field Defect",
       col = "black",
       bg = "white",
       pch = 21,
       cex = 2,
       lwd = 2)

bubble(out_SRSvisual_metareg_sample,
       studlab = FALSE,
       xlab = "Sample Size",
       ylab = "SRS New or Worsening of Visual Field Defect",
       col = "black",
       bg = "white",
       pch = 21,
       cex = 2,
       lwd = 2)

bubble(out_SRSvisual_metareg_year,
       studlab = FALSE,
       xlab = "Publication Period (5-Year Epochs)",
       ylab = "SRS New or Worsening of Visual Field Defect",
       col = "black",
       bg = "white",
       pch = 21,
       cex = 2,
       lwd = 2)

bubble(out_SRSvisual_metareg_optic_dose,
       studlab = FALSE,
       xlab = "Maximum Dose to Optic Pathway (Gy)",
       ylab = "SRS New or Worsening of Visual Field Defect",
       col = "black",
       bg = "white",
       pch = 21,
       cex = 2,
       lwd = 2)


forest(out_SRSvisual,
       xlim = c(0.00, 0.4),
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
funnel(out_SRSvisual,
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

metabias(out_SRSvisual,
         method.bias = "Egger",
         k.min = 10,
         studlab = FALSE)

l1o_out_SRSvisual <- metainf(out_SRSvisual, pooled = "random")

forest(l1o_out_SRSvisual,
       col.square = "black",
       col.inside = "black",
       col.line = "black",
       col.square.lines = "black",
       col.diamond = "darkgray",
       col.diamond.lines.random = "darkgray",
       xlim = c(0.5, 2),
       rightcols = c("effect", "ci", "I2"))

baujat(out_SRSvisual,
       bg = "grey",
       studlab = FALSE,
       Xlim = c(0, 10))

labbe(out_SRSvisual,
      bg = "grey",
      studlab = TRUE,
      Xlim = c(0, 0.25),
      ylim = c(0, 0.5))

