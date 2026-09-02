# loaded packages

library(readxl)
library(writexl)
library(estmeansd)

# imported tumor volume data

tumor_volume <- read_excel("calc_mean.xlsx")

# created output variables

tumor_volume$mean_used <- NA_real_
tumor_volume$sd_used <- NA_real_
tumor_volume$method <- NA_character_

# converted reported summary statistics

for(i in seq_len(nrow(tumor_volume))){
  
  n <- as.numeric(tumor_volume$n[i])
  
  mean <- as.numeric(tumor_volume$Mean[i])
  sd <- as.numeric(tumor_volume$SD[i])
  
  median <- as.numeric(tumor_volume$Median[i])
  
  q1 <- as.numeric(tumor_volume$Q1[i])
  q3 <- as.numeric(tumor_volume$Q3[i])
  
  min <- as.numeric(tumor_volume$Min[i])
  max <- as.numeric(tumor_volume$Max[i])
  
  if(!is.na(mean)){
    
    tumor_volume$mean_used[i] <- mean
    tumor_volume$sd_used[i] <- sd
    tumor_volume$method[i] <- "original"
    
    next
    
  }
  
  estimate <- tryCatch(
    
    qe.mean.sd(
      
      min.val = min,
      q1.val = q1,
      med.val = median,
      q3.val = q3,
      max.val = max,
      n = n
      
    ),
    
    error = function(e) NULL
    
  )
  
  if(!is.null(estimate)){
    
    tumor_volume$mean_used[i] <- estimate$est.mean
    tumor_volume$sd_used[i] <- estimate$est.sd
    tumor_volume$method[i] <- "mcgrath"
    
  }else{
    
    tumor_volume$mean_used[i] <- NA
    tumor_volume$sd_used[i] <- NA
    tumor_volume$method[i] <- "not convertible"
    
  }
  
}

# diirectly exported converted data to avoid copy errors for the meta-regression analysis

write_xlsx(
  tumor_volume,
  "tumor_volume_converted.xlsx"
)