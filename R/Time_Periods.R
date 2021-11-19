# Attribute TDAT_LMF for comparison of two time periods
attribute_time <- function(TDAT_LMF, time_period1, time_period2){
  t1 <- cbind(Year = time_period1, Time_Period = rep("2011-2015", 5))
  t2 <- cbind(Year = time_period2,  Time_Period = rep("2016-2020", 5))
  time_period_table <- rbind(t1,t2)
  
  output <-  merge(x = TDat_LMF,
                   y = time_period_table,
                   by = "Year",
                   all.x = TRUE)
  return(output)
}


