# Exploratory Data Analysis ----

basic_eda <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}