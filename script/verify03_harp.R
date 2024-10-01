#Example of verification process
#Load required libraries
library(harp)
library(dplyr)

wdir <- "/Users/wido/Documents/WORK/2024/harp/"
setwd(wdir)
sql_path <- paste0(wdir,"data/SQlite_tables/")
sql_path_obs <- paste0(sql_path,"OBS")
sql_path_fcst <- paste0(sql_path,"FCTables")
# load obs sqlite
t2m_obs <- read_point_obs(
  seq_dttm(2024010800, 2024011123), "T2m", 
  obs_path = sql_path_obs
)
# load fcst sqlite
t2m_fcst <- read_point_forecast(
  dttm             = seq_dttm(20240108, 20240111, "12h"),
  fcst_model       = c("wrfout"),
  fcst_type        = "det", 
  parameter        = "T2M",
  lead_time        = seq(0,72,3),
  by               = "3h",
  file_path        = sql_path_fcst,
  file_template    = "fctable_det",
  force_param_name = TRUE,
  drop_any_na      = FALSE
)

# joining file
t2m <- join_to_fcst(t2m_fcst, t2m_obs)
t2m <- mutate (scale_param(t2m,-273.15, "degC"),  # convert kelvin to celcius
               T2m = T2m - 273.15)
# scatter plot
plot_scatter(t2m,          # filename 
             "wrfout",     # model name
             T2m)         # column name in t2m file

# verify model
verif_con <- det_verify(t2m, T2m)
colnames(verif_con$det_summary_scores)
# plot
plot_point_verif(verif_con, bias)
plot_point_verif(verif_con, mae,
                 plot_num_cases = FALSE)
plot_point_verif(verif_con, stde)
plot_point_verif(verif_con, hexbin)
plot_point_verif(verif_con, 
                 score = hit_rate, 
                 facet_by =  vars(threshold), 
                 plot_num_cases = FALSE)
# categorical
verif_cat <- det_verify(t2m, T2m, thresholds = seq(15, 40, 5))
verif_cat
plot_point_verif(verif_cat, equitable_threat_score)

plot_point_verif(
  verif_cat, 
  hit_rate, 
  filter_by = vars(threshold == 25)
)

plot_point_verif(
  verif_cat, 
  hit_rate, 
  facet_by = vars(threshold)
)

plot_point_verif(
  verif_cat, 
  frequency_bias, 
  x_axis    = threshold,
  filter_by = vars(lead_time %in% seq(12, 48, 3)), 
  facet_by  = vars(lead_time)
)

plot_point_verif(
  verif_cat, 
  frequency_bias, 
  x_axis    = threshold,
  filter_by = vars(lead_time %in% 12), 
  facet_by  = vars(lead_time)
)
