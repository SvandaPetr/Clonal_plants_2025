#----------------------------------------------------------#
#
#
#                   Clonal Plants script
#                         2025
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Install and load packages ----
#----------------------------------------------------------#

install("here")
install("readxl")
install("usethis")

library("here")  
library("readxl")
library("usethis")

#----------------------------------------------------------#
# 1. Import dataset ----
#----------------------------------------------------------#

here::here("data","Rhodopea_seedlings.xlsx")

korinky <- readxl::read_xlsx("data/Rhodopea_seedlings.xlsx", sheet = "harvest_data",
                              na = c("", NA), 
                              col_types = c("text", "numeric", "text", "numeric", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric", "text"),
                              col_names = c("samle_id", "plate_id", "position_id", "row", "column", "mother_id", "mother_ploidy", "sample_ploidy", "above_RS_before", "below_RS_before", "axillary_rosettes_before", "leaves_before", "treatment", "leaves_main", "leaves_axillary", "leaves_RS", "longest_leaf", "axillary_rosettes","RS_rosettes", "root_axes_main", "below_RS_in", "below_RS_out", "weight_root_in", "weight_root_out_noSRL","weight_root_out", "weight_shoot", "weight_SRL", "root_lenght_2_m", "SRL_m_g", "notes"),
                              skip = 1
                             )


#----------------------------------------------------------#
# 2. Analyses ----
#----------------------------------------------------------#



