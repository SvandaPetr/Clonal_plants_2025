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
install("ggplot2")

library("here")  
library("readxl")
library("usethis")
library("ggplot2")

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
# 2. clearing of NA ----
#----------------------------------------------------------#

koreny <- korinky[!is.na(korinky$weight_SRL),]


#----------------------------------------------------------#
# 3. Adding columns ----
#----------------------------------------------------------#

#column for change in number of leaves
koreny$diff_leaves <- koreny$leaves_main - koreny$leaves_before

#column for all the leaves grown? not sure if it will be useful, we have shoot biomass but idk
koreny$leaves_all <- koreny$diff_leaves + koreny$leaves_axillary + koreny$leaves_RS

#column for change in number of axillary rosettes
koreny$diff_axillary <- koreny$axillary_rosettes - koreny$axillary_rosettes_before

#column for change in number of RS
koreny$diff_RS_above <- koreny$RS_rosettes - koreny$above_RS_before

#column for change in number of belowground rootsprouts
koreny$diff_RS_below <- koreny$below_RS_in + koreny$below_RS_out - koreny$below_RS_before


#----------------------------------------------------------#
# 4. Analyses ----
#----------------------------------------------------------#


#---------------------------#
## 4.1 Basic differences between ploidy, no treatment in mind ----
#---------------------------#


#-----#
### 4.1.1 Ploidy and aboveground biomass ----
#-----#


# Want to have a look on the distribution
par(mfrow = c(2,2))
hist(koreny$weight_shoot)
hist(koreny$weight_shoot[koreny$sample_ploidy == "2x"])
hist(koreny$weight_shoot[koreny$sample_ploidy == "3x"])
hist(koreny$weight_shoot[koreny$sample_ploidy == "4x"])
par(mfrow = c(1,1)) #altogether kinda nice, by not so much by themself


# Still lets simply have a look on the differences
boxplot(koreny$weight_shoot ~ koreny$sample_ploidy)

model <- aov(koreny$weight_shoot ~ koreny$sample_ploidy)
anova(model)

#let´s check the diagnostic plots
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1)) #eeeh, i would try to do something with that, but i do not want to waste time with that because i dont believe in it anyway ->

TukeyHSD(model) #ok, but i did not consider the difference in size before planting -> we did not weigh them,                 so lets see if shoot_weight ~ number of leaves before planting is significant


# Check the effect of size before
model <- lm(koreny$weight_shoot ~ koreny$leaves_before)
anova(model) #yea, as expected

ggplot2::ggplot(data = koreny,
                aes(x = weight_shoot,
                    y = leaves_before, 
                    colour = sample_ploidy)) +
                geom_point() #maybe kinda funny difference between size on the start

#maybe kinda funny difference between size on the start
boxplot(koreny$leaves_before ~ koreny$sample_ploidy) #ye, and the shoot biomass copies that so nicely let´s move on


#-----#
### 4.1.1 Ploidy and belowground biomass ----
#-----#







