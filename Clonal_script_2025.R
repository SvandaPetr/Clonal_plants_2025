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

install.packages("here")
install.packages("readxl")
install.packages("usethis")
install.packages("ggplot2")
install.packages("tidyr")

library("here")  
library("readxl")
library("usethis")
library("ggplot2")
library("tidyverse")
library("tidyr")

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
koreny$RS_below <- koreny$below_RS_in + koreny$below_RS_out

#column for all the roots grown
koreny$root_all <- koreny$weight_root_in + koreny$weight_root_out

#column for RS_in_out ratio 
koreny$ratio_in_out <- koreny$below_RS_in/koreny$below_RS_out

#column for Root/Shoot

koreny$root_shoot <- koreny$root_all/koreny$weight_shoot

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

koreny$sample_ploidy <- as.factor(koreny$sample_ploidy)
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

#maybe kinda funny difference between size on the start
boxplot(koreny$leaves_before ~ koreny$sample_ploidy) #ye, and the shoot biomass copies that so nicely let´s move on


model <- lm(weight_shoot ~ sample_ploidy*leaves_before,
              data = koreny)
anova(model)

model <- lm(weight_shoot ~ sample_ploidy+leaves_before,
            data = koreny)
anova(model)

#leaves before vezou všechno
model <- lm(weight_shoot ~ leaves_before+sample_ploidy,
            data = koreny)
anova(model)


#vysledná váha se liší podle velikosti na začátku

#-----#
### 4.1.2 Ploidy and belowground biomass ----
#-----#


par(mfrow = c(2,2))
hist(koreny$root_all)
hist(koreny$root_all[koreny$sample_ploidy == "2x"])
hist(koreny$root_all[koreny$sample_ploidy == "3x"])
hist(koreny$root_all[koreny$sample_ploidy == "4x"])
par(mfrow = c(1,1)) #altogether kinda not nice


# Still lets simply have a look on the differences
boxplot(koreny$root_all ~ koreny$sample_ploidy)

model <- lm(koreny$root_all ~ koreny$sample_ploidy)
anova(model)
summary(model)

model <- lm(root_all ~ leaves_before, data = koreny)
anova(model)

model <- lm(koreny$root_all ~ koreny$sample_ploidy*koreny$leaves_before)
anova(model)

model <- lm(koreny$root_all ~ koreny$sample_ploidy+koreny$leaves_before)
anova(model)

#leaves before vezou všechno
model <- lm(koreny$root_all ~ koreny$leaves_before+koreny$sample_ploidy)
anova(model)


#u kořenů stejně tak

#-----#
### 4.1.3 Ploidy and eoot/shoot ----
#-----#

model <- aov(koreny$root_shoot ~ koreny$sample_ploidy)
anova(model)

#-----#
### 4.1.4 ploidy ~ root lenght ----
#-----#

model <- lm(koreny$root_lenght_2_m ~ koreny$sample_ploidy)
anova(model)
summary(model)


model <- lm(koreny$root_lenght_2_m ~ koreny$leaves_before)
anova(model)

model <- lm(koreny$root_lenght_2_m ~ koreny$sample_ploidy*koreny$leaves_before)
anova(model)

model <- lm(koreny$root_lenght_2_m ~ koreny$sample_ploidy+koreny$leaves_before)
anova(model)

#opět, leaves before žerou všechno
model <- lm(koreny$root_lenght_2_m ~ koreny$leaves_before+koreny$sample_ploidy)
anova(model)


#-----#
### 4.1.5 ploidy ~ SRL ----
#-----#

model <- aov(koreny$SRL_m_g ~ koreny$sample_ploidy)
anova(model)
TukeyHSD(model)

#-----#
### 4.1.6 ploidy ~ axilary ----
#-----#

model <- aov(koreny$axillary_rosettes ~ koreny$sample_ploidy)
anova(model)
TukeyHSD(model)

#-----#
### 4.1.7 ploidy ~ longest ----
#-----#

model <- aov(koreny$longest_leaf ~ koreny$sample_ploidy)
anova(model)
TukeyHSD(model)

#-----#
### 4.1.8 treatment ~ cokoli ----
#-----#

model <- aov(koreny$leaves_before ~ koreny$treatment)
anova(model)

model <- aov(koreny$root_all ~ koreny$treatment)
anova(model)

model <- aov(koreny$SRL_m_g ~ koreny$treatment)
anova(model)

model <- aov(koreny$root_lenght_2_m ~ koreny$treatment)
anova(model)

model <- aov(koreny$root_axes_main ~ koreny$treatment)
anova(model)

model <- aov(koreny$leaves_all ~ koreny$treatment)
anova(model) #aaaha?

model <- aov(koreny$axillary_rosettes ~ koreny$treatment)
anova(model)
model <- aov(koreny$diff_axillary ~ koreny$treatment)
anova(model) #aaaaha?

model <- aov(koreny$leaves_axillary ~ koreny$treatment)
anova(model)

model <- aov(koreny$root_shoot ~ koreny$treatment)
anova(model)

#-----#
### 4.1.8 root_lenght ~ axes ----
#-----#

model <- lm(koreny$root_lenght_2_m ~ koreny$root_axes_main)
anova(model)

hist(koreny$root_lenght_2_m)
hist(log(koreny$root_lenght_2_m))

model <- lm(log(koreny$root_lenght_2_m) ~ koreny$root_axes_main)
anova(model) #aaaha
model_primka <- model
plot(log(koreny$root_lenght_2_m) ~ koreny$root_axes_main)
abline(model_primka, col = "red")

#-----#
### 4.1.8 root_weight ~ axes ----
#-----#

model <- lm(koreny$root_all ~ koreny$root_axes_main)
anova(model)
model_primka <- model
plot(koreny$root_all ~ koreny$root_axes_main)
abline(model_primka, col = "red")


#-----#
### 4.1.8 leaves_before ~ axes ----
#-----#

model <- lm(koreny$leaves_before ~ koreny$root_axes_main)
anova(model)
model_primka <- model
plot(koreny$leaves_before ~ koreny$root_axes_main)
abline(model_primka, col = "red")

#takže jen, že větší kytky dělají více axis

#---------------------------#
## 4.2 variability ----
#---------------------------#

#MIDDLE

koreny_clear <- drop_na(koreny, root_axes_main, RS_below, below_RS_in, below_RS_out)


cv_root_weight_middle <- sd(koreny_clear$root_all[koreny_clear$treatment == "middle"])/
  mean(koreny_clear$root_all[koreny_clear$treatment == "middle"]) * 100

cv_shoot_weight_middle <- sd(koreny_clear$weight_shoot[koreny_clear$treatment == "middle"])/
  mean(koreny_clear$weight_shoot[koreny_clear$treatment == "middle"]) * 100

cv_root_lenght_middle <- sd(koreny_clear$root_lenght_2_m[koreny_clear$treatment == "middle"])/
  mean(koreny_clear$root_lenght_2_m[koreny_clear$treatment == "middle"]) * 100

cv_SRL_middle <- sd(koreny_clear$SRL_m_g[koreny_clear$treatment == "middle"])/
  mean(koreny_clear$SRL_m_g[koreny_clear$treatment == "middle"]) * 100

cv_rossets_middle <- sd(koreny_clear$diff_axillary[koreny_clear$treatment == "middle"])/
  mean(koreny_clear$diff_axillary[koreny_clear$treatment == "middle"]) * 100

cv_axes_middle <- sd(koreny_clear$root_axes_main[koreny_clear$treatment == "middle"])/
  mean(koreny_clear$root_axes_main[koreny_clear$treatment == "middle"]) * 100

cv_RS_in_middle <- sd(koreny_clear$below_RS_in[koreny_clear$treatment == "middle"])/
  mean(koreny_clear$below_RS_in[koreny_clear$treatment == "middle"]) * 100

cv_RS_out_middle <- sd(koreny_clear$below_RS_out[koreny_clear$treatment == "middle"])/
  mean(koreny_clear$below_RS_out[koreny_clear$treatment == "middle"]) * 100

cv_RS_all_middle <- sd(koreny_clear$RS_below[koreny_clear$treatment == "middle"])/
  mean(koreny_clear$RS_below[koreny_clear$treatment == "middle"]) * 100

#RIM

cv_root_weight_rim <- sd(koreny_clear$root_all[koreny_clear$treatment == "rim"])/
  mean(koreny_clear$root_all[koreny_clear$treatment == "rim"]) * 100

cv_shoot_weight_rim <- sd(koreny_clear$weight_shoot[koreny_clear$treatment == "rim"])/
  mean(koreny_clear$weight_shoot[koreny_clear$treatment == "rim"]) * 100

cv_root_lenght_rim <- sd(koreny_clear$root_lenght_2_m[koreny_clear$treatment == "rim"])/
  mean(koreny_clear$root_lenght_2_m[koreny_clear$treatment == "rim"]) * 100

cv_SRL_rim <- sd(koreny_clear$SRL_m_g[koreny_clear$treatment == "rim"])/
  mean(koreny_clear$SRL_m_g[koreny_clear$treatment == "rim"]) * 100

cv_rossets_rim <- sd(koreny_clear$diff_axillary[koreny_clear$treatment == "rim"])/
  mean(koreny_clear$diff_axillary[koreny_clear$treatment == "rim"]) * 100

cv_axes_rim <- sd(koreny_clear$root_axes_main[koreny_clear$treatment == "rim"])/
  mean(koreny_clear$root_axes_main[koreny_clear$treatment == "rim"]) * 100

cv_RS_in_rim <- sd(koreny_clear$below_RS_in[koreny_clear$treatment == "rim"])/
  mean(koreny_clear$below_RS_in[koreny_clear$treatment == "rim"]) * 100

cv_RS_out_rim <- sd(koreny_clear$below_RS_out[koreny_clear$treatment == "rim"])/
  mean(koreny_clear$below_RS_out[koreny_clear$treatment == "rim"]) * 100

cv_RS_all_rim <- sd(koreny_clear$RS_below[koreny_clear$treatment == "rim"])/
  mean(koreny_clear$RS_below[koreny_clear$treatment == "rim"]) * 100

cv_data <- data.frame(
  Promenna = rep(c("Root weight", "Shoot weight", "Root length", "SRL", 
                   "Rossets", "Axes", "RS in", "RS out", "RS all"), times = 2),
  
  Treatment = rep(c("middle", "rim"), each = 9),
  CV_hodnota = c(
    # Hodnoty pro middle
    cv_root_weight_middle, cv_shoot_weight_middle, cv_root_lenght_middle, 
    cv_SRL_middle, cv_rossets_middle, cv_axes_middle, cv_RS_in_middle, 
    cv_RS_out_middle, cv_RS_all_middle,
    
    # Hodnoty pro rim
    cv_root_weight_rim, cv_shoot_weight_rim, cv_root_lenght_rim, 
    cv_SRL_rim, cv_rossets_rim, cv_axes_rim, cv_RS_in_rim, 
    cv_RS_out_rim, cv_RS_all_rim
  )
)


ggplot(cv_data, aes(x = Promenna, y = CV_hodnota, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = round(CV_hodnota, 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_fill_manual(values = c("middle" = "#1b9e77", "rim" = "#d95f02")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  labs(
    title = "Variační koeficienty podle treatmentu",
    x = "Měřená proměnná",
    y = "Variační koeficient (%)",
    fill = "Treatment"
  )

#-----#
# Predchozi veci ----
#-----#

#-----#
### 4.1.3 treatment and belowground biomass ----
#-----#


# Still lets simply have a look on the differences
boxplot(koreny$root_all ~ koreny$treatment + koreny$sample_ploidy)

model <- aov(koreny$weight_shoot ~ koreny$treatment*koreny$leaves_before)
anova(model)


#-----#
### 4.1.3 treatment and sprouts ----
#-----#

# Still lets simply have a look on the differences
boxplot(koreny$RS_below ~ koreny$treatment)

model <- aov(koreny$RS_below ~ koreny$treatment)
anova(model)


#-----#
### 4.1.3 treatment and sprouts in n out ----
#-----#

#odtvorba diploidů
koreny_bez_2<- koreny[koreny$sample_ploidy!="2x",]

# Still lets simply have a look on the differences
boxplot(koreny_bez_2$below_RS_in ~ koreny_bez_2$treatment + koreny_bez_2$sample_ploidy)
boxplot(koreny_bez_2$below_RS_in ~ koreny_bez_2$treatment)
model <- aov(koreny$below_RS_in ~ koreny$treatment)
anova(model)


# Still lets simply have a look on the differences
boxplot(koreny_bez_2$below_RS_out ~ koreny_bez_2$treatment + koreny_bez_2$sample_ploidy)
boxplot(koreny_bez_2$below_RS_out ~ koreny_bez_2$treatment)
model <- aov(koreny$below_RS_out ~ koreny$treatment)
anova(model)

# Still lets simply have a look on the differences
boxplot(koreny_bez_2$ratio_in_out ~ koreny_bez_2$treatment + koreny_bez_2$sample_ploidy)
boxplot(koreny_bez_2$ratio_in_out ~ koreny_bez_2$treatment)
model <- aov(koreny_bez_2$ratio_in_out ~ koreny_bez_2$treatment)
anova(model)

data_sprouts_long <- koreny_bez_2 %>%
  pivot_longer(
    cols = c(below_RS_in, below_RS_out), 
    names_to = "Location_Type",                             # New column for the names ('Number_of_Sprouts_In', 'Number_of_Sprouts_Out')
    values_to = "Sprout_Count"                               # New column for the values (the sprout counts)
  ) %>%
  # Clean up the 'Location_Type' column to just "In" or "Out"
  mutate(Location = gsub("Number_of_Sprouts_", "", Location_Type)) %>%
  select(-Location_Type) # Remove the original raw pivoted column

print(data_sprouts_long)



ggplot(data_sprouts_long, aes(x = treatment, y = Sprout_Count, fill = Location)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ sample_ploidy, scales = "free_x") + # Creates separate plots for '3x' and '4x'
  labs(
    title = "Number of Sprouts by Category, Treatment, and Location",
    x = "Treatment Group",
    y = "Number of Sprouts",
    fill = "Sprout Location" # Legend title
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.spacing = unit(2, "lines"), # Space between facets
    plot.title = element_text(hjust = 0.5) # Center plot title
  )


m1<-aov(data_sprouts_long$Sprout_Count~data_sprouts_long$sample_ploidy+Error(data_sprouts_long$Location))
summary(m1)

m2<-aov(data_sprouts_long$Sprout_Count~data_sprouts_long$treatment+Error(data_sprouts_long$Location))
summary(m2)


m<-aov(data_sprouts_long$Sprout_Count~data_sprouts_long$sample_ploidy*data_sprouts_long$treatment+Error(data_sprouts_long$Location))
summary(m)