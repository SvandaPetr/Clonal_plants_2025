here::here("data","Rhodopea_seedlings.xlsx")

korinky <- readxl::read_xlsx("data/Rhodopea_seedlings.xlsx", sheet = "harvest_data",
                             na = c("", NA), 
                             col_types = c("text", "numeric", "text", "numeric", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric", "text"),
                             col_names = c("samle_id", "plate_id", "position_id", "row", "column", "mother_id", "mother_ploidy", "sample_ploidy", "above_RS_before", "below_RS_before", "axillary_rosettes_before", "leaves_before", "treatment", "leaves_main", "leaves_axillary", "leaves_RS", "longest_leaf", "axillary_rosettes","RS_rosettes", "root_axes_main", "below_RS_in", "below_RS_out", "weight_root_in", "weight_root_out_noSRL","weight_root_out", "weight_shoot", "weight_SRL", "root_lenght_2_m", "SRL_m_g", "notes"),
                             skip = 1
)

koreny <- korinky[!is.na(korinky$weight_SRL),]
koreny$diff_leaves <- koreny$leaves_main - koreny$leaves_before
koreny$leaves_all <- koreny$diff_leaves + koreny$leaves_axillary + koreny$leaves_RS
koreny$diff_axillary <- koreny$axillary_rosettes - koreny$axillary_rosettes_before
koreny$diff_RS_above <- koreny$RS_rosettes - koreny$above_RS_before
koreny$RS_below <- koreny$below_RS_in + koreny$below_RS_out
koreny$root_all <- koreny$weight_root_in + koreny$weight_root_out
koreny$ratio_in_out <- koreny$below_RS_in/koreny$below_RS_out
koreny$root_shoot <- koreny$root_all/koreny$weight_shoot


#analýzy

#zjistit jestli IN a OUT je stejné množství kořenů (máme jenom biomasu na porovnání ne?), abychom vědělx jestli počet RS není ovlivněný čistě rozdílným množstvím kořenů. 

#porovnání biomasy kořenů IN a OUT pro každou ploidii
par(mfrow = c(1, 3))
for (ploidy in c("2x", "3x", "4x")) { 
  sub <- koreny[koreny$sample_ploidy == ploidy, ]
  boxplot(cbind(sub$weight_root_in, sub$weight_root_out),
          names = c("Root In", "Root Out"),
          col = c("steelblue", "tomato"),
          main = paste("Ploidy:", ploidy),
          ylab = "Weight")
}
par(mfrow = c(1, 1))

#porovnání biomasy kořenů IN a OUT pro treatment a každou ploidii
par(mfrow = c(2, 3)) 
for (treat in c("rim", "middle")) {
  for (ploidy in c("2x", "3x", "4x")) { 
    sub <- koreny[koreny$sample_ploidy == ploidy & koreny$treatment == treat, ]
    boxplot(cbind(sub$weight_root_in, sub$weight_root_out),
            names = c("Root In", "Root Out"),
            col = c("steelblue", "tomato"),
            main = paste(ploidy, treat),
            ylab = "Weight")
  }
}
par(mfrow = c(1, 1))

library(dplyr)
library(tidyr)
koreny_long <- koreny %>%
  select(samle_id, sample_ploidy, weight_root_in, weight_root_out, treatment) %>%
  pivot_longer(cols = c(weight_root_in, weight_root_out),
               names_to = "type",
               values_to = "weight")

model<-lm(weight~sample_ploidy*type, data=koreny_long)
anova(model)

#ano je mnohem víc kořenů OUT než IN (platí pro všechny ploidie)

