
test = read_dta("C:/Users/hadunka2/Box/01. Zambia Deforestation/Data_Vinni.dta")

test = 
  test %>%
  group_by(HHID) %>%
  mutate(first_year_l_FAW_1 = ifelse(army_aff == 1, year, NA)) %>%
  mutate(first_year_l_FAW_1 = min(first_year_l_FAW_1, na.rm = TRUE)) %>%
  ungroup()

summary(
  lm_robust(Charc ~ 
              l_FAW + l_rainfall + s_lrainfall,
            fixed_effects = ~ year + HHID,
            data = test,
            clusters = HHID
  ))

aggte(
  att_gt(
    yname = "Charc",
    tname = "year",
    xformla = ~ l_rainfall,
    gname = "first_year_l_FAW_1",
    data = test,
    #control_group= c("notyettreated"),
    panel = FALSE,
    clustervars = "HHID"
  ),
  type = "dynamic", na.rm = T)


install.packages("ggplot2")
install.packages("dplyr")
install.packages("RColorBrewer")
install.packages("fixest")

library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(fixest)


data_plot <- event_study(
  data = test,
  yname = "Charc",
  idname = "HHID",
  tname = "year",
  gname = "first_year_l_FAW_1",
  xformla = ~ l_rainfall
)

data_plot = as.data.frame(out) %>%  mutate(CI_Lower = estimate - 1.96 * std.error ,
                                           CI_Upper = estimate + 1.96 * std.error,
                                           estimator = as.factor(estimator))
plot <- ggplot(data_plot, aes(x = estimate, y = term, xmin = CI_Lower, xmax = CI_Upper,
                              color = estimator)) +
  geom_pointrange(position = position_dodge(width = 0.6), alpha =0.9) +
  geom_hline(yintercept = -0.5, linetype = "dashed", color = "black", size =1) +
  geom_vline(xintercept =  0, linetype = "longdash", color = "black", size =0.6) +
  coord_flip() +theme_minimal() +   theme(axis.text=element_text(size=12),
                                          axis.title=element_text(size=12,face="bold"),
                                          legend.position = "bottom") + scale_color_brewer(palette="Paired") +
  labs(x = "Point Estimates and 95% Confidence Intervals",
       y = "Event Time",
       color = "") + xlim(-0.8, 0.6)
plot + scale_y_continuous(limits = c(-3, 3), breaks = seq(-3,3,1)) +
  scale_x_continuous(limits = c(-0.8, 0.8), breaks = seq(-0.8,0.8,0.4))