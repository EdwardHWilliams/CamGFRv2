

p1 <- Results_eGFR_Creattype %>% 
  mutate(Creatinine_type = ifelse(Creatinine_type == "IDMS", "IDMS", "Non-IDMS")) %>%
  ggplot(aes(y = RMSE, x = as.numeric(eGFR_cut), colour = equation)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~Creatinine_type) + 
  scale_colour_manual(values = tol6qualitative) + 
  ggplot_theme() + 
  scale_x_continuous(breaks = c(1,2,3), labels=c("1" = "<60", "2" = "60-90",
                            "3" = ">90")) +
  xlab("eGFR ranges [ml/min]") + 
  theme(panel.spacing.x=unit(1.5, "lines"))

dummy <- data.frame(stat = c("median", "RMSE"), x = c(0, NA))

p2 <- Results_eGFR_Creattype %>% 
  select(RMSE, median, equation, eGFR_cut, Creatinine_type) %>%
  gather(key = stat, value = value, -eGFR_cut, -equation, -Creatinine_type) %>%
  mutate(Creatinine_type = ifelse(Creatinine_type == "IDMS", "IDMS", "Non-IDMS")) %>%
  ggplot(aes(y = value, x = as.numeric(eGFR_cut), colour = equation)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~Creatinine_type) + 
  scale_colour_manual(values = tol6qualitative) + 
  ggplot_theme() + 
  scale_x_continuous(breaks = c(1,2,3), labels=c("1" = "<60", "2" = "60-90",
                                                 "3" = ">90")) +
  geom_hline(data = dummy, aes(yintercept = x), linetype = "dashed") + 
  xlab("eGFR ranges [ml/min]") + 
  theme(panel.spacing.x=unit(1.5, "lines"),
        strip.placement = "outside", 
        strip.background.y = element_blank(), 
        strip.text = element_text(size = rel(1))) + 
  facet_grid(stat~Creatinine_type, scales = "free", switch = "y") + 
  ylab("")


p2_2 <- Results_BSAadj_eGFR_Creattype %>% 
  select(RMSE, median, equation, eGFR_cut, Creatinine_type) %>%
  gather(key = stat, value = value, -eGFR_cut, -equation, -Creatinine_type) %>%
  mutate(Creatinine_type = ifelse(Creatinine_type == "IDMS", "IDMS", "Non-IDMS")) %>%
  ggplot(aes(y = value, x = as.numeric(eGFR_cut), colour = equation)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~Creatinine_type) + 
  scale_colour_manual(values = tol6qualitative) + 
  ggplot_theme() + 
  scale_x_continuous(breaks = c(1,2,3), labels=c("1" = "<60", "2" = "60-90",
                                                 "3" = ">90")) +
  geom_hline(data = dummy, aes(yintercept = x), linetype = "dashed") + 
  xlab("eGFR ranges [ml/min]") + 
  theme(panel.spacing.x=unit(1.5, "lines"),
        strip.placement = "outside", 
        strip.background.y = element_blank(), 
        strip.text = element_text(size = rel(1))) + 
  facet_grid(stat~Creatinine_type, scales = "free", switch = "y") + 
  ylab("")



dummy2 <- data.frame(stat = c("median", "RMSE", "P20", "IQR"), 
                     x = c(0, NA, NA, NA))

p3 <- Results_eGFR_Creattype %>% 
  select(RMSE, median, IQR, P20, equation, eGFR_cut, Creatinine_type) %>%
  gather(key = stat, value = value, -eGFR_cut, -equation, -Creatinine_type) %>%
  mutate(Creatinine_type = ifelse(Creatinine_type == "IDMS", "IDMS", "Non-IDMS")) %>%
  ggplot(aes(y = value, x = as.numeric(eGFR_cut), colour = equation)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~Creatinine_type) + 
  scale_colour_manual(values = tol6qualitative) + 
  ggplot_theme() + 
  scale_x_continuous(breaks = c(1,2,3), labels=c("1" = "<60", "2" = "60-90",
                                                 "3" = ">90")) +
  geom_hline(data = dummy2, aes(yintercept = x), linetype = "dashed") + 
  xlab("eGFR ranges [ml/min]") + 
  theme(panel.spacing.x=unit(1.5, "lines"),
        strip.placement = "outside", 
        strip.background.y = element_blank(), 
        strip.text = element_text(size = rel(1))) + 
  facet_grid(stat~Creatinine_type, scales = "free", switch = "y") + 
  ylab("")



ggsave("eGFR_comparison_plot_option3.pdf", plot = p3, 
       path = fig_export_dir,
       width = 20, height = 20/0.7, units = "cm")

ggsave("eGFR_comparison_plot_option2.pdf", plot = p2, 
       path = fig_export_dir,
       width = 20, height = 20/0.7/1.75, units = "cm")

ggsave("BSAadj_eGFR_comparison_plot_option2.pdf", plot = p2, 
       path = fig_export_dir,
       width = 20, height = 20/0.7/1.75, units = "cm")



ggsave("eGFR_comparison_plot_option1.pdf", plot = p2_2, 
       path = fig_export_dir,
       width = 20, height = 20/0.7/3.5, units = "cm")
