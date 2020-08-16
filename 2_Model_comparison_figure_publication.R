library(cowplot)

################################################################################
# Plot pulbised and refit split by centre and creatinien type
################################################################################

Results_plot <- Results_Centre_CreatType %>% 
  ungroup() %>%
  mutate(equation = factor(equation,
                           levels = c("Refit_CamGFR", "CamGFR_v1", "LM", "CKD-EPI", "FAS", "MDRD"),
                           labels = c("CamGFR v2", "CamGFR v1", "Lund-Malmo", "CKD-EPI", "FAS", "MDRD"))) %>%
  mutate(equ_centre = interaction(Centre, equation))


p1 <- Results_plot %>% 
  ggplot(aes_string(x = "Creatinine_type", y = "RMSE", group = "equ_centre")) + 
  geom_errorbar(aes_string(ymin = "RMSE_lwr", ymax = "RMSE_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 3, colour = "black", fill = "white", pch=21) + 
  geom_point(aes_string(shape = "Centre"), position= position_dodge(width = .5), size = 1.8) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank()) +
  scale_color_manual(values =tol6qualitative) +
  scale_shape_manual(values = c("C", "G", "M")) +
  ylab("RMSE\n(Accuracy)") + 
  theme(text = element_text(size = 14))

p2 <- Results_plot %>% 
  ggplot(aes_string(group = "equ_centre", x = "Creatinine_type", y = "median")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes_string(ymin = "median_lwr", ymax = "median_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 3, colour = "black", fill = "white", pch=21) + 
  geom_point(aes_string(shape = "Centre"), position= position_dodge(width = .5), size = 1.8) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank()) +
  scale_color_manual(values =tol6qualitative) +
  scale_shape_manual(values = c("C", "G", "M")) +
  ylab("Residual median\n(Bias)") + 
  theme(text = element_text(size = 14))

p3 <- Results_plot %>% 
  ggplot(aes_string(group = "equ_centre", x = "Creatinine_type", y = "IQR")) + 
  geom_errorbar(aes_string(ymin = "IQR_lwr", ymax = "IQR_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 3, colour = "black", fill = "white", pch=21) + 
  geom_point(aes_string(shape = "Centre"), position= position_dodge(width = .5), size = 1.8) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank()) + 
  scale_color_manual(values =tol6qualitative) +
  scale_shape_manual(values = c("C", "G", "M")) +
  ylab("Residual IQR\n(Precision)") 

p4 <- Results_plot %>% 
  ggplot(aes_string(group = "equ_centre", x = "Creatinine_type", y = "P20")) + 
  geom_errorbar(aes_string(ymin = "P20_lwr", ymax = "P20_upr", 
                           colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 3, colour = "black", fill = "white", pch=21) + 
  geom_point(aes_string(shape = "Centre"), position= position_dodge(width = .5), size = 1.8) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank())  +
  scale_color_manual(values =tol6qualitative) +
  scale_shape_manual(values = c("C", "G", "M")) +
  ylab("1 - P20\n(Clinical robustness)") +
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(), 
        text = element_text(size = 14), 
        legend.title = element_blank(), 
        legend.text = element_text(size = rel(1.2)),
        legend.box = "vertical",
        legend.spacing.x = unit(0.5, "cm")) 


plot_creat_centre <- 
  rbind(ggplotGrob(p2), 
        ggplotGrob(p3), 
        ggplotGrob(p1), 
        ggplotGrob(p4), 
        size = "last")

ggsave(paste0(fig_export_dir, "Compare_equations_plot.pdf"), plot_creat_centre,
       width = 8, height = 10)

################################################################################
# Plot pulbised and refit split by centre, only IDMS
################################################################################

Results_plot_IDMS <- Results_Centre_CreatType %>% 
  bind_rows(mutate(Results_CreatType, Centre = "All")) %>%
  filter(Creatinine_type == "IDMS") %>%
  ungroup() %>%
  mutate(equ_centre = interaction(Centre, equation))


p1 <- Results_plot_IDMS %>% 
  ggplot(aes_string(x = "Centre", y = "RMSE", group = "equ_centre")) + 
  geom_errorbar(aes_string(ymin = "RMSE_lwr", ymax = "RMSE_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 2) + #, colour = "black", fill = "white", pch=21) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank()) +
  scale_color_manual(values =tol6qualitative) +
  ylab("RMSE\n(Accuracy)") + 
  theme(text = element_text(size = 14))

p2 <- Results_plot_IDMS %>% 
  ggplot(aes_string(group = "equ_centre", x = "Centre", y = "median")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes_string(ymin = "median_lwr", ymax = "median_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 2) + #, colour = "black", fill = "white", pch=21) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank()) +
  scale_color_manual(values =tol6qualitative) +
  ylab("Residual median\n(Bias)") + 
  theme(text = element_text(size = 14))

p3 <- Results_plot_IDMS %>% 
  ggplot(aes_string(group = "equ_centre", x = "Centre", y = "IQR")) + 
  geom_errorbar(aes_string(ymin = "IQR_lwr", ymax = "IQR_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 2) + #, colour = "black", fill = "white", pch=21) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank()) + 
  scale_color_manual(values =tol6qualitative) +
  ylab("Residual IQR\n(Precision)") 

p4 <- Results_plot_IDMS %>% 
  ggplot(aes_string(group = "equ_centre", x = "Centre", y = "P20")) + 
  geom_errorbar(aes_string(ymin = "P20_lwr", ymax = "P20_upr", 
                           colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 2) + #, colour = "black", fill = "white", pch=21) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank())  +
  scale_color_manual(values =tol6qualitative) +
  ylab("1 - P20\n(Clinical robustness)") +
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(), 
        text = element_text(size = 14), 
        legend.title = element_blank(), 
        legend.text = element_text(size = rel(1.2)),
        legend.box = "vertical",
        legend.spacing.x = unit(0.5, "cm")) 


plot_creat_centre_IDMS <- 
  rbind(ggplotGrob(p2), 
        ggplotGrob(p3), 
        ggplotGrob(p1), 
        ggplotGrob(p4), 
        size = "last")

ggsave(paste0(fig_export_dir, "Compare_equations_plot_IDMS.pdf"), plot_creat_centre,
       width = 9.1, height = 10)


################################################################################
# Plot pulbised and refit split by patient type, only IDMS
################################################################################


Results_plot_IDMS <- Results_PatientType %>% 
  bind_rows(mutate(Results_CreatType, Patient_type = "All")) %>%
  filter(Creatinine_type == "IDMS") %>%
  ungroup() %>%
  mutate(equ_Patient_type = interaction(Patient_type, equation))




p1 <- Results_plot_IDMS %>% 
  ggplot(aes_string(x = "Patient_type", y = "RMSE", group = "equ_Patient_type")) + 
  geom_errorbar(aes_string(ymin = "RMSE_lwr", ymax = "RMSE_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 2) + #, colour = "black", fill = "white", pch=21) + 
  # geom_point(aes_string(shape = "Patient_type"), position= position_dodge(width = .5), size = 1.8) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank()) +
  scale_color_manual(values =tol6qualitative) +
  # scale_shape_manual(values = c("A", "C", "G", "M")) +
  ylab("RMSE\n(Accuracy)") + 
  theme(text = element_text(size = 14))

p2 <- Results_plot_IDMS %>% 
  ggplot(aes_string(group = "equ_Patient_type", x = "Patient_type", y = "median")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes_string(ymin = "median_lwr", ymax = "median_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 2) + #, colour = "black", fill = "white", pch=21) + 
  # geom_point(aes_string(shape = "Patient_type"), position= position_dodge(width = .5), size = 1.8) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank()) +
  scale_color_manual(values =tol6qualitative) +
  # scale_shape_manual(values = c("C", "G", "M")) +
  ylab("Residual median\n(Bias)") + 
  theme(text = element_text(size = 14))

p3 <- Results_plot_IDMS %>% 
  ggplot(aes_string(group = "equ_Patient_type", x = "Patient_type", y = "IQR")) + 
  geom_errorbar(aes_string(ymin = "IQR_lwr", ymax = "IQR_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 2) + #, colour = "black", fill = "white", pch=21) + 
  # geom_point(aes_string(shape = "Patient_type"), position= position_dodge(width = .5), size = 1.8) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank()) + 
  scale_color_manual(values =tol6qualitative) +
  # scale_shape_manual(values = c("C", "G", "M")) +
  ylab("Residual IQR\n(Precision)") 

p4 <- Results_plot_IDMS %>% 
  ggplot(aes_string(group = "equ_Patient_type", x = "Patient_type", y = "P20")) + 
  geom_errorbar(aes_string(ymin = "P20_lwr", ymax = "P20_upr", 
                           colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 2) + #, colour = "black", fill = "white", pch=21) + 
  # geom_point(aes_string(shape = "Patient_type"), position= position_dodge(width = .5), size = 1.8) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank())  +
  scale_color_manual(values =tol6qualitative) +
  scale_shape_manual(values = c("C", "G", "M")) +
  ylab("1 - P20\n(Clinical robustness)") +
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(), 
        text = element_text(size = 14), 
        legend.title = element_blank(), 
        legend.text = element_text(size = rel(1.2)),
        legend.box = "vertical",
        legend.spacing.x = unit(0.5, "cm")) 


plot_creat_Patient_type_IDMS <- 
  rbind(ggplotGrob(p2), 
        ggplotGrob(p3), 
        ggplotGrob(p1), 
        ggplotGrob(p4), 
        size = "last")



ggsave(paste0(fig_export_dir, "Compare_equations_plot_IDMS_patient_type.pdf"), plot_creat_Patient_type_IDMS,
       width = 9.1, height = 10)








################################################################################
# Plot pulbised and refit split by age and creatinien type
################################################################################


plot_data <- Results_Age_IDMS

p1 <- plot_data %>% 
  ggplot(aes_string(group = "equation", x = "Age_cut", y = "RMSE")) + 
  geom_errorbar(aes_string(ymin = "RMSE_lwr", ymax = "RMSE_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = tol6qualitative) +
  ylab("RMSE\n(Accuracy)") + 
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())



p2 <- plot_data %>% 
  ggplot(aes_string(group = "equation", x = "Age_cut", y = "median")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes_string(ymin = "median_lwr", ymax = "median_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  ylab("Residual median\n(Bias)") + 
  scale_color_manual(values = tol6qualitative) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())

p3 <- plot_data %>% 
  ggplot(aes_string(group = "equation", x = "Age_cut", y = "IQR")) + 
  geom_errorbar(aes_string(ymin = "IQR_lwr", ymax = "IQR_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = tol6qualitative) +
  ylab("Residual IQR\n(Precision)") + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())


p4 <- plot_data %>% 
  ggplot(aes_string(group = "equation", x = "Age_cut", y = "P20")) + 
  geom_errorbar(aes_string(ymin = "P20_lwr", ymax = "P20_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = tol6qualitative) +
  ylab("1 - P20\n(Clinical robustness)") +
  theme(legend.position = "bottom", 
        text = element_text(size = 14), 
        legend.title = element_blank(), 
        legend.text = element_text(size = rel(1.2)),
        legend.spacing.x = unit(0.5, "cm")) + 
  xlab("Age [years]") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_age <- rbind(ggplotGrob(p2), 
                  ggplotGrob(p3), 
                  ggplotGrob(p1), 
                  ggplotGrob(p4), 
                  size = "last")


ggsave(paste0(fig_export_dir, "Compare_equations_IDMS_Age.pdf"), plot_age,
       width = 13, height = 10)


################################################################################
# Plot pulbised and refit split by estimated GFR and creatinien type
################################################################################


plot_data <- Results_eGFR_Creattype %>%
  filter(Creatinine_type == "IDMS")

p1 <- plot_data %>% 
  ggplot(aes_string(group = "equation", x = "eGFR_cut", y = "RMSE")) + 
  geom_errorbar(aes_string(ymin = "RMSE_lwr", ymax = "RMSE_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = tol6qualitative) +
  ylab("RMSE\n(Accuracy)") + 
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())



p2 <- plot_data %>% 
  ggplot(aes_string(group = "equation", x = "eGFR_cut", y = "median")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes_string(ymin = "median_lwr", ymax = "median_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  ylab("Residual median\n(Bias)") + 
  scale_color_manual(values = tol6qualitative) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())

p3 <- plot_data %>% 
  ggplot(aes_string(group = "equation", x = "eGFR_cut", y = "IQR")) + 
  geom_errorbar(aes_string(ymin = "IQR_lwr", ymax = "IQR_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = tol6qualitative) +
  ylab("Residual IQR\n(Precision)") + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())


p4 <- plot_data %>% 
  ggplot(aes_string(group = "equation", x = "eGFR_cut", y = "P20")) + 
  geom_errorbar(aes_string(ymin = "P20_lwr", ymax = "P20_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = tol6qualitative) +
  ylab("1 - P20\n(Clinical robustness)") +
  theme(legend.position = "bottom", 
        text = element_text(size = 14), 
        legend.title = element_blank(), 
        legend.text = element_text(size = rel(1.2)),
        legend.spacing.x = unit(0.5, "cm")) + 
  xlab("eGFR [ml/min]") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_eGFR <- rbind(ggplotGrob(p2), 
                  ggplotGrob(p3), 
                  ggplotGrob(p1), 
                  ggplotGrob(p4), 
                  size = "last")


ggsave(paste0(fig_export_dir, "Compare_equations_IDMS_eGFR.pdf"), plot_eGFR,
       width = 13, height = 10)








################################################################################
# Plot all split by creatinine type
################################################################################

Results_plot_2 <- Results_CreatType_all %>% 
  ungroup() %>%
  # filter(!(equation %in% c("Spline_model_age", "Spline_model_creat"))) %>%
  mutate(equation = factor(equation, 
                           levels = c("Spline_model_age", 
                                      "Spline_model_age_creat",
                                      "Spline_model_creat",
                                      "BIC_model",
                                      "Leave_out_1_model", 
                                      "Leave_out_n_model",
                                      "Refit_CamGFR", "LM",
                                      "CKD-EPI",  "FAS", "MDRD" ), 
                           labels = c("Spline model: Age", 
                                      "Spline model: Age, Creat",
                                      "Spline model: Creat",
                                      "BIC",
                                      "Leave-out-1", 
                                      "Leave-out-n", 
                                      "Refit CamGFR", 
                                      "Lund-Malmo", 
                                      "CKD-EPI", "FAS", "MDRD")))

p1 <- Results_plot_2 %>% 
  ggplot(aes_string(group = "equation", x = "Creatinine_type", y = "RMSE")) + 
  geom_errorbar(aes_string(ymin = "RMSE_lwr", ymax = "RMSE_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = tol11qualitative) +
  ylab("RMSE\n(Accuracy)") + 
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())

p2 <- Results_plot_2 %>% 
  ggplot(aes_string(group = "equation", x = "Creatinine_type", y = "median")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes_string(ymin = "median_lwr", ymax = "median_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = tol11qualitative) +
  ylab("Residual median\n(Bias)") + 
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())

p3 <- Results_plot_2 %>% 
  ggplot(aes_string(group = "equation", x = "Creatinine_type", y = "IQR")) + 
  geom_errorbar(aes_string(ymin = "IQR_lwr", ymax = "IQR_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = tol11qualitative) +
  ylab("Residual IQR\n(Precision)")  + 
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())

p4 <- Results_plot_2 %>% 
  ggplot(aes_string(group = "equation", x = "Creatinine_type", y = "P20")) + 
  geom_errorbar(aes_string(ymin = "P20_lwr", ymax = "P20_upr", 
                           colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank()) +
  scale_color_manual(values = tol11qualitative) +
  ylab("1 - P20\n(Dose accuracy)") +
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(), 
        text = element_text(size = 14), 
        legend.title = element_blank(), 
        legend.text = element_text(size = rel(1.2)),
        legend.box = "vertical",
        legend.spacing.x = unit(0.5, "cm")) 

plot_creattype_all <- rbind(ggplotGrob(p2), 
           ggplotGrob(p3), 
           ggplotGrob(p1),
           ggplotGrob(p4), 
           size = "last")

ggsave(paste0(fig_export_dir, "Compare_equations_all_plot.pdf"), p,
       width = 8, height = 10)


# pdf(paste0(fig_export_dir, "Figure2.pdf"), 
#     width = 12, height = 9)
# p
# dev.off(); dev.off()


################################################################################
# Plot pulbised and refit split by centre and creatinien type
################################################################################

Results_plot <- Results_Centre_CreatType %>% 
  ungroup() %>%
  mutate(equation = factor(equation,
                           levels = c("Refitted CamGFR", "LM", "CKD-EPI", "FAS", "MDRD"),
                           labels = c("Refitted CamGFR", "Lund-Malmo", "CKD-EPI", "FAS", "MDRD"))) %>%
  mutate(equ_centre = interaction(Centre, equation))


p1 <- Results_plot %>% 
  ggplot(aes_string(x = "Creatinine_type", y = "RMSE", group = "equ_centre")) + 
  geom_errorbar(aes_string(ymin = "RMSE_lwr", ymax = "RMSE_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 3, colour = "black", fill = "white", pch=21) + 
  geom_point(aes_string(shape = "Centre"), position= position_dodge(width = .5), size = 1.8) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank()) +
  scale_color_manual(values =tol6qualitative) +
  scale_shape_manual(values = c("C", "G", "M")) +
  ylab("RMSE\n(Accuracy)") + 
  theme(text = element_text(size = 14))

p2 <- Results_plot %>% 
  ggplot(aes_string(group = "equ_centre", x = "Creatinine_type", y = "median")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes_string(ymin = "median_lwr", ymax = "median_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 3, colour = "black", fill = "white", pch=21) + 
  geom_point(aes_string(shape = "Centre"), position= position_dodge(width = .5), size = 1.8) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank()) +
  scale_color_manual(values =tol6qualitative) +
  scale_shape_manual(values = c("C", "G", "M")) +
  ylab("Residual median\n(Bias)") + 
  theme(text = element_text(size = 14))

p3 <- Results_plot %>% 
  ggplot(aes_string(group = "equ_centre", x = "Creatinine_type", y = "IQR")) + 
  geom_errorbar(aes_string(ymin = "IQR_lwr", ymax = "IQR_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 3, colour = "black", fill = "white", pch=21) + 
  geom_point(aes_string(shape = "Centre"), position= position_dodge(width = .5), size = 1.8) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank()) + 
  scale_color_manual(values =tol6qualitative) +
  scale_shape_manual(values = c("C", "G", "M")) +
  ylab("Residual IQR\n(Precision)") 

p4 <- Results_plot %>% 
  ggplot(aes_string(group = "equ_centre", x = "Creatinine_type", y = "P20")) + 
  geom_errorbar(aes_string(ymin = "P20_lwr", ymax = "P20_upr", 
                           colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 3, colour = "black", fill = "white", pch=21) + 
  geom_point(aes_string(shape = "Centre"), position= position_dodge(width = .5), size = 1.8) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank())  +
  scale_color_manual(values =tol6qualitative) +
  scale_shape_manual(values = c("C", "G", "M")) +
  ylab("1 - P20\n(Clinical robustness)") +
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(), 
        text = element_text(size = 14), 
        legend.title = element_blank(), 
        legend.text = element_text(size = rel(1.2)),
        legend.box = "vertical",
        legend.spacing.x = unit(0.5, "cm")) 


plot_creat_centre <- 
  rbind(ggplotGrob(p2), 
        ggplotGrob(p3), 
        ggplotGrob(p1), 
        ggplotGrob(p4), 
        size = "last")

ggsave(paste0(fig_export_dir, "Compare_equations_plot.pdf"), plot_creat_centre,
       width = 8, height = 10)

################################################################################
# Plot pulbised and refit split by creat type (thesis)
################################################################################

Results_plot <- Results_CreatType %>%
  ungroup() %>%
  mutate(Creatinine_type = ifelse(Creatinine_type == "Non_IDMS", "Non-IDMS", "IDMS")) 
  # mutate(equation = factor(equation,
  #                          levels = c("Refit_CamGFR", "CamGFR_v1", "CKD-EPI", "LM", "FAS", "MDRD"),
  #                          labels = c("CamGFR v2", "CamGFR v1", "CKD-EPI", "Lund-Malmo",  "FAS", "MDRD"))) 
  # bind_rows(mutate(Results_CreatType, Centre = "All")) %>%
  # filter(Creatinine_type == "IDMS") %>%
  # ungroup() %>%
  # mutate(equ_centre = interaction(Centre, equation))


p1 <- Results_plot %>% 
  ggplot(aes_string(x = "Creatinine_type", y = "RMSE", group = "equation")) + 
  geom_errorbar(aes_string(ymin = "RMSE_lwr", ymax = "RMSE_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 2) + #, colour = "black", fill = "white", pch=21) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank()) +
  scale_color_manual(values =tol6qualitative) +
  ylab("RMSE\n(Accuracy)") + 
  theme(text = element_text(size = 14))

p2 <- Results_plot %>% 
  ggplot(aes_string(x = "Creatinine_type", y = "median", group = "equation")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes_string(ymin = "median_lwr", ymax = "median_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 2) + #, colour = "black", fill = "white", pch=21) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank()) +
  scale_color_manual(values =tol6qualitative) +
  ylab("Residual median\n(Bias)") + 
  theme(text = element_text(size = 14))

p3 <- Results_plot %>% 
  ggplot(aes_string(x = "Creatinine_type", y = "IQR", group = "equation")) + 
  geom_errorbar(aes_string(ymin = "IQR_lwr", ymax = "IQR_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 2) + #, colour = "black", fill = "white", pch=21) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank()) + 
  scale_color_manual(values =tol6qualitative) +
  ylab("Residual IQR\n(Precision)") 

p4 <- Results_plot %>% 
  ggplot(aes_string(x = "Creatinine_type", y = "P20_dose", group = "equation")) + 
  geom_errorbar(aes_string(ymin = "P20_dose_lwr", ymax = "P20_dose_upr", 
                           colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5), size = 2) + #, colour = "black", fill = "white", pch=21) + 
  ggplot_theme() + 
  theme(legend.position = "none",
        axis.title.x = element_blank())  +
  scale_color_manual(values =tol6qualitative) +
  guides(colour = guide_legend(nrow = 2)) +
  ylab("Dose P20\n(Clinical robustness)") +
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(), 
        text = element_text(size = 14), 
        legend.title = element_blank(), 
        legend.text = element_text(size = rel(1.2)),
        legend.box = "vertical",
        legend.spacing.x = unit(0.5, "cm")) 


plot_creat <- 
  rbind(ggplotGrob(p2), 
        ggplotGrob(p3), 
        ggplotGrob(p1), 
        ggplotGrob(p4), 
        size = "last")



ggsave("Refit_CamGFR_comparison.pdf", plot = plot_creat, 
       path = fig_export_dir,
       width = 20, height = 20/0.7, units = "cm")
s

