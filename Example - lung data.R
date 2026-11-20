# data
library(survival)
lung <- survival::cancer
lung$time_y <- lung$time/365.25
lung$sex <- factor(lung$sex, levels = 1:2, labels = c("Male", "Female"))



## survival
# default call
fit <- survfit(Surv(time_y, status) ~ sex, data = lung)
pdf("survival.default.pdf", width = 7, height = 5)
plot(fit)
dev.off()

# adding some extras (default in BiMaUmisc)
pdf("survival.extra.pdf", width = 7, height = 5)
plot(fit, mark.time = TRUE, lwd = 2, col = 1:2,
     xaxs = "i", yaxs = "i", xlim = c(0, 3), 
     xlab = "Time (years)", ylab = "Survival (proportion)",
     main = "Survival curve",
     las = 1, font.lab = 2)
legend("bottomleft", c("Male", "Female"), col = 1:2, lty = 1, 
       lwd = 2, bty = "n")
dev.off()



## ggsurvfit
library(ggsurvfit)

# default call
fit2 <- survfit2(Surv(time_y, status) ~ sex, data = lung)
pdf("ggsurvfit.default.pdf", width = 7, height = 5)
ggsurvfit(fit2)
dev.off()

# adding some extras (default in BiMaUmisc)
p <- ggsurvfit(fit2)  +
  theme_classic() + 
  add_censor_mark() +
  add_risktable(risktable_stats = "n.risk") +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_color_manual(values = 1:2) + 
  theme(legend.position = c(0.12, 0.15),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text = element_text(face = "bold")) +
  labs(x = "Time (years)", y = "Survival (proportion)",
       title = "Survival curve")
built_p <- ggsurvfit_build(p)
ggsave("ggsurvfit.default.BiMaUmisc.pdf", plot = built_p, 
       width = 7, height = 5)

# trying to fix alignment of the risk table
p <- ggsurvfit(fit2) +
  theme_classic() + 
  add_censor_mark() +
  add_risktable(risktable_stats = "n.risk") +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 1), 
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), 
                     expand = c(0, 0)) +
  scale_color_manual(values = 1:2) + 
  theme(legend.position = c(0.12, 0.15),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text = element_text(face = "bold")) +
  labs(x = "Time (years)", y = "Survival (proportion)",
       title = "Survival curve")
built_p <- ggsurvfit_build(p)
ggsave("ggsurvfit.default.BiMaUmisc.overlap.pdf", plot = built_p, 
       width = 7, height = 5)



## survminer
library(survminer)

# default call
p <- ggsurvplot(fit)
ggsave("survminer.default.pdf", p$plot, width = 7, height = 5)

# adding some extras (default in BiMaUmisc)
p <- ggsurvplot(fit, data = lung, 
                legend.title = "", legend.labs = c("Male", "Female"), 
                legend = c(0.1, 0.15),
                pval = TRUE, pval.coord = c(2.25, 0.9),
                xlab = "Time (years)", ylab = "Survival (proportion)",
                font.x = "bold", font.y = "bold",
                risk.table = TRUE, tables.height = 0.2,
                tables.theme = theme_cleantable(),
                title = "Survival curve", font.tickslab = "bold",
                censor.size = 6, palette = 1:2)

p$plot <- p$plot + 
  coord_cartesian(xlim = c(0, 3), ylim = c(0, 1), clip = "off") +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 1), 
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), 
                     expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

library(cowplot)
combined <- plot_grid(p$plot, p$table, ncol = 1, align = "v",
                      axis = "lr", rel_heights = c(4, 1))
ggsave("survminer.default.BiMaUmisc.pdf", combined, width = 7, height = 5)

# trying to fix alignment of the risk table
p$table <- p$table + 
  coord_cartesian(xlim = c(0, 3), clip = "off") +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 1), 
                     expand = c(0, 0))
combined <- plot_grid(p$plot, p$table, ncol = 1, align = "v",
                      axis = "lr", rel_heights = c(4, 1))
ggsave("survminer.default.BiMaUmisc.overlap.pdf", combined, 
       width = 7, height = 5)



## BiMaUmisc
library(BiMaUmisc)

# default
pdf("BiMaUmisc.default.pdf", width = 7, height = 5)
survival.plot(lung, time_y, status, sex, units = "y")
dev.off()

# adding extras
pdf("BiMaUmisc.extras.pdf", width = 7, height = 5)
survival.plot(lung, time_y, status, sex, 
              legend.pos = "topright", legend.title = "Sex at birth", 
              events = TRUE, risk.text = FALSE, 
              p.value.pos = c(2.4, 0.4),
              units = "y", xlab = "Time since diagnosis (in years)", 
              main = "Overall survival", 
              col = c("darkviolet", "darkgreen"))
dev.off()