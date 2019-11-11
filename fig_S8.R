source('functions.R')
s.coef <- fread('data/singles_coefficients_fig_1.csv')
langm <- fread('data/langmuir_null_predictions.csv')
names(s.coef)[1] <- 'strain'

fit.compare <- merge(s.coef, langm, by='strain')

p <- 'results/fig_S8.pdf'
pdf(p, 3,3, family = 'CM Sans')
ggplot(fit.compare, aes(Vj.fit, epsilon)) + 
geom_errorbarh(aes(xmin = Vj.fit - se,
                   xmax = Vj.fit + se, height = 0), color = 'gray') +
geom_errorbar(aes(ymin = epsilon - epsilon.sd,
                  ymax = epsilon + epsilon.sd, width = 0), color = 'gray') +
geom_point() +
geom_text(aes(label=strain),hjust=0, vjust=0) +
scale_y_log10() + scale_x_log10() +
geom_abline(intercept = 0, slope = 1, col='red', linetype = 'dashed') +
theme_bw() + 
theme(panel.grid = element_blank(), aspect.ratio=1) +
annotation_logticks(sides="lb", size=.4)
dev.off()
embed_fonts(p)
