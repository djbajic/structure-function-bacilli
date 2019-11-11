source('functions.R')
d <- fread('data/constitutive_expr_model.csv')

tp <- subset(d, comp %like% 'P')

p <- 'results/fig_5B.pdf'
pdf(p, 3,3, family = 'CM Sans')
ggplot(tp,
       aes(x = n.pred, y = Vj.fit)) +
geom_errorbar(aes(ymin=Vj.fit - se,
                  ymax=Vj.fit + se),
              size=.3, width = 0, color = 'chocolate2') +
geom_errorbarh(aes(xmin=n.pred - 2*n.pred.se,
                   xmax=n.pred + 2*n.pred.se),
               size=.3, height = 0, color = 'chocolate2') +
geom_point(size = 3, shape=1, color = 'chocolate2') + 
scale_y_log10() + scale_x_log10() +
geom_abline(intercept = 0, slope = 1, col='red', linetype = 'dashed') +
theme_bw() + labs(y = 'Measured' ,
                  x = 'Predicted from model with constitutive expression ',
                  colour = 'P. polymyxa in\n community') +
annotation_logticks(sides="lb", size=.4) +
scale_shape_manual(values = c(c(0:2, 15:18))) +
theme(panel.grid = element_blank(), aspect.ratio=1)
dev.off()
embed_fonts(p, outfile = p)


# RMSD
tp[, sqrt(sum((n.pred-Vj.fit)^2, na.rm=T)/.N)]
