source('functions.R')
d <- fread('data/constitutive_expr_model.csv')

tp <- subset(d, !(comp %like% 'P'))

p <- 'results/fig_5A.pdf'
pdf(p, 3,3, family = 'CM Sans')
ggplot(tp, 
       aes(x = n.pred, y = Vj.fit)) +
geom_errorbar(aes(ymin=Vj.fit - se,
                  ymax=Vj.fit + se),
              size=.3, width = 0, color = 'skyblue3') +
geom_errorbarh(aes(xmin=n.pred - 2*n.pred.se,
                   xmax=n.pred + 2*n.pred.se),
               size=.3, height = 0, color = 'skyblue3') +
geom_point(size = 3, shape=1, color = 'skyblue3') + 
scale_color_manual(values = c('skyblue3', 'chocolate2')) +
geom_abline(intercept = 0, slope = 1, col='red', linetype = 'dashed') +
theme_bw() + labs(y = 'Measured' ,
                  x = 'Predicted from model with constitutive expression ',
                  colour = 'P. polymyxa in\n community') +
theme(panel.grid = element_blank(), aspect.ratio=1)
dev.off()
embed_fonts(p, outfile = p)

# RMSD
tp[, sqrt(sum((n.pred-Vj.fit)^2, na.rm=T)/.N)]


