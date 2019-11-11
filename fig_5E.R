source('functions.R')
gr <- fread('data/communities_saturating_model_predictions.csv')

tp <- subset(gr, (comp %like% 'P'))
tp[, sqrt(sum((satm.pred-Vj.fit)^2, na.rm=T)/.N)]
tp[, cor.test(satm.pred, Vj.fit)]

p <- 'results/5E.pdf'
pdf(p, 3,3, family = 'CM Sans')
ggplot(tp, aes(x = satm.pred, y = Vj.fit)) +
geom_errorbar(aes(ymin=Vj.fit - se,
                  ymax=Vj.fit + se),
              size=.3, width = 0, color = 'chocolate2') +
geom_errorbarh(aes(xmin=satm.pred - 2*satm.pred.se,
                   xmax=satm.pred + 2*satm.pred.se),
               size=.3, height = 0, color = 'chocolate2') +
geom_point(size = 3, shape=1, color = 'chocolate2') + 
geom_abline(intercept = 0, slope = 1, col='red', linetype = 'dashed') +
theme_bw() + labs(y = 'Measured' ,
                  x = 'Predicted from saturating null model ',
                  colour = 'P. polymyxa in\n community') +
scale_shape_manual(values = c(c(0:2, 15:18))) +
theme(panel.grid = element_blank(), aspect.ratio=1)
dev.off()
embed_fonts(p, outfile = p)
