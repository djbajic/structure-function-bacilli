source('functions.R')
d <- fread('data/communities_saturating_model_predictions.csv')

tp <- d[comp %like% 'P' & replc==1]
tp[, upper.limit := Vj.fit - P.pred.comm]
tp <- tp[order(c.len),]
tp[, comp := ordered(comp, comp)]

p <- 'results/fig_S11D.pdf'
pdf(p, 10, 3)
ggplot(tp, aes(x = comp, y=-P.pred.comm, color = comp %like% 'P')) +
geom_hline(yintercept=0, linetype='dashed', color = 'gray') +
geom_crossbar(aes(ymin=-P.pred.comm, ymax=upper.limit),
              color='chocolate2', fatten=2) + xlab('Community') + 
theme_bw() + theme(panel.grid = element_blank(),
                   axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()
embed_fonts(p, outfile = p)


