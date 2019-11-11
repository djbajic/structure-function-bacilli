source('functions.R')
d <- fread('data/communities_saturating_model_predictions.csv')

tp <- melt(d[c.len>2 & replc==1], id.vars = 'comp',
           measure.vars = c('PA.H.pairwise.tot', 
                            'H.pairwise.pop', 
                            'H.pairwise.behav'))

tp.err <- melt(d[c.len>2 & replc==1], id.vars = 'comp',
               measure.vars = c('PA.H.pairwise.tot.se',
                                'H.pairwise.pop.se',
                                'H.pairwise.behav.se'))
tp[, se := tp.err$value]

p <- 'results/fig_6B.pdf'
pdf(p, 2, 4, family = 'CM Sans')
ggplot(tp, aes(x = variable, y=value, color = comp %like% 'P')) +
geom_hline(yintercept=0, linetype='dashed', color = 'gray') +
geom_pointrange(aes(ymin=value-se, ymax=value+se), size = .2,
                position = position_jitter(width = 0.1)) +
scale_colour_manual(values = c('skyblue3', 'chocolate2'), guide=F) +
theme_bw() + theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)

d[, check := Vj.fit - PA.pred - sum.PA.pairwise - PA.H.pairwise.tot]
d$check<1e-13 # all values should be =0 (or extremely small)
