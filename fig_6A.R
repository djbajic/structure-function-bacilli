source('functions.R')
d <- fread('data/pairs_saturating_model_predictions.csv')

### FIGURE 6A
tp <- melt(d, id.vars = 'comp',
           measure.vars = c('PA.epsilon.tot', 
                            'epsilon.pop', 
                            'epsilon.behav'))

tp.err <- melt(d, id.vars = 'comp',
               measure.vars = c('PA.epsilon.tot.se',
                                'epsilon.pop.se',
                                'epsilon.behav.se'))
tp[, se := tp.err$value]

p <- 'results/fig_6A.pdf'
pdf(p, 2, 4, family = 'CM Sans')
ggplot(tp, aes(x = variable, y=value, color = comp %like% 'P')) +
geom_hline(yintercept=0, linetype='dashed', color = 'gray') +
geom_pointrange(aes(ymin=value-se, ymax=value+se), size=.2,
                position = position_jitter(width = 0.1)) +
scale_colour_manual(values = c('skyblue3', 'chocolate2'), guide=F) +
theme_bw() + theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)
