source('functions.R')
strfun <- fread('data/structure_function_PA.csv')

p <- 'results/fig_5B_inset.pdf'
pdf(p, 3, 3, family = 'CM Sans')
ggplot(subset(strfun, poly %like% 'P'),
       aes(x = Vj.1.V2, y = Vj.real.V2)) +
    geom_errorbar(aes(ymin=Vj.real.V2 - se.real.V2,
                      ymax=Vj.real.V2 + se.real.V2),
                  size=.3, width = 0, colour = 'gray') +
    geom_errorbarh(aes(xmin=Vj.1.V2 - 2*Vj.1.V2.se,
                       xmax=Vj.1.V2 + 2*Vj.1.V2.se), 
                   size=.3, height = 0, colour = 'gray') +
    geom_point(size = 3, color = 'chocolate2') +
    xlim(0, 15) + ylim(0, 40) +
    theme_bw() + labs(y = 'Measured' ,
                      x = 'Predicted from additive model ',
                      colour = 'Species in\n community') +
    geom_abline(intercept = 0, slope = 1) +
theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)
