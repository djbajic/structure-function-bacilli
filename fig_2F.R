source('functions.R')
strfun <- fread('data/structure_function_PA.csv')

p <- 'results/fig_2F.pdf'
pdf(p, 5, 4, family = 'CM Sans')
ggplot(strfun, aes(x = Vj.1.V2, y = Vj.real.V2,
                   colour = poly %like% 'P',
                   shape = as.factor(len.v2))) +
    geom_errorbar(aes(ymin=Vj.real.V2 - se.real.V2,
                      ymax=Vj.real.V2 + se.real.V2),
                  size=.3, width = 0, colour = 'gray') +
    geom_errorbarh(aes(xmin=Vj.1.V2 - 2*Vj.1.V2.se,
                       xmax=Vj.1.V2 + 2*Vj.1.V2.se), 
                   size=.3, height = 0, colour = 'gray') +
        geom_point(size = 3) + xlim(0, 15) + ylim(0, 60) +
    theme_bw() + labs(y = 'Measured' ,
                      x = 'Predicted from additive model ',
                      colour = 'Species in\n community') +
    scale_colour_manual(values = c('skyblue3', 'chocolate2')) +
    scale_shape_manual(values = c(c(0:2, 15:18))) +
    geom_abline(intercept = 0, slope = 1) +
    theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)

# correlations in this figure in P and not P containing consortia
unique(strfun[,.(V2, Vj.1.V2, Vj.real.V2)])[, cor.test(Vj.1.V2, Vj.real.V2, method='p'), by=(V2%like% 'P')]            
