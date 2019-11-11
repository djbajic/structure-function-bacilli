source('functions.R')
gr <- fread('data/comm_mixes_dMMfits.csv')

aux <- gr[comp %like% 'P' & c.len<=2]
aux <- aux[which(!is.na(aux$P))]

p <- 'results/fig_4B.pdf'
pdf(p, 4, 4, family = 'CM Sans')
ggplot(aux, aes(x=comp, y=P)) +
geom_bar(stat = 'identity') +
scale_y_log10() + 
geom_errorbar(aes(ymin=P - sd.P, 
                  ymax=P + sd.P),
              size=.3, width = .1,) +
annotation_logticks(sides="l", size=.4) +
theme_bw() + labs(y = 'P. Polymyxa CFU' ,
                  x = 'Community size') +
theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)
