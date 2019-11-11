source('functions.R')
gr <- fread('data/data_P_conditioning_OD.csv')

p <- 'results/4D.pdf'
pdf(p, 4, 4, family = 'CM Sans')
ggplot(subset(gr, cond.SN!='B'), aes(x = cond.SN, y = abs.m)) +
geom_bar(stat ='identity') +
geom_errorbar(aes(ymax = abs.m + se, ymin = abs.m - se), width = .2) +
theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)
