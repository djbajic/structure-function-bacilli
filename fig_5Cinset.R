require(data.table)
require(ggplot2)
source('functions.R')
d <- fread('data_SN_conditioning.csv')

# Example  
aux <- d[strain=='P' & cond.SN=='T' & cond.SN.dil == '0.0125']
aux.fit <- with(aux, double.mm(x, f.j, 3))

aux.pred <- data.table(z = seq(0, 1.5, by=.005))
aux.pred[, pred := 1 - (1 + aux.fit$Vj.fit*z)*exp(-aux.fit$Vj.fit*z)]
aux.pred[, pred.minus.se := 1 - (1 + (aux.fit$Vj.fit - aux.fit$se)*z)*exp(-(aux.fit$Vj.fit-aux.fit$se)*z)]
aux.pred[, pred.plus.se := 1 - (1 + (aux.fit$Vj.fit + aux.fit$se)*z)*exp(-(aux.fit$Vj.fit+aux.fit$se)*z)]

pdf('results/P_fit_example.pdf', 2.5, 2)
ggplot(aux.pred, aes(z, pred)) +
geom_ribbon(aes(ymin = pred.minus.se, ymax = pred.plus.se),
            fill = 'gray', color = 'gray') +
geom_line(aes(z, pred), linetype = 'dashed', col='red2') +
geom_point(data=aux, aes(x=(x*3), y=f.j),
           size=2, shape = 0) +
theme_bw() +
theme(panel.grid = element_blank()) +
labs(x = 'Z', y = 'fj')
dev.off()
