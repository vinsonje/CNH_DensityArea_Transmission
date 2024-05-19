##########################################
#Sensitivity Analysis for  
#Reversion
#Dilution and allee effects
##########################################

setwd("C:/Users/SIU856560341/Desktop/CNH - Parasite Transmission and Host Density")

source(paste(getwd(), "/densityarea_trans_sourcer.R", sep = ''))

######################################
#Host R0 parameters
######################################
mH.X = 0.15
gH.X = 0.15

mH.Y = 0.15
gH.Y = 0.15

beta.X = 1/40
beta.Y = 1/40

#######################################
#Land-use parameters
#######################################
aX = seq(0.02, 0.4, 0.01)
r = 0.02
t.crit = 30

########################################################################
#Reversion
########################################################################
p = 1

#####################################
#RND
#Reversion
#Neither Dilution
#####################################
Nmax.X = 25
Nmax.Y = c(12, 50)
px.X = 0.5
px.Y = 0.5
######################################################
p0.X = c(0, 1.0)
p0.Y = 1.0
######################################################
p1.X = 1.0
p1.Y = 1.0
######################################################

RND.parms = expand.grid(Nmax.X = Nmax.X, Nmax.Y = Nmax.Y,
                        px.X = px.X, px.Y = px.Y,
                        p0.X = p0.X, p0.Y = p0.Y,
                        p1.X = p1.X, p1.Y = p1.Y,
                        aX = aX, r = r, t.crit = t.crit, p = p,
                        mH.X = mH.X, gH.X = gH.X,
                        mH.Y = mH.Y, gH.Y = gH.Y)

R0.out.RND = simulate.densityarea.trans(RND.parms)

SA.calcs.RND = sensitivity.calcs(R0.out.RND)

p.RND.max = ggplot(SA.calcs.RND, aes(x = hl.time, y = max)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.RND.max

p.RND.maxtime = ggplot(SA.calcs.RND, aes(x = hl.time, y = max.time)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0]))  + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.RND.maxtime

p.RND.min = ggplot(SA.calcs.RND, aes(x = hl.time, y = min)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.RND.min

p.RND.mintime = ggplot(SA.calcs.RND, aes(x = hl.time, y = min.time)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0]))  + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.RND.mintime

#####################################
#RXD
#Reversion
#X (Intact) Dilution
#####################################
Nmax.X = 25
Nmax.Y = c(12, 50)
px.X = 0.5
px.Y = 0.5
######################################################
p0.X = c(0, 1.0)
p0.Y = 1.0
######################################################
p1.X = 0.5
p1.Y = 1.0
######################################################

RXD.parms = expand.grid(Nmax.X = Nmax.X, Nmax.Y = Nmax.Y,
                        px.X = px.X, px.Y = px.Y,
                        p0.X = p0.X, p0.Y = p0.Y,
                        p1.X = p1.X, p1.Y = p1.Y,
                        aX = aX, r = r, t.crit = t.crit, p = p,
                        mH.X = mH.X, gH.X = gH.X,
                        mH.Y = mH.Y, gH.Y = gH.Y)

R0.out.RXD = simulate.densityarea.trans(RXD.parms)

SA.calcs.RXD = sensitivity.calcs(R0.out.RXD)

p.RXD.max = ggplot(SA.calcs.RXD, aes(x = hl.time, y = max)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.RXD.max

p.RXD.maxtime = ggplot(SA.calcs.RXD, aes(x = hl.time, y = max.time)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0]))  + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.RXD.maxtime

p.RXD.min = ggplot(SA.calcs.RXD, aes(x = hl.time, y = min)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.RXD.min

p.RXD.mintime = ggplot(SA.calcs.RXD, aes(x = hl.time, y = min.time)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0]))  + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.RXD.mintime

#####################################
#RBD
#Reversion
#Both Dilution
#####################################
Nmax.X = 25
Nmax.Y = c(12, 50)
px.X = 0.5
px.Y = 0.5
######################################################
p0.X = c(0, 1.0)
p0.Y = 1.0
######################################################
p1.X = 0.5
p1.Y = 0.5
######################################################

RBD.parms = expand.grid(Nmax.X = Nmax.X, Nmax.Y = Nmax.Y,
                        px.X = px.X, px.Y = px.Y,
                        p0.X = p0.X, p0.Y = p0.Y,
                        p1.X = p1.X, p1.Y = p1.Y,
                        aX = aX, r = r, t.crit = t.crit, p = p,
                        mH.X = mH.X, gH.X = gH.X,
                        mH.Y = mH.Y, gH.Y = gH.Y)

R0.out.RBD = simulate.densityarea.trans(RBD.parms)

SA.calcs.RBD = sensitivity.calcs(R0.out.RBD)

p.RBD.max = ggplot(SA.calcs.RBD, aes(x = hl.time, y = max)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.RBD.max

p.RBD.maxtime = ggplot(SA.calcs.RBD, aes(x = hl.time, y = max.time)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0]))  + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.RBD.maxtime

p.RBD.min = ggplot(SA.calcs.RBD, aes(x = hl.time, y = min)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.RBD.min

p.RBD.mintime = ggplot(SA.calcs.RBD, aes(x = hl.time, y = min.time)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0]))  + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.RXD.mintime

###############################################################################
#Settled
##############################################################################
p = 0

#####################################
#SND
#Settled
#Neither Dilution
#####################################
Nmax.X = 25
Nmax.Y = c(12, 50)
px.X = 0.5
px.Y = 0.5
######################################################
p0.X = c(0, 1.0)
p0.Y = 1.0
######################################################
p1.X = 1.0
p1.Y = 1.0
######################################################

SND.parms = expand.grid(Nmax.X = Nmax.X, Nmax.Y = Nmax.Y,
                        px.X = px.X, px.Y = px.Y,
                        p0.X = p0.X, p0.Y = p0.Y,
                        p1.X = p1.X, p1.Y = p1.Y,
                        aX = aX, r = r, t.crit = t.crit, p = p,
                        mH.X = mH.X, gH.X = gH.X,
                        mH.Y = mH.Y, gH.Y = gH.Y)

R0.out.SND = simulate.densityarea.trans(SND.parms)

SA.calcs.SND = sensitivity.calcs(R0.out.SND)

p.SND.max = ggplot(SA.calcs.SND, aes(x = hl.time, y = max)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.SND.max

p.SND.maxtime = ggplot(SA.calcs.SND, aes(x = hl.time, y = max.time)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0]))  + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.SND.maxtime

p.SND.min = ggplot(SA.calcs.SND, aes(x = hl.time, y = min)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.SND.min

p.SND.mintime = ggplot(SA.calcs.SND, aes(x = hl.time, y = min.time)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0]))  + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.SND.mintime

#####################################
#SXD
#Settled
#X (Intact) Dilution
#####################################
Nmax.X = 25
Nmax.Y = c(12, 50)
px.X = 0.5
px.Y = 0.5
######################################################
p0.X = c(0, 1.0)
p0.Y = 1.0
######################################################
p1.X = 0.5
p1.Y = 1.0
######################################################

SXD.parms = expand.grid(Nmax.X = Nmax.X, Nmax.Y = Nmax.Y,
                        px.X = px.X, px.Y = px.Y,
                        p0.X = p0.X, p0.Y = p0.Y,
                        p1.X = p1.X, p1.Y = p1.Y,
                        aX = aX, r = r, t.crit = t.crit, p = p,
                        mH.X = mH.X, gH.X = gH.X,
                        mH.Y = mH.Y, gH.Y = gH.Y)

R0.out.SXD = simulate.densityarea.trans(SXD.parms)

SA.calcs.SXD = sensitivity.calcs(R0.out.SXD)

p.SXD.max = ggplot(SA.calcs.SXD, aes(x = hl.time, y = max)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.SXD.max

p.SXD.maxtime = ggplot(SA.calcs.SXD, aes(x = hl.time, y = max.time)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0]))  + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.SXD.maxtime

p.SXD.min = ggplot(SA.calcs.SXD, aes(x = hl.time, y = min)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.SXD.min

p.SXD.mintime = ggplot(SA.calcs.SXD, aes(x = hl.time, y = min.time)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0]))  + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.SXD.mintime

#####################################
#SBD
#Settled
#Both Dilution
#####################################
Nmax.X = 25
Nmax.Y = c(12, 50)
px.X = 0.5
px.Y = 0.5
######################################################
p0.X = c(0, 1.0)
p0.Y = 1.0
######################################################
p1.X = 0.5
p1.Y = 0.5
######################################################

SBD.parms = expand.grid(Nmax.X = Nmax.X, Nmax.Y = Nmax.Y,
                        px.X = px.X, px.Y = px.Y,
                        p0.X = p0.X, p0.Y = p0.Y,
                        p1.X = p1.X, p1.Y = p1.Y,
                        aX = aX, r = r, t.crit = t.crit, p = p,
                        mH.X = mH.X, gH.X = gH.X,
                        mH.Y = mH.Y, gH.Y = gH.Y)

R0.out.SBD = simulate.densityarea.trans(SBD.parms)

SA.calcs.SBD = sensitivity.calcs(R0.out.SBD)

p.SBD.max = ggplot(SA.calcs.SND, aes(x = hl.time, y = max)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.SBD.max

p.SBD.maxtime = ggplot(SA.calcs.SBD, aes(x = hl.time, y = max.time)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0]))  + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.SBD.maxtime

p.SBD.min = ggplot(SA.calcs.SBD, aes(x = hl.time, y = min)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) 

p.SBD.min

p.SBD.mintime = ggplot(SA.calcs.SBD, aes(x = hl.time, y = min.time)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  geom_point(aes(color=as.factor(p0.X), shape=as.factor(Nmax.Y))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0]))  + scale_shape(guide = "none") +
  theme(legend.position = "none", axis.title = element_blank()) 

p.SBD.mintime



#################################
#Extact Legend
#################################
p.leg = get_legend(p.SBD.mintime + theme(legend.position = "bottom"))

ggdraw() + draw_plot(p.leg)


################################################################################
#Combining Plots
################################################################################
lay.mat.col = matrix(c(0.3, 0.3, 0.3), ncol = 1)
p.max = ggarrange(p.RND.max, p.SND.max,
                  p.RXD.max, p.SXD.max,
                  p.RBD.max, p.SBD.max,
                  ncol = 2, nrow = 3,
                  common.legend = FALSE,
                  labels = "AUTO", hjust = -4.0)


p.max = annotate_figure(p.max,
                        bottom = text_grob("Halflife (years)", color = "black", size = 17),
                        left = text_grob(expression(paste("Max Landscape R"["0"])), color = "black", rot = 90, size = 17),
                        top = text_grob("Reversion                                          Settled", color = "black", size = 17),
                        right = text_grob("No Dilution               Dilution in Intact          Dilution in Both", color = "black", size = 17, rot = 270)
)
p.max

p.max.leg = ggarrange(p.max,
                      p.leg,
                      ncol = 1, nrow = 2,
                      heights = c(1.0, 0.1))

p.max.leg

p.maxtime = ggarrange(p.RND.maxtime, p.SND.maxtime,
                  p.RXD.maxtime, p.SXD.maxtime,
                  p.RBD.maxtime, p.SBD.maxtime,
                  ncol = 2, nrow = 3,
                  common.legend = FALSE,
                  labels = "AUTO", hjust = -4.0)


p.maxtime = annotate_figure(p.maxtime,
                        bottom = text_grob("Halflife (years)", color = "black", size = 17),
                        left = text_grob(expression(paste("Time (years) Max Landscape R"["0"])), color = "black", rot = 90, size = 17),
                        top = text_grob("Reversion                                          Settled", color = "black", size = 17),
                        right = text_grob("No Dilution               Dilution in Intact          Dilution in Both", color = "black", size = 17, rot = 270)
)
p.maxtime

p.maxtime.leg = ggarrange(p.maxtime,
                      p.leg,
                      ncol = 1, nrow = 2,
                      heights = c(1.0, 0.1))

p.maxtime.leg

p.min = ggarrange(p.RND.min, p.SND.min,
                  p.RXD.min, p.SXD.min,
                  p.RBD.min, p.SBD.min,
                  ncol = 2, nrow = 3,
                  common.legend = FALSE,
                  labels = "AUTO", hjust = -4.0)


p.min = annotate_figure(p.min,
                        bottom = text_grob("Halflife (years)", color = "black", size = 17),
                        left = text_grob(expression(paste("Min Landscape R"["0"])), color = "black", rot = 90, size = 17),
                        top = text_grob("Reversion                                          Settled", color = "black", size = 17),
                        right = text_grob("No Dilution               Dilution in Intact          Dilution in Both", color = "black", size = 17, rot = 270)
)
p.min

p.min.leg = ggarrange(p.min,
                      p.leg,
                      ncol = 1, nrow = 2,
                      heights = c(1.0, 0.1))

p.min.leg

p.mintime = ggarrange(p.RND.mintime, p.SND.mintime,
                      p.RXD.mintime, p.SXD.mintime,
                      p.RBD.mintime, p.SBD.mintime,
                      ncol = 2, nrow = 3,
                      common.legend = FALSE,
                      labels = "AUTO", hjust = -4.0)


p.mintime = annotate_figure(p.mintime,
                            bottom = text_grob("Halflife (years)", color = "black", size = 17),
                            left = text_grob(expression(paste("Time (years) Min Landscape R"["0"])), color = "black", rot = 90, size = 17),
                            top = text_grob("Reversion                                          Settled", color = "black", size = 17),
                            right = text_grob("No Dilution               Dilution in Intact          Dilution in Both", color = "black", size = 17, rot = 270)
)
p.mintime

p.mintime.leg = ggarrange(p.mintime,
                          p.leg,
                          ncol = 1, nrow = 2,
                          heights = c(1.0, 0.1))

p.mintime.leg



