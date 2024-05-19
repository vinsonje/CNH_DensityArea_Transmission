##########################################
#Simulate and generate plots for 
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
aX = 0.1
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

p.RND = ggplot(R0.out.RND, aes(x = time, y = R0.land)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159"))  +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top")) + ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) + 
  geom_vline(aes(xintercept = t.crit), lwd = 1.5, linetype = "dashed", color = "black")

p.RND + theme(legend.position = "none")

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

p.RXD = ggplot(R0.out.RND, aes(x = time, y = R0.land)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159"))  +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top")) + ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) + 
  geom_vline(aes(xintercept = t.crit), lwd = 1.5, linetype = "dashed", color = "black")

p.RXD + theme(legend.position = "none")

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

p.RBD = ggplot(R0.out.RBD, aes(x = time, y = R0.land)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159"))  +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top")) + ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) + 
  geom_vline(aes(xintercept = t.crit), lwd = 1.5, linetype = "dashed", color = "black")

p.RBD + theme(legend.position = "none")


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

p.SND = ggplot(R0.out.SND, aes(x = time, y = R0.land)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159"))  +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top")) + ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) + 
  geom_vline(aes(xintercept = t.crit), lwd = 1.5, linetype = "dashed", color = "black")

p.SND + theme(legend.position = "none")

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

p.SXD = ggplot(R0.out.SXD, aes(x = time, y = R0.land)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159"))  +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top")) + ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) + 
  geom_vline(aes(xintercept = t.crit), lwd = 1.5, linetype = "dashed", color = "black")

p.SXD + theme(legend.position = "none")

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

p.SBD = ggplot(R0.out.SBD, aes(x = time, y = R0.land)) + geom_line(aes(color=as.factor(p0.X), linetype=as.factor(Nmax.Y)), lwd=1.0) + theme_cowplot() +
  theme(legend.position = "bottom", axis.text = element_text(size = 20), axis.title =  element_text(size = 20)) + 
  scale_color_manual(name = "Host regulation at low habitat amount", labels = c("Allee in Intact", "Neither"), values = c("#1A85FF","#D41159")) +
  scale_linetype_manual(name = "Host performance in Cleared", labels = c("Worse", "Better"), values = c("solid", "dotted")) +
  guides(color = guide_legend(nrow = 1, title.position = "top"), linetype = guide_legend(nrow = 1, title.position = "top")) +
  ylab(bquote("landscape" ~ R[0])) + ylim(0, 5.0) + 
  theme(legend.position = "none", axis.title = element_blank()) + 
  geom_vline(aes(xintercept = t.crit), lwd = 1.5, linetype = "dashed", color = "black")

p.SBD + theme(legend.position = "bottom")

###########################################################################
#Extract legend
###########################################################################
p.leg = get_legend(p.SBD + theme(legend.position = "bottom"))

ggdraw() + draw_plot(p.leg)


################################################################################
#Combining Plots
################################################################################
lay.mat.col = matrix(c(0.3, 0.3, 0.3), ncol = 1)
p.all = ggarrange(p.RND, p.SND,
                  p.RXD, p.SXD,
                  p.RBD, p.SBD,
                  ncol = 2, nrow = 3,
                  common.legend = FALSE,
                  labels = "AUTO", hjust = -4.0)


p.all = annotate_figure(p.all,
                bottom = text_grob("Time (years)", color = "black", size = 17),
                left = text_grob(expression(paste("Landscape R"["0"])), color = "black", rot = 90, size = 17),
                top = text_grob("Reversion                                          Settled", color = "black", size = 17),
                right = text_grob("No Dilution               Dilution in Intact          Dilution in Both", color = "black", size = 17, rot = 270)
)
p.all

p.all.leg = ggarrange(p.all,
                      p.leg,
                      ncol = 1, nrow = 2,
                      heights = c(1.0, 0.1))

p.all.leg
