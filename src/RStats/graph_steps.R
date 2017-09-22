pdf("steps_plot.pdf")
plot(read.table("stats_steps.data", sep=",", col.names = c("discs", "steps")), main="Hanoi: Step count depending on number of discs")
dev.off()
