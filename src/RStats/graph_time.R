pdf("time_plot.pdf")
plot(rowMeans(read.table("stats_time.data", sep=",")), xlab = "discs", ylab = "time (s)", main = "Hanoi: Execution time depending on number of discs")
dev.off()

