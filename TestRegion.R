library(data.table)
library(ggplot2)
skip <- 1400
nrows <- 200
TestRegion <- data.table::fread("TestRegion.csv", skip=skip, nrows=nrows)
test.mat <- as.matrix(TestRegion)
str(test.mat)
image(test.mat)

TestRegion.tall <- data.table(
  intensity=as.numeric(test.mat),
  depth.index=as.integer(col(test.mat)),
  distance.shots=as.integer(row(test.mat)+skip))
dist.hilite <- data.table(distance.shots=seq(1440, 1500, by=10))
gg.img <- ggplot()+
  scale_fill_gradient(low="white", high="red")+
  geom_tile(aes(
    distance.shots, depth.index, fill=intensity),
    data=TestRegion.tall)+
  scale_y_reverse()+
  geom_vline(aes(
    xintercept=distance.shots),
    data=dist.hilite)
png("TestRegion-zoom.png", width=20, height=10, units="in", res=50)
print(gg.img)
dev.off()

intensity.hilite <- TestRegion.tall[dist.hilite, on="distance.shots"]
ggplot()+
  geom_point(aes(
    depth.index, intensity),
    data=intensity.hilite)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0,"lines"))+
  facet_grid(distance.shots ~ ., labeller=label_both)
