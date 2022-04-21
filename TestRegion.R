library(data.table)
library(ggplot2)
skip <- 1400
nrows <- 200
TestRegion <- data.table::fread("TestRegion.csv", skip=skip, nrows=nrows)
test.mat <- as.matrix(TestRegion)
str(test.mat)
if(FALSE){
  image(test.mat)
}

TestRegion.tall <- data.table(
  intensity=as.numeric(test.mat),
  depth.index=as.integer(col(test.mat)),
  distance.shots=as.integer(row(test.mat)+skip)
)[order(distance.shots, -depth.index)]
TestRegion.tall[, index.from.bottom := 1:.N, by=distance.shots]
dist.hilite <- data.table(distance.shots=seq(1440, 1510, by=10))
scale.fill <- scale_fill_gradient(low="white", high="blue")
gg.img <- ggplot()+
  geom_tile(aes(
    distance.shots, depth.index, fill=intensity),
    data=TestRegion.tall)+
  scale_y_reverse()+
  geom_vline(aes(
    xintercept=distance.shots),
    data=dist.hilite)

intensity.hilite <- TestRegion.tall[
  dist.hilite, on="distance.shots"]
penalty <- 100
graph.df <- gfpop::graph(
  gfpop::Edge("first_bottom", "increasing_to_shelf", "up"),
  gfpop::Edge("increasing_to_shelf", "increasing_to_shelf", "up"),
  gfpop::Edge("increasing_to_shelf", "shelf", "up"),
  gfpop::Edge("shelf", "decreasing_after_shelf", "down"),
  gfpop::Edge("decreasing_after_shelf", "decreasing_after_shelf", "down"),
  gfpop::Edge("decreasing_after_shelf", "bottom", "down"),
  gfpop::Edge("bottom", "increasing_to_fish", "up", penalty=penalty),
  gfpop::Edge("increasing_to_fish", "increasing_to_fish", "up"),
  gfpop::Edge("increasing_to_fish", "fish", "up"),
  gfpop::Edge("fish", "decreasing_after_fish", "down"),
  gfpop::Edge("decreasing_after_fish", "decreasing_after_fish", "down"),
  gfpop::Edge("decreasing_after_fish", "bottom", "down"),
  gfpop::StartEnd("first_bottom","bottom"),
  all.null.edges = TRUE)
seg.dt <- intensity.hilite[, {
  fit <- gfpop::gfpop(intensity, graph.df)
  with(fit, data.table(
    start=c(1L, changepoints[-length(changepoints)]+1L),
    end=changepoints,
    state=states,
    mean=parameters))
}, by=distance.shots]
state.to.show <- c("fish","shelf")
seg.to.show <- seg.dt[state %in% state.to.show]
dcast(seg.to.show, distance.shots ~ state, length)

model.color <- "red"
gg.segs <- ggplot()+
  ggtitle(paste0("Segmentation of graph-constrained optimal changepoint algorithm with penalty=", penalty))+
  geom_point(aes(
    index.from.bottom, intensity),
    shape=1,
    color="grey50",
    data=intensity.hilite)+
  geom_step(aes(
    start, mean),
    color=model.color,
    data=seg.dt)+
  geom_point(aes(
    start, mean),
    data=seg.to.show,
    shape=21,
    color="black",
    fill=model.color)+
  geom_text(aes(
    start, mean, label=paste0("  ",state)),
    hjust=0, 
    data=seg.to.show)+
  theme_bw()+
  facet_grid(distance.shots ~ ., labeller=label_both)
png("TestRegion-segmentation.png", width=8, height=11, units="in", res=200)
print(gg.segs)
dev.off()

gg.img <- ggplot()+
  ggtitle(paste0("Predictions from graph-constrained optimal changepoint algorithm with penalty=", penalty))+
  theme_bw()+
  coord_cartesian(expand=FALSE)+
  scale.fill+
  geom_tile(aes(
    distance.shots, index.from.bottom, fill=intensity),
    data=TestRegion.tall)+
  geom_vline(aes(
    xintercept=distance.shots),
    alpha=0.5,
    data=dist.hilite)+
  geom_point(aes(
    distance.shots, start, color=state),
    data=seg.to.show,
    shape=21,
    fill=NA)
png("TestRegion-predictions.png", width=8, height=4, units="in", res=200)
print(gg.img)
dev.off()
