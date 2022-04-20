TestRegion <- data.table::fread("TestRegion.csv", nrows=3000)
library(ggplot2)
TestRegion[, distance := 1:.N]
TestRegion.tall <- data.table::melt(TestRegion,id="distance")
TestRegion.tall[, depth := as.integer(sub("V","", variable))]

ggplot()+
  scale_fill_gradient(low="white", high="red")+
  coord_equal()+
  geom_tile(aes(
    distance, depth, fill=value),
    data=TestRegion.tall)
