library(tidyverse)
library(tidycensus)
library(sf)
library(stringr)
library(lubridate)
library(tigris)
library(here)
library(censusapi)
library(httr)
library(jsonlite)
library(testthat)

library(igraph)

install.packages("viridis")
library(viridis)

library(circlize)
install.packages("circlize")

library(haven)
yr5_movestate <- read_dta("5y_movestate.dta")

mig_count5 <- yr5_movestate |>
  mutate(state_comb = migsta5*100 + statefip) |>
  group_by(state_comb) |>
  summarize(n = n())

mig_count5 |>
  arrange(desc(n))

yr5_movestate |>
  select(year, statefip, migsta5) |>
  count(migsta5) |>
  arrange(desc(n))

library(haven)
state_cd <- read_dta("state_cent_d_v1.dta")


## state_sr1: for the not existing state combination of migration, n = "na"
state_sr1_y5 <- state_cd |>
  left_join(mig_count5, by = "state_comb")
state_sr2_y5 <- state_sr1_y5 |>
  mutate(mig_5yago = state1_name) |>
  mutate(mig_current = state2_name)

state_sr3_y5 <- state_sr2_y5 |>
  filter(is.na(n) == FALSE)

state_yr5_viz <- data.frame(
  from = c(state_sr3_y5$mig_5yago),
  to = c(state_sr3_y5$mig_current),
  weight = c(state_sr3_y5$n)
)

mig15_viz <- graph_from_data_frame(state_yr5_viz, directed = TRUE)
plot(mig15_viz)

layout_sy <- layout_with_sugiyama(mig15_viz)$layout
layout_kk <- layout_with_kk(mig15_viz)

plot(mig15_viz,
     edge.width = E(mig15_viz)$weight * 0.1,
     vertex.size = 5,
     vertex.color = "lightblue",
     vertex.label.color = "black",
     edge.color = "gray50",
     edge.arrow.size = 0.1
)

ecount(mig15_viz)
vcount(mig15_viz)

migin <- igraph::degree(mig15_viz, mode = "in")
migin_df <- tibble(
  node = names(migin),
  in_degree = as.numeric(migin)
)

migin_df |>
  write.csv("migin_df.csv", row.names = FALSE)

migout <- igraph::degree(mig15_viz, mode = "out")
migout_df <- tibble(
  node = names(migout),
  in_degree = as.numeric(migout)
)

migout_df |>
  write.csv("migout_df.csv", row.names = FALSE)


### TRY TO DRAW A HEAT MAP FOR THE MIGRATION ###


state_yr5_viz |>
  ggplot(mapping = aes(x = to, 
                       y = from)) +
  geom_tile(aes(fill = weight)) +
  scale_fill_viridis(
    option = "magma",
    limits = c(0,90),
    na.value = "gray80"  # <- 这句关键：obs == 0 的格子设置为灰色
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Migration Heatmap",
       caption = "Data: 2015 ASEC, IPUMS\n *States Combinations with no immigrants were left blank",
       x = "To (current)",
       y = "From (5 years ago)",
       fill = "Expected - Observed")

# NEW PACKAGE FOR THE Sankey diagram
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)

install.packages("viridis")
install.packages("circlize")
install.packages("networkD3")


#### SAMPLE CODE ####################################
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long$IDsource=match(data_long$source, nodes$name)-1
data_long$IDtarget=match(data_long$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name",
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)

# MY CODE #############
prep_sankey_viz15 <- state_sr3_y5 |>
  select(n, mig_5yago, mig_current, state1_fips, state2_fips) |>
  mutate(weight = n/10)

sankey_viz15 <- data.frame(
  source = c(prep_sankey_viz15$mig_5yago),
  target = c(prep_sankey_viz15$mig_current),
  value = c(prep_sankey_viz15$n)
)

nodes <- data.frame(name=c(as.character(sankey_viz15$source), 
                           as.character(sankey_viz15$target)) %>% unique())
sankey_viz15$IDsource=match(sankey_viz15$source, nodes$name)-1
sankey_viz15$IDtarget=match(sankey_viz15$target, nodes$name)-1

mig_network1 <- sankeyNetwork(Links = sankey_viz15, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name",
              sinksRight=FALSE,
              fontSize = 13)
#nodeWidth=3, fontSize=13, nodePadding=20

# Change the color of label text in Javascript
mig_network2 <- htmlwidgets::onRender(
  mig_network1,
  '
  function(el, x) {
    d3.selectAll(".node text").style("fill", "orange");
  }
  '
)

### Write out some dataframe
state_sr2_y5 |>
  write.csv("state_sr2_y5.csv", row.names = FALSE)

### Generate a 51*51 Matrix

state_sr2_y5_sh <- state_sr2_y5|>
  select(mig_5yago, mig_current, n)

flow_matrix <- state_sr2_y5_sh |>
  filter(!is.na(n)) %>%
  pivot_wider(names_from = mig_current, values_from = n, values_fill = 0) %>%
  group_by(mig_5yago) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  column_to_rownames("mig_5yago") %>%
  as.matrix()

sorted_states <- sort(rownames(flow_matrix))
flow_matrix <- flow_matrix[sorted_states, sorted_states]

### Chi-square test on the matrix

chisq_result <- chisq.test(flow_matrix, correct = FALSE)

chisq_texp <- chisq_result$expected
chisq_tobv <- chisq_result$observed

exp_df <- as.data.frame(as.table(chisq_texp))
obs_df <- as.data.frame(as.table(chisq_tobv))

names(exp_df) <- c("from", "to", "expected")
names(obs_df) <- c("from", "to", "observed")

chisq_results_df <- left_join(obs_df, exp_df, by = c("from", "to"))

chisq_expmore <- chisq_results_df |>
  filter(from != to) |>
  filter(expected > observed)
chisq_expmore2 <- chisq_expmore |>
  filter(observed != 0)

chisq_obvmore <- chisq_results_df |>
  filter(from != to) |>
  filter(expected < observed)

### Use the FULL CHI-SQ TEST RESULT for visualiztion
state_yr5_viz |>
  ggplot(mapping = aes(x = to, 
                       y = from)) +
  geom_tile(aes(fill = weight)) +
  scale_fill_gradient2(
    low = "royalblue", mid = "white", high = "orangered",
    midpoint = 0,
    limits = c(-20,20),
    na.value = "gray80"  # <- 这句关键：obs == 0 的格子设置为灰色
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Migration Heatmap (Gray = Zero Observed)",
       x = "To (current)",
       y = "From (5 years ago)",
       fill = "Expected - Observed")



library(tidyverse)

# 假设你的数据有这些列：out_state, in_state, obs_mig, pred_mig
chisq_viz <- chisq_results_df |>
  mutate(diff = expected - observed,
         fill_color = if_else(observed == 0, "gray90", as.character(diff)))

chisq_viz |>
  arrange(desc(diff))

# 将 diff 变成数值，fill_color 会是 numeric or character，ggplot 会自动处理
chisq_viz$fill_color <- ifelse(chisq_viz$observed == 0, 
                                       NA, chisq_viz$diff)
chisq_viz |>
  ggplot(mapping = aes(x = expected, y = observed)) +
  geom_tile(aes(fill = fill_color)) +
  scale_fill_gradient2(
    low = "skyblue", mid = "white", high = "firebrick",
    midpoint = 0,
    limits = c(-20,20),
    na.value = "gray80"  # <- 这句关键：obs == 0 的格子设置为灰色
  ) +
  theme_minimal() +
  labs(title = "Migration Heatmap (Gray = Zero Observed)",
       x = "Destination State",
       y = "Origin State",
       fill = "Observed - Predicted")

chisq_viz |>
  ggplot(mapping = aes(x = to, 
                       y = from)) +
  geom_tile(aes(fill = fill_color)) +
  scale_fill_gradient2(
    low = "royalblue", mid = "white", high = "orangered",
    midpoint = 0,
    limits = c(-20,20),
    na.value = "gray80" 
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Comparison between Expected and Actual Migration Flow",
       caption = "Data: 2015 ASEC\n * Grey indicates no observation of immigration between x and y state",
       x = "To (current)",
       y = "From (5 years ago)",
       fill = "Expected - Observed")


### A clustering Test

### Create a undirected network for the immigration
# mig15_viz_ur <- graph_from_data_frame(state_yr5_viz, directed = FALSE)

mig15_viz_ur <- as_undirected(mig15_viz, mode = "collapse")
#The command for the Clauset-Newman-Moore algorithm is: fastgreedy.community()
fgc_mig1015 <- cluster_fast_greedy(mig15_viz_ur)

#plot dendrogram
plot(as.dendrogram(fgc_mig1015))

#plot modularity - note in this case the x-index corresponds to the number of merges made to a network that starts as all isolates.
plot(fgc_mig1015$modularity)
## Count from right to left: 3 splits seems to be the best --- we would have 4 clusters

#this will give you a community vector for the highest modularity point.
membership(fgc_mig1015)

V(mig15_viz_ur)$color = membership(fgc_mig1015)
plot(mig15_viz_ur)
network::mixingmatrix(intergraph::asNetwork(KarateNetwork),"color")

### List the name of clusters
# fgc_mig1015 是 fast greedy 社区对象
membership_vec <- membership(fgc_mig1015)

# 转换为数据框
mig15_cluster <- data.frame(
  state = names(membership_vec),
  cluster_n = as.factor(membership_vec)
)

# 分组列出各社区州名

clustering_list <- mig15_cluster|>
  group_by(cluster_n) |>
  summarise(states = paste(state, collapse = ", "))

clustering_list |>
  write.csv("clustering_list.csv", row.names = FALSE)

print(clustering_list)

### Vizualize a Chord Diagram

state_cluster <- data.frame(
  state = names(membership_vec),
  cluster = as.factor(membership_vec)
)

# 原始数据假设为 your_df: from, to, weight
# 加入 from 和 to 的社区信息
mig15_cluster <- state_yr5_viz |>
  left_join(state_cluster, by = c("from" = "state")) |>
  rename(from_comm = cluster) |>
  left_join(state_cluster, by = c("to" = "state")) |>
  rename(to_comm = cluster)

cluster_flow <- mig15_cluster |>
  group_by(from_comm, to_comm) |>
  summarise(flow = sum(weight), .groups = "drop")

flow_matrix <- xtabs(flow ~ from_comm + to_comm, data = cluster_flow)

chordDiagram(flow_matrix)

chordDiagram(
  flow_matrix,
  annotationTrack = "none",  # ⛔ 关闭默认刻度/轴线
  preAllocateTracks = list(track.height = 0.05)  # 预留外圈标签空间
)

# 加上州名标签（垂直于圆边 + 方向美化）
circos.trackPlotRegion(
  track.index = 1,
  panel.fun = function(x, y) {
    sector.name = get.cell.meta.data("sector.index")
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    circos.text(
      x = mean(xlim),
      y = ylim[1] + 1,  # 控制离圆心距离
      labels = sector.name,
      facing = "clockwise",    # ⬅ 竖排、沿圆边
      niceFacing = TRUE,       # ⬅ 自动调整朝向，避免上下颠倒
      adj = c(0, 0.5),         # 文本位置微调
      cex = 0.5                # 字体大小
    )
  },
  bg.border = NA  # 关闭 panel 边框
)



#########################################################################

# 加入社区编号
state_yr5_viz$from_cluster <- membership_vec[state_yr5_viz$from]
state_yr5_viz$to_cluster <- membership_vec[state_yr5_viz$to]

# 聚合社区间总流量（合并边）
community_flow <- edge_df |>
  group_by(from_community, to_community) |>
  summarise(flow = sum(weight), .groups = "drop")

# 转换为矩阵形式
flow_matrix <- xtabs(flow ~ from_community + to_community, data = community_flow)

# 画 Chord Diagram
chordDiagram(flow_matrix)

