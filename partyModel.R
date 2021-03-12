library(randomForest)

# Uncomment for use in tree_func near bottom
# library(ggraph)
# library(igraph)

source("modelPreprocessing.R")
source("stringManipulator.R")

set.seed(42)
sample <- caTools::sample.split(dfScaled$party, SplitRatio = 0.8)

train <- subset(
  dfScaled[, !names(dfScaled) %in% c("flipped"), drop = F], sample == TRUE
  )

test  <- subset(
  dfScaled[, !names(dfScaled) %in% c("flipped"), drop = F], sample == FALSE
  )

set.seed(42)
model <- randomForest(party ~ ., data = train)

train.preds <- data.frame(
  predicted = model$predicted, actual = train$party
)

test.preds <- model %>% predict(test %>% select(1:65))

test.preds <- data.frame(
  predicted = test.preds, actual = testPA$party
)

# testCM <- yardstick::conf_mat(
#   testPreds, truth = actual, estimate = predicted
# )
# 
# summary(testCM)

# accuracy     <- 0.826
# bal_accuracy <- 0.827
# precision    <- 0.787
# recall       <- 0.881
# f_meas       <- 0.831

preds <- bind_rows(train.preds, test.preds) %>%
  select(predicted, actual)

preds$district <- row.names(preds)
row.names(preds) <- 1:length(preds$district)

paStates <- predState(preds$district)
paDistricts <- predDistrict(preds$district)

preds$state <- paStates
preds$district <- paDistricts
preds <- preds[, c(4, 3, 2, 1)]

varImportances <- importance(model)
topTen <- varImportances[varImportances > 4, ] %>% sort(decreasing = TRUE)
topTen <- as.data.frame(topTen)
colnames(topTen)[1] <- "Importances"
rownames(topTen) <- c(
  "Walking/Public Transit",
  "Renter Occupied Units",
  "Natural-born Citizens",
  "Commuters by Car",
  "Owner Occupied Units",
  "Foreign-born Citizens",
  "Whites",
  "Asians",
  "Persons of Other Race",
  "Median Rent"
  )

# tree_func <- function for mapping 
# source: https://shiring.github.io/machine_learning/2017/03/16/rf_plot_ggraph

# tree_func <- function(final_model, tree_num) {
#   
#   # get tree by index
#   tree <- randomForest::getTree(final_model, k = tree_num, labelVar = TRUE) %>%
#     tibble::rownames_to_column() %>%
#     # make leaf split points to NA, so the 0s won't get plotted
#     mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
#   
#   # prepare data frame for graph
#   graph_frame <- data.frame(
#     from = rep(tree$rowname, 2),
#     to = c(tree$`left daughter`, tree$`right daughter`)
#     )
#   
#   # convert to graph and delete the last node that we don't want to plot
#   graph <- graph_from_data_frame(graph_frame) %>%
#     delete_vertices("0")
#   
#   # set node labels
#   V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
#   V(graph)$leaf_label <- as.character(tree$prediction)
#   V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
#   
#   # plot
#   plot <- ggraph(graph, 'dendrogram') + 
#     theme_bw() +
#     geom_edge_link() +
#     geom_node_point() +
#     geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
#     geom_node_label(
#       aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white"
#       ) +
#     geom_node_label(
#       aes(label = leaf_label, fill = leaf_label),
#       na.rm = TRUE,
#       repel = TRUE,
#       colour = "white",
#       fontface = "bold",
#       show.legend = FALSE
#       ) +
#     theme(
#       panel.grid.minor = element_blank(),
#       panel.grid.major = element_blank(),
#       panel.background = element_blank(),
#       plot.background = element_rect(fill = "white"),
#       panel.border = element_blank(),
#       axis.line = element_blank(),
#       axis.text.x = element_blank(),
#       axis.text.y = element_blank(),
#       axis.ticks = element_blank(),
#       axis.title.x = element_blank(),
#       axis.title.y = element_blank(),
#       plot.title = element_text(size = 18)
#       )
#   
#   print(plot)
# }
# 
# treeNum <- which(model$forest$ndbigtree == min(model$forest$ndbigtree))
# 
# for (num in treeNum) {tree_func(final_model = model, tree_num = num)}

