
output_tree_info <- function(rfModel = rf_model){
  tree_info_list <- lapply(seq_len((rfModel$forest$num.trees)), function(i){
    tree_info <- treeInfo(rfModel, i)
    tree_info$tree_id <- i
    tree_info
    # plot_tree(tree_info, train_data_df=df_all2, rf_list=rfModel, dependent_var=y_varname, work_dir="results/figures/", plot_name=paste0('example_plot', i))
  })
  tree_infos <- do.call(rbind, tree_info_list)
  # Identify the most common split variables
  table(tree_info$splitvarName)
  summary(tree_info)

  # Calculate the depth of each node
  tree_info_list <- lapply(tree_info_list, function(tree_info){
    tree_info$depth <- 0
    for(i in 1:nrow(tree_info)){
    # tree_info[i,]$depth <- sum(tree_info[i,]$nodeID %in% c(tree_info$leftChild, tree_info$rightChild))
    leftchild_id <- which(tree_info$nodeID==tree_info[i,]$leftChild)
    rightchild_id <- which(tree_info$nodeID==tree_info[i,]$rightChild)
    if(i==1) tree_info[i,]$depth <- 1
    if(length(leftchild_id)!=0) tree_info[leftchild_id,]$depth <- tree_info[i,]$depth+1
    if(length(rightchild_id)!=0) tree_info[rightchild_id,]$depth <- tree_info[i,]$depth+1
    }
    tree_info
  })
  lapply(tree_info_list, function(tree_info){ max(tree_info$depth)}) %>% unlist %>% max

  tree_infos <- do.call(rbind, tree_info_list)
  tree_infos
}
