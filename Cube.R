Cube <- function(input, dimensions, levels, measures, statistics = NULL, computetotal = NULL,
                 rule_from_numeric_to_categorical = NULL, order = NULL, label = NULL) {
  
  # Calculate all possible combination of the dimensions
  dimensions_combinations <- c()
  for (i in seq_along(levels)) {
    dimensions_combinations <- c(dimensions_combinations, combn(names(levels), i, simplify = F))
  }
  
  # Calculate all possible combination of levels
  result.list <- c()
  for (dm_comb in dimensions_combinations) {
    test <- c()
    for (dm in dm_comb) {
      test <- append(test, list(lapply(levels[[dm]],
                                       function(x) levels[[dm]][which(levels[[dm]] == x):length(levels[[dm]])])))
    }
    result.df <- expand.grid(test)
    result.list <- unique(c(result.list, lapply(apply(result.df, 1, identity), unlist, use.names = F)))
  }
  
  # Add new variables based on existing levels
  for (dm in names(levels)) {
    for (new_var in names(assigned_rule[[dm]])) {
      detail_new_var <- assigned_rule[[dm]][[new_var]]
      if (detail_new_var[[1]] == "split_in_bands") {
        
        cut_labels <- c()
        for (i in 1:(length(detail_new_var[[3]]) - 1)) {
          cut_labels <- c(cut_labels, paste(detail_new_var[[3]][[i]], detail_new_var[[3]][[i + 1]] - 1, sep = "-"))
        }
        
        input <- input[, (new_var) := as.character(cut(get(..detail_new_var[[2]]), detail_new_var[[3]], cut_labels, right = F))]
      }
    }
  }
  
  # Calculate the statistics
  input <- data.table::groupingsets(input, j = list(eval(parse(text = 'do.call("sum", .SD)'))),
                                    by = unlist(levels, use.names = F),
                                    sets = result.list,
                                    .SDcols = measures)
  setnames(input, paste0("V", seq_along(measures)), measures)
  
  # Filter to remove total for not necessary columns
  not_computetotal <- setdiff(names(levels), computetotal)
  max_level <- lapply(levels, function(x) x[[length(x)]])
  input <- na.omit(input, unlist(max_level[not_computetotal]))
  
  # For each dimension create an order column with max as 99. In case need to compute the total create a new category 
  order_cols <- c()
  for (dm in names(levels)) {
    new_col <- paste(dm, "order", sep = "_")
    
    input[, (new_col) := rowSums(is.na(.SD)) + 1, .SDcols = levels[[dm]]]
    
    if (dm %in% computetotal) {
      input[is.na(get(dm)), (dm) := paste0("All", dm)]
    } 
    
    input[get(new_col) == max(get(new_col)), (new_col) := 99]
    order_cols <- c(order_cols, new_col)
  }
  
  # Find the first non NA value for multiple levels dimensions
  multiple_levels <- levels[sapply(levels, function(x) length(x) > 1)]
  input <- input[, (names(multiple_levels)) := lapply(multiple_levels, function(x) {
    fcoalesce(input[, lapply(.SD, as.character), .SDcols = x])
  })]
  
  # Remove unnecessary columns and reorder the remaining ones
  input[, (unlist(multiple_levels)) := NULL]
  setnames(input, names(levels), paste(names(levels), "label_value", sep = "-"))
  setcolorder(input, c(measures, names(levels), order_cols))
  
  return(input)
}