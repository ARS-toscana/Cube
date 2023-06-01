Cube <- function(input, dimensions, levels, measures, statistics = NULL, computetotal = NULL,
                 rule_from_numeric_to_categorical = NULL, order = NULL, label = NULL, summary_threshold = NULL) {
  
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
  
  if (!is.null(order)) {
    for (dm in names(order)){
      if (length(order[[dm]]) == 1) {
        tmp <- unique(input[, unlist(c(names(order[[dm]]), order[[dm]]), use.names = F), with = F])
        setorderv(tmp, unlist(order[[dm]]))
        order[[dm]][[names(order[[dm]])]] <- unlist(tmp[, names(order[[dm]]), with = F], use.names = F)

      }
    }
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
  
  statistic_list <- list()
  measure_list <- c()
  measure_name_list <- c()
  
  measures <- setdiff(measures, names(statistics))
  
  for (measure in measures) {
    if (is.null(statistics)) {
      statistic_list <- append(statistic_list, parse(text = "do.call(sum, .SD)"))
      measure_value <- paste0(measure, "_sum")
      measure_list <- c(measure_list, measure_value)
      measure_name_list <- measure
      
      tmp <- data.table::groupingsets(input, jj = c(statistic_list),
                                      by = unlist(levels, use.names = F),
                                      sets = result.list,
                                      .SDcols = measure_name_list)
      setnames(tmp, "V1", measure_value)
      # setnames(tmp, paste0("V", seq_along(measures)), measure_list)
      
      tmp_2 <- if (!exists("tmp_2")) copy(tmp) else cbind(tmp_2, tmp)
      rm(tmp)
      
    } else {
      for (statistic in statistics[[measure]]) {
        measure_list <- paste(measure, statistic, sep = "_")
        measure_name_list <- measure
        if (!grepl("\\(", statistic)) {
          statistic <- paste0("do.call(", statistic, ", .SD)")
        }
        statistic <- parse(text = statistic)
        
        tmp <- data.table::groupingsets(input, j = c(eval(statistic)),
                                        by = unlist(levels, use.names = F),
                                        sets = result.list,
                                        .SDcols = measure_name_list)
        setnames(tmp, "V1", measure_list)
        # setnames(tmp, paste0("V", seq_along(measures)), measure_list)
        
        tmp_2 <- if (!exists("tmp_2")) copy(tmp) else cbind(tmp_2, tmp)
        rm(tmp)
      }
    }
  }
  
  input <- tmp_2
  rm(tmp_2)
  
  # Calculate the statistics
  # tmp <- data.table::groupingsets(input, j = c(eval(statistic_list)),
  #                                   by = unlist(levels, use.names = F),
  #                                   sets = result.list,
  #                                   .SDcols = measures)
  # setnames(input, paste0("V", seq_along(measures)), measure_name_list)
  
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
  setcolorder(input, c(measure_list, names(levels), order_cols))
  
  if (!is.null(order)) {
    for (dm in names(order)){
      ordered_list <- unlist(order[[dm]], use.names = F)
      ordered_list <- c(ordered_list, sort(setdiff(unique(input[[dm]]), ordered_list)))

      input <- merge(input,
                     data.table::data.table(ordered_list, seq_along(ordered_list)),
                     by.x = dm,
                     by.y = "ordered_list",
                     all.x = T)
      setorderv(input, "V2")
      
      setnames(input, "V2", paste(dm, "value-order", sep = "_"))
    }
  }
  
  level_label_names <- paste(names(levels), "label_value", sep = "-")
  setnames(input, names(levels), level_label_names)
  
  if (is.numeric(summary_threshold)) {
    
    summary_threshold <- as.integer(summary_threshold)
    tmp <- copy(input)
    
    for(measure in measure_list) {
      tmp[, (measure) := fifelse(get(measure) < summary_threshold & get(measure) > 0, F, T)] 
    }
    
    tmp <- tmp[, lapply(.SD, all), by = order_cols, .SDcols = measure_list]
    
    assign("summary_threshold", tmp, envir = parent.frame())
  }
  
  return(input)
}