make_param_set = function(dt, subset = NULL) {
  param_list = lapply(names(dt), function(col_name){
    column = dt[[col_name]]
    if (is.numeric(column)) {
      lb = if (col_name %in% names(subset) && !is.na(subset[[col_name]][1])) subset[[col_name]][1] else min(column)
      ub = if (col_name %in% names(subset) && !is.na(subset[[col_name]][2])) subset[[col_name]][2] else max(column)
      if (is.double(column)){
        param = p_dbl(lower = lb, upper = ub)
      } else if (is.integer(column)) {
        param = p_int(lower = lb, upper = ub)
      }
    } else {
      if (is.character(column)) {
        lev = if (col_name %in% names(subset)) subset[[col_name]] else unique(column)
      } else {
        lev = if (col_name %in% names(subset)) as.character(subset[[col_name]]) else levels(column)[unique(column[!is.na(column)])]
      }
      param = p_fct(levels = lev)
    }
    param
  })
  names(param_list) = names(dt)
  ps = do.call(paradox::ps, param_list)
  ps$extra_trafo = function(x, param_set, predictor) {
    if (is.null(predictor)) {
      stop("trafo() of parameter set needs a 'predictor' input")
    }
    factor_cols = names(which(sapply(predictor$data$X, is.factor)))
    for (factor_col in factor_cols) {
      fact_col_pred = predictor$data$X[[factor_col]]
      value =  factor(x[[factor_col]], levels = levels(fact_col_pred), ordered = is.ordered(fact_col_pred))
      set(x, j = factor_col, value = value)
    }
    return(x)
  }
  return(ps)
}

update_box = function(current_box, j, lower = NULL, upper = NULL, val = NULL, complement = TRUE) {
  new_box = current_box$clone(deep = TRUE)
  if (!is.null(lower) && !is.na(lower)) {
    new_box$subspaces()[[j]]$lower[[1]] = lower
  }

  if (!is.null(upper) && !is.na(upper)) {
    new_box$subspaces()[[j]]$upper[[1]] = upper
  }

  if (all(!is.null(val)) && all(!is.na(val))) {
    if (complement) {
      val = unique(c(new_box$subspaces()[[j]]$levels[[1]], val))
    }
    new_box$subspaces()[[j]]$levels[[1]] = val
  }
  return(new_box)
}

evaluate_box = function(box, x_interest, predictor, n_samples, desired_range, strategy = "random") {
  yhat_interest = predictor$predict(x_interest)[[1]]
  ## generate new data
  if (strategy == "random") {
    dt = SamplerUnif$new(box)$sample(n = n_samples)$data
    dt = box$extra_trafo(dt, predictor = predictor)
  } else if (strategy == "extremes") {
    low = private$box$lower
    low = low[!is.na(low)]
    up = private$box$upper
    up = up[!is.na(up)]
    val = data.frame(rbind(low, up))
    vall = as.list(val)
    lev = private$box$levels
    lev[sapply(lev, is.null)] <- NULL
    l = c(lev, vall)
    dt = data.table(expand.grid(l))
  }
  dt$pred = predictor$predict(dt)
  ## reuse generated one
  # private$check_in_box()
  # evaluate according to impurity --> lower better
  impurity = nrow(dt[!pred %between% desired_range])/n_samples
  # evaluate according to distance to pred --> lower better
  dist = mean(abs(dt$pred - yhat_interest))
  return(c(impurity = impurity, dist = dist))
}


make_surface_plot = function(box, param_set, grid_size, predictor, x_interest, feature_names, surface = "prediction", desired_range = NULL) {

  param_set_sub = param_set$clone()$subset(feature_names)
  dt_grid = make_ice_curve_area(predictor, x_interest, grid_size, param_set_sub, surface = surface, desired_range = desired_range)
  x_feat_name = feature_names[1L]
  y_feat_name = feature_names[2L]

  if (param_set_sub$all_numeric) {
    p = ggplot2::ggplot(data = dt_grid, ggplot2::aes_string(x = x_feat_name, y = y_feat_name)) +
      ggplot2::geom_tile(ggplot2::aes_string(fill = "pred")) +
      ggplot2::geom_rug(ggplot2::aes_string(x = x_feat_name, y = y_feat_name), predictor$data$X, alpha = 0.2,
        position = ggplot2::position_jitter(), sides = "bl") +
      ggplot2::guides(z = ggplot2::guide_legend(title = "pred")) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "right")
    p = p + ggplot2::geom_rect(xmin=box$lower[[x_feat_name]],
      xmax=box$upper[[x_feat_name]],
      ymin=box$lower[[y_feat_name]],
      ymax=box$upper[[y_feat_name]], color="yellow", fill = NA, alpha = .3)
    p = p + ggplot2::geom_point(data = x_interest, ggplot2::aes_string(x = x_feat_name, y = y_feat_name),colour = "white")

  } else if (param_set_sub$all_categorical) {
    p = ggplot2::ggplot(dt_grid, ggplot2::aes_string(x_feat_name, y_feat_name)) +
      ggplot2::geom_tile(ggplot2::aes_string(fill = "pred")) +
      ggplot2::geom_point(ggplot2::aes_string(x_feat_name, y_feat_name), x_interest, color = "white") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "pred")) +
      ggplot2::theme_bw()
    ### get value combinations in box
    frames = expand.grid(box$levels[[x_feat_name]], box$levels[[y_feat_name]])
    names(frames) = c(x_feat_name, y_feat_name)
    frames[[x_feat_name]] = as.integer(factor(frames[[x_feat_name]], levels = levels(dt_grid[[x_feat_name]])))
    frames[[y_feat_name]] = as.integer(factor(frames[[y_feat_name]], levels = levels(dt_grid[[y_feat_name]])))

    for (r in seq_len(nrow(frames))) {
      p = p + ggplot2::geom_rect(xmin=frames[r, x_feat_name] - 0.4,
        xmax=frames[r, x_feat_name] + 0.4,
        ymin=frames[r, y_feat_name] - 0.4,
        ymax=frames[r, y_feat_name] + 0.4, color="yellow", fill = NA, alpha = .3)
    }

  } else {
    cat_feature = feature_names[param_set_sub$is_categ]
    num_feature = setdiff(feature_names[1:2], cat_feature)
    dt_grid$pred = predictor$predict(dt_grid)[[1]]
    y_hat_interest = predictor$predict(x_interest)
    x_interest_with_pred = cbind(x_interest, pred = y_hat_interest[[1]])

    #### get obs in box
    col_num = dt_grid[[(num_feature)]]
    id = which(dt_grid[[(cat_feature)]] == box$params[[cat_feature]]$levels &
        col_num >= box$lower[[num_feature]] & col_num <= box$upper[[num_feature]])
    dt_sub = dt_grid[id,]
    ####

    p = ggplot2::ggplot(data = dt_grid, ggplot2::aes_string(x = num_feature, y = "pred", group = cat_feature, color = cat_feature)) +
      ggplot2::geom_line(inherit.aes = FALSE, data = dt_sub, ggplot2::aes_string(x = num_feature, y = "pred", group = cat_feature),
        color = "yellow", lwd = 3) +
      ggplot2::geom_line() +
      ggplot2::geom_rug(data = predictor$data$X, inherit.aes = FALSE, ggplot2::aes_string(x = num_feature), sides = "b") +
      ggplot2::theme_bw()

    p = p +
      ggplot2::geom_point(ggplot2::aes_string(x = num_feature, y = "pred"), x_interest_with_pred, colour = "black")

  }
  p
}


make_ice_curve_area = function(predictor, x_interest, grid_size, ps, surface, desired_range) {
  exp_grid = generate_design_grid(ps, grid_size)$data
  x_interest_sub = x_interest[, !names(x_interest) %in% names(ps$class), with = FALSE]
  instance_dt = x_interest_sub[rep(1:nrow(x_interest_sub), nrow(exp_grid))]
  grid_dt = cbind(instance_dt, exp_grid)
  grid_dt = ps$extra_trafo(grid_dt, predictor = predictor)
  if (surface == "prediction") {
    pred = predictor$predict(grid_dt)[[1]]
  } else if (surface == "range") {
    pred = predict_range(predictor, newdata = grid_dt, range = desired_range)
  }
  cbind(grid_dt, pred)
}

describe_box = function(box, digits = 2L) {
  capfactor = 10^digits
  results = as.data.table(box)
  results$levels = lapply(results$levels, function(lev) paste(lev, collapse = ", "))
  results$range = ifelse(!is.na(results$lower) & results$lower < results$upper,
    paste0("[", ceiling(results$lower * capfactor) / capfactor, ", ",
      floor(results$upper * capfactor) / capfactor, "]"), # CASE 1: lower < upper
    ifelse(!is.na(results$lower),
      paste0("{", results$lower, "}"), # CASE 2: lower = upper
      paste0("{", results$levels, "}") # CASE 3: values
    )
  )
  return(results[, .(id, range)])
}


predict_range = function(predictor, newdata, range) {
  prediction = predictor$predict(newdata)
  return(as.numeric(prediction >= range[1] & prediction <= range[2]))
}

#' version 1 = MAIRE paper (0.33 vs. 0.66 = one vs. all)
#' version 2 = my extended version (dummy encoded features)
#' version 3 = my MAIRE version (dummy encoded features and [0, 1] for numerical ones) --> hybrid of version 1 and version 2
transform_for_explanation = function(data, predictor, x_interest, version = 2, frac = 0.0001) {
  data = rbind(x_interest, data)
  expldata = data.table()
  categorylist = list()
  for (fnam in names(data)) {
    fcol = data[[fnam]]
    if (predictor$data$feature.types[fnam] == "categorical" & !is.ordered(fcol)) {
      if (version != 1) {
        ## my version: allows to have a subset of categories
        fcol = factor(fcol)
        lev = levels(fcol)
        if (length(unique(fcol)) == 2L) {
          fcol = data.table(as.numeric(fcol) - 1L)
          names(fcol) = fnam
        } else {
          if (length(unique(fcol)) != 1) {
            # one-hot encoding
            fcol = data.table(model.matrix(~0+fcol))
          } else {
            fcol = data.table(rep(1, nrow(data)))
          }
          names(fcol) = paste(fnam, lev, sep = "_")
        }
        # set to 0.66 vs. 0.33 (0.66)
        if (version == 3) {
         fcol = data.table(ifelse(fcol == 1, 2/3, 1/3))
         if (nrow(fcol) == 1 & nrow(fcol) < nrow(data)) {
           fcol = rbindlist(replicate(n = nrow(data), expr = fcol, simplify = FALSE))
         }
        }
      } else if (version == 1) {
        ## maire version: allows either class of x_interest or all
        fcol = data.table(ifelse(fcol == fcol[1], 0.66, 0.33))
        if (nrow(fcol) == 1 & nrow(fcol) < nrow(data)) {
           fcol = rbindlist(replicate(n = nrow(data), expr = fcol, simplify = FALSE))
         }
        names(fcol) = fnam
      }
    } else {
      if (predictor$data$feature.types[fnam] == "categorical") {
        if (!is.factor(fcol)) {
          fcol =  factor(fcol)
        }
        categorylist[[fnam]] = levels(fcol)
        fcol = as.numeric(fcol)
      }
      if (version %in% c(1, 3)) {
        ## transform numeric features to be between 0.1 and 0.9
        if (var(fcol) == 0) {
          fcol = data.table(rep(0.5, nrow(data)))
        } else {
          fcol =  data.table((fcol - min(fcol)) / (max(fcol) - min(fcol)) * (1 - 2*frac) + frac)
        }
      } else {
        fcol = data.table(fcol)
      }
      names(fcol) = fnam
    }
    expldata = cbind(expldata, fcol)
  }
  res = list(expldata = expldata[-1L,], explx_interest = expldata[1L,])
  attr(res, "categorylist") = categorylist
  return(res)
}

get_max_box = function (x_interest, fixed_features, predictor, param_set, desired_range, resolution =500L) {
  luval = lapply(predictor$data$feature.names, function(i_name) {
    val_name = x_interest[[i_name]]
    type_name = predictor$data$feature.types[[i_name]]
    if (i_name %in% fixed_features) {
      if (type_name == "categorical") return(val_name) else c(val_name, val_name)
      return(c(val_name, val_name))
    }
    ps_sub = param_set$clone(deep = TRUE)
    ps_sub = ps_sub$subset(i_name)
    grid1d = paradox:::generate_design_grid(ps_sub, resolution = resolution)$data
    x_interest_sub = data.table::copy(x_interest)
    x_interest_sub[, (i_name):=NULL]
    dt = data.table::data.table(grid1d, x_interest_sub)
    param_set$extra_trafo(dt, predictor = predictor)
    dt[, "pred"] = predictor$predict(dt)
    # select closest grid points to x_interest$i_name with a prediction outside desired range
    # If all grid point lower value of x_interest have a prediction within desired range --> lower = NA
    # same holds for upper
    if (type_name == "categorical") {
      dt = dt[pred %between% desired_range,]
      return(dt[[i_name]])
    } else {
      dt = dt[!pred %between% desired_range,]
      dt[,"dist"] = abs(dt[[i_name]] - val_name)
      dt = dt[order(dist),]
      lower = dt[eval(parse(text=i_name)) < val_name][1,][[(i_name)]]
      upper = dt[eval(parse(text=i_name)) > val_name][1,][[(i_name)]]
      return (c(lower, upper))
    }
  })
  names(luval) = predictor$data$feature.names
  # NA lower or upper values are filled with lower and upper of observed values
  param_set_update = make_param_set(rbind(x_interest, predictor$data$get.x()), subset = luval)
  return(param_set_update)
}

identify_in_box = function(box, data) {
  data = data.table::setDT(data)
  data = data[, box$data$id, with = FALSE]
  check_inbox = function(col, paramval) {
    if (paramval$class %in% c("ParamInt", "ParamDbl")) {
      col >= paramval$lower & col <= paramval$upper
    } else {
      col %in% paramval$levels[[1]]
    }
  }
  datainbox = data[, Map(check_inbox, .SD, box$subspaces()), .SDcols = names(data)]
  apply(datainbox, 1, all)
}

# surpress messages
quiet = function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

sampling = function(predictor, x_interest, fixed_features, desired_range, param_set, num_sampled_points = 1000L, strategy = "traindata") {

  largest_box = get_max_box(x_interest = x_interest, fixed_features = fixed_features,
    predictor = predictor, param_set = param_set, desired_range = desired_range)

  if (strategy == "traindata") {
    sampdata = copy(predictor$data$get.x())
    if (!is.null(fixed_features) & strategy == "traindata")  {
      sampdata = sampdata[, (fixed_features) :=
          x_interest[, fixed_features, with = FALSE]]
    }
    in_box = identify_in_box(largest_box, data = sampdata)
    sampdata = sampdata[in_box,]
  } else if (strategy == "sampled") {

    sampdata = SamplerUnif$new(largest_box)$sample(n = num_sampled_points)$data

    factor_cols = names(which(sapply(predictor$data$X, is.factor)))
    for (factor_col in factor_cols) {
      fact_col_pred = predictor$data$X[[factor_col]]
      value =  factor(sampdata[[factor_col]], levels = levels(fact_col_pred), ordered = is.ordered(fact_col_pred))
      set(sampdata, j = factor_col, value = value)
    }
  }
  return(sampdata)
}
