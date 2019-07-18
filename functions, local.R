# ensure the factor levels of the test set match those of the train set
match_levels = function(data, data_test){
  vars = colnames(data)
  for(var in vars){
    if(is.factor(data[,var])){
      levels(data_test[,var]) = levels(data[,var]) %>% 
        as.character() %>% 
        union(levels(data_test[,var]) %>% 
                as.character())
    }
  }
  return(data_test)
}

# removes variables, if they cannot yield a positive r-squared individually when included in a simple linear model
preprocess = function(data, variables_selected){
  remove = NULL
  for(i in 1:nrow(data)){
    if(colnames(data)[i] %in% variables_selected){
      print(colnames(data)[i])
      if(r2(data[, i], data$y) <= 0)
        remove = c(remove, i)
    }
  }
  return(data[, -remove])
}

# split numeric data by factors, creating new variables
split_data = function(data, category, numeric, data_test){
  index_category = which(colnames(data) == category)
  index_numeric = which(colnames(data) == numeric)
  LEVELS = levels(data[,index_category])
  
  df_new = NULL
  for(i in 1:length(LEVELS)){
    vector = ifelse(
      data[,index_category] == LEVELS[i],
      data[,index_numeric],
      0
    )
    vector = ifelse(
      vector %>% is.na(),
      0,
      vector
    )
    df_new = cbind(df_new, vector)
  }
  
  colnames(df_new) = LEVELS
  colnames(df_new) = paste(colnames(data)[index_category],'.', LEVELS, sep = "")
  data = cbind(data, df_new)
  data = data[,-c(index_category, index_numeric)]
  return(data)
}

# inspect new object

r2 = function(x, y){
  df = data.frame(x,y)
  df = df[which(!is.na(df$x)),]
  model = glm(y ~ x, data = df, na.action = na.omit)
  return(mycv.glm(df,model,10,1)$R2)
}

mycv.glm<-
  function (data, glmfit, K=10, seed=123) {
    #glmfit is glm fit with whole data
    #this function is to get the Cross-validated mean square error for regression
    #output R2 and MSE
    n <- nrow(data)
    set.seed(seed) #K=10
    
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated mean squared error
    
    CV=NULL; Rcv=NULL
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      glm.fit=glm(glmfit$call, data=data[train.index,])
      
      #observed test set y
      glm.y <- glmfit$y[test.index]
      #observed - predicted on test data
      error= glm.y - predict(glm.fit, newdata=data[test.index,])
      #mean squred error
      MSE <- mean(error^2)
      CV=c(CV,MSE)
      R=1-sum(error^2)/sum((glm.y-mean(glm.y))^2)
      Rcv=c(Rcv,R)
    }
    
    #Output
    list(call = glmfit$call, K = K, 
         MSE = mean(CV),R2=mean(Rcv), 
         seed = seed)  
    
  }

spearman = function(data, class, numeric){
  index_class = which(colnames(data) == class)
  index_numeric = which(colnames(data) == numeric)
  index_SalePrice = which(colnames(data) == 'SalePrice')
  
  data[,index_class] = data[,index_class] %>% as.factor()
  
  corr = NULL
  LEVELS = levels(data[,index_class])
  for(LEVEL in LEVELS){
    corr = c(
      corr,
      rcorr(data[data[,index_class] == LEVEL, c(index_numeric, index_SalePrice)] %>% as.matrix(), type = 'spearman')$r[1,2]
    )
  }
  result = data.frame(LEVELS, corr)
  result = result[order(-result$corr),]
  return(result)
}

ggfunction = function(data, class, numeric = NULL){
  index_class = which(colnames(data) == class)
  if(is.null(numeric)){
    gg = ggplot(data, aes(x = data[,index_class] %>% as.factor(), y = SalePrice))+
      geom_bar(stat = 'identity')
    print(gg)
  }
  else{
    index_numeric = which(colnames(data) == numeric)
    data = data[which(data[,index_numeric] != 0), ]
    gg = ggplot(data, aes(x = data[,index_numeric], y = SalePrice))+
      facet_wrap(~ data[,index_class])+
      geom_point()
    print(gg)
  }
  return(gg)
}

newVariable = function(data, current_variables, NTREE, seed = 1, maxnodes = NULL){
  set.seed(seed)
  # print("Let's do this!")
  MSE = NULL
  # calculate the number of variables to choose from, subtracting those already selected and the response
  n = ncol(data) - 1 - length(current_variables)
  if(!is.null(current_variables))
    variables_new = colnames(data %>% select(-y, -current_variables))
  else
    variables_new = colnames(data %>% select(-y))
  
  # create a string of a model with the currently selected variables in it
  variables_string = paste(current_variables, collapse = " + ")
  model = paste("y ~ ", variables_string, sep = "")
  
  for(i in 1:n){
    new_model = paste(
      c(model, variables_new[i]),
      collapse = " + "
    )
    print(new_model)
    if(is.null(maxnodes)){
      # print(maxnodes)
      mse = randomForest(
        new_model %>% as.formula(),
        data,
        mtry = length(current_variables) + 1,
        ntree = NTREE,
        replace = F
      )$mse %>% mean()
    }
    else{
      # print(maxnodes)
      mse = randomForest(
        new_model %>% as.formula(),
        data,
        mtry = length(current_variables) + 1,
        ntree = NTREE,
        replace = F,
        maxnodes = maxnodes
      )$mse %>% mean()
    }
    # mse %>% formatC(format = 'e') %>% print()
    MSE = c(MSE, mse)
  }
  results = data.frame(variables_new, MSE)
  results = results[order(results$MSE),]
  variable_new = results$variables_new[1]
  return(list(
    variables = c(current_variables, variable_new %>% as.character()),
    new_variable = variable_new,
    MSE = results$MSE[1]
  ))
  
}

forward_selection = function(data, ntree, seed = 1, maxnodes = NULL){
  # print(maxnodes)
  set.seed(seed)
  MSE_best = Inf
  current_variables = vector(mode = "character", length = 0)
  for(i in 1:(ncol(data) - 1)){
    result = newVariable(data, current_variables, ntree, 1, maxnodes)
    # print(result)
    if(result$MSE < MSE_best){
      current_variables = result$variables %>% as.character()
      MSE_best = result$MSE
      print(current_variables)
      print(MSE_best)
    }
    else
      break
  }
  return(list(
    variables = current_variables,
    MSE = MSE_best
  ))
}

ordinal_conversion_vector = function(v,y,v_test){
  df = data.frame(v,y)
  l = levels(v)
  y_by_level = rep(0,0)
  n = length(l)
  for(i in 1:n){
    y_by_level = c(
      y_by_level,
      ifelse(
        which(df$v == l[i]) %>% length() == 0,
        0,
        df[which(df$v == l[i]), 'y'] %>% median()
      )
    )
  }
  results = data.frame(l, y_by_level)
  results = results[order(results$y_by_level),]
  
  v = mapvalues(
    v,
    from = results$l,
    to = 1:n
  ) %>% as.integer()
  v_test = mapvalues(
    v_test,
    from = results$l,
    to = 1:n
  ) %>% as.integer()
  return(list(
    vec = v,
    vec_test = v_test
  ))
}

ordinal_conversion = function(data, data_test){
  var_names = colnames(data)
  n = ncol(data)
  for(i in var_names){
    if(is.factor(data[,i])){
      result = ordinal_conversion_vector(data[,i], data$y, data_test[,i])
      data[,i] = result$vec
      data_test[,i] = result$vec_test
    }
  }
  return(list(data = data,data_test = data_test))
}

impute_NA_factor_vector = function(v){
  v = ifelse(
    is.na(v),
    'na',
    v
  )
  return(v %>% as.factor())
}
impute_NA_factor = function(data){
  n = ncol(data)
  for(i in 1:n){
    if(is.factor(data[,i])){
      data[,i] = data[,i] %>% impute_NA_factor_vector()
    }
  }
  return(data)
}

impute_0 = function(data){
  n = ncol(data)
  for(i in 1:n){
    if(is.numeric(data[,i])){
      data[,i] = ifelse(
        is.na(data[,i]),
        0,
        data[,i]
      )
    }
  }
  return(data)
}

hasna = function(data){
  strings = NULL
  for(i in 1:ncol(data))
    if(data[,i] %>% is.na() %>% sum() > 0)
      strings = c(strings, colnames(data)[i])
    return(strings)
}
