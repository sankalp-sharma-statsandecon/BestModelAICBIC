#' Find best model among all independent variables based on AIC or BIC values.
#' 
#' @param data,depvar,crit: data frame containing dependent and independent variables, dependent variable, choose AIC or BIC score.
#' @return A list file containing, best model, its AIC/BIC and adjusted R2
#' @export
bm_aic_bic <- function(data, depvar, crit = "AIC"){
  
  xfunc <- function(k, j_col){
    xnam <- combn(colnames(data)[colnames(data)!=depvar], m = j_col)[,k]
    fmla <- as.formula(paste(depvar," ~ ", paste(xnam, collapse= "+")))
    
    if(crit == "AIC"){
      return(c("AIC" = AIC(lm(fmla,data=data)),
               lm(fmla,data=data)$coefficients,
               "Adj_R_squared" = summary(lm(fmla,data=data))$adj.r.squared))
    } else {
      return(c("BIC" = BIC(lm(fmla,data=data)),
               lm(fmla,data=data)$coefficients,
               "Adj_R_squared" = summary(lm(fmla,data=data))$adj.r.squared))
    }
  }
  
  result <- lapply(1:length(names(data)[names(data)!=depvar]), 
                   function(j) lapply(1:ncol(combn(x = names(data)[names(data)!=depvar], m = j)), 
                                      function(i) xfunc(k = i, j_col = j)))
  
  res_vec <- c()
  i_count <- c()
  j_count <- c()
  
  for (i in 1:length(result)){
    for(j in 1:lengths(result)[i]){
      res_vec <- c(res_vec,result[[i]][[j]][[1]])
      i_count <- c(i_count, i)
      j_count <- c(j_count, j)
    }
  }
  res_df <- cbind("Score" = res_vec, "i" = i_count, "j" = j_count)
  ans <- t(data.frame("Values"=
                        result[[res_df[which.min(res_df[,1]),][-1][[1]]]][[res_df[which.min(res_df[,1]),][-1][[2]]]]))
  list_ans <- list(crit = ans[,1], "Select_model" = ans[,-c(1,length(ans))], "Adj_R2" = ans[,length(ans)])
  names(list_ans)[1] <- crit
  return(list_ans)
}
