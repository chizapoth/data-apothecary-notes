# roc curve

library(ROCR)
# use data
data("ROCR.simple")
ROCR.simple

df <- data.frame(ROCR.simple)
pred <- prediction(df$predictions, df$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)


# if in stata, need to hand craft tpr fpr for differennt cutoffs

# first try 10 cutoffs 

df |> head()


# for cut off 0.1, record tpr fpr
# for cut off 0.2, record tpr fpr 

compute_tpf_fpr(cutoff = 0.1, pred = df$predictions, true_label = df$labels)

res_list <- list()

cutoffs <- seq(0.05, 0.95, by = 0.05)
cutoffs

for(i in 1:length(cutoffs)){
  res_list[[i]] <- compute_tpf_fpr(cutoff = cutoffs[i], 
                                   pred = df$predictions, 
                                   true_label = df$labels)
  cat(i, '\n')
}

res_list
df_to_plot <- do.call(rbind, res_list)

plot(df_to_plot$fpr, df_to_plot$tpr, type = 'b')

# export data
library(haven)
write_dta(df, "~/Documents/GitHub/teaching/MF9130/v24/rscript/data_roc_1.dta")
write_dta(df_to_plot, "~/Documents/GitHub/teaching/MF9130/v24/rscript/data_roc_2.dta")



# util ----

compute_tpf_fpr <- function(cutoff, pred, true_label){
  
  #true_label <- df$labels
  #pred <- df$predictions
  # cutoff <- 0.1
  # tpr: sensitivity, tp/(tp+fn) = tp/p
  # fpr: 1-specificity, fp/(fp+tn) = fp/n
  number_p <- sum(true_label == 1)
  number_n <- sum(true_label == 0)
  
  pred_label <- ifelse(pred <= cutoff, 0, 1)
  
  # get tp
  id_p <- which(true_label == 1)
  id_n <- which(true_label == 0)
  
  number_tp <- sum(pred_label[id_p] == 1)
  number_fp <- sum(pred_label[id_n] == 1)
  
  tpr <- number_tp / number_p
  fpr <- number_fp / number_n
  
  res_df <- data.frame(cutoff = cutoff, 
                       tpr = tpr, 
                       fpr = fpr)
  return(res_df)
}



