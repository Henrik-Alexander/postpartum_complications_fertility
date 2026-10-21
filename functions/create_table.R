clean_varnames <- function(var_names) {
  
  # Clean the intercept
  var_names[var_names == "(Intercept)"] <- "Intercept"
  
  # Clean the  age group variable
  var_names <- str_replace(var_names, "age_group", "Age: ")
  var_names <- str_remove(var_names, "\\(")
  var_names <- str_replace(var_names, ",", "-")
  
  # Upper end of intervals
  upper_end <- str_extract(var_names, "[0-9]{2}\\]")
  var_names <- str_remove(var_names, "[0-9]{2}\\]")
  var_names[!is.na(upper_end)] <- paste0(var_names[!is.na(upper_end)], as.numeric(str_extract(upper_end[!is.na(upper_end)], "\\d{2}")) - 1)
  
  # Clean the year of first birth variable
  var_names <- str_replace(var_names, "year_firstbirth", "Year: ")
  var_names <- str_replace(var_names, "income_decile", "Income quantile: ")
  var_names <- str_replace(var_names, "edu_level", "Education: ")
  var_names <- str_replace(var_names, "delivery_mode", "Delivery: ")
  var_names <- str_replace(var_names, "any_pregnancy", "Pregnancy complications")
  var_names <- str_replace(var_names, "relationship", "Union status: ")
  var_names <- str_replace(var_names, "migrant_cat", "Migration status: ")
  var_names <- str_replace(var_names, "any_postnatal", "Postnatal complication")
  
  
  return(var_names)
  
}



create_table <- function(models, out = NULL, exp = FALSE, digits=3) {
  
  # Create model names
  model_names <- paste("Model", 1:length(models))
  
  # Clean coefficients
  clean_coeffs <- function(coefficients, exponentiate) {
    
    if (exponentiate) {
      coefficients$estimate <- exp(coefficients$estimate)
      coefficients$conf.low <- exp(coefficients$conf.low)
      coefficients$conf.high <- exp(coefficients$conf.high)
    }
    
    
    # Create the coefficients
    c <- round(coefficients$estimate, digits = digits)
    
    # Create starts
    stars <- rep("", nrow(coefficients))
    stars[coefficients$p.value < 0.05] <- "*"
    stars[coefficients$p.value < 0.01] <- "**"
    
    # Create the confidence intervals
    conf_int <- paste0(" (", round(coefficients$conf.low, digits), ", ",  round(coefficients$conf.high, digits), ")")
    
    # Return the coefficients
    coeff <- paste0(c, stars, conf_int)
    names(coeff) <- coefficients$term
    
    return(coeff)

  }
  
  # Bring the coefficients together
  combine_coeffs <- function(coeffs) {
    # Which is the longest
    lenghts <- sapply(coeffs, length)
    longest <- which.max(lenghts)
    
    f_tab <- data.frame(Variable = clean_varnames(names(coeffs[[longest]])))

    for (i in 1:length(coeffs)) {
      f_tab[[i+1]] <- ""
      f_tab[1:lenghts[i], i+1] <- coeffs[[i]]
    }
    return(f_tab)
    
  }
  
  # Create single table
  coeffs <- lapply(models, tidy, conf.int = TRUE)
  coeffs <- lapply(coeffs, clean_coeffs, exponentiate = exp)
  coeffs <- combine_coeffs(coeffs)
  colnames(coeffs) <- c("Variable", model_names)
  
  # Create the model diagnostiics
  model_fit <- lapply(models, glance)
  
  # Select the diagnostics
  diagnostics <- lapply(model_fit, function(model) {
    data.frame(
      AIC = as.character(round(model$AIC, digits)),
      BIC = as.character(round(model$BIC, digits)),
      N = as.character(round(model$nobs, digits))
    )
  })
  diagnostics <- t(bind_rows(diagnostics))
  diagnostics <- as.data.frame(diagnostics)
  colnames(diagnostics) <- c(model_names)
  diagnostics$Variable <- rownames(diagnostics)
  
  # Combine coefficients and diagnostics
  final_table <- bind_rows(coeffs, diagnostics)
  
  
  if (is.null(out)) {
    return(final_table)
    
  } else {
    
    # Create a new workbook
    wb <- createWorkbook()
    
    ws <- addWorksheet(wb, "Regression table")
    
    # Set column widths
    setColWidths(wb, sheet = ws, cols = 1:ncol(final_table), widths = rep(25, ncol(final_table)))
    
    headerStyle <- createStyle(
      fontName = "Calibri",
      fontSize = 11,
      textDecoration = "Bold",
      halign = "center",
      valign = "center",
      border = "TopBottom "
    )
    
    # Write the header
    writeData(wb, sheet = ws, x = final_table, startRow = 1, startCol = 1, colNames = TRUE, headerStyle = headerStyle)
    # Write the table
    writeData(wb, sheet = ws, x = final_table, startRow = 1, startCol = 1, colNames = TRUE)

    # Add the footnote
    footnote <- "Note: * p < 0.05, ** p < 0.01"
    writeData(wb, sheet = ws, x = footnote, startRow = nrow(final_table)+2, startCol = 1, colNames = FALSE)
    
    saveWorkbook(wb, file = out, overwrite = TRUE)
    
  }
}


latex_table <- function(models, out, title, apply.coef, ci, type, label) {
  
  
  # Create the covariate labels
  longest <- lapply(models, function(x) length(coef(x)))
  
  labels <- lapply(models, function(x) names(coef(x)))[[which.max(longest)]]
  labels <- clean_varnames(labels)
  labels <- c(labels[2:length(labels)], labels[1])
  
  # Print the regression table
  stargazer(models,
            ci=ci,
            apply.coef=apply.coef,
            type=type, 
            label=label,
            title=title,
            covariate.labels = labels,
            out=out)
  
  
}
  
  


