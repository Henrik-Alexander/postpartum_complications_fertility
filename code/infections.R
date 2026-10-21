###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: Investigate infections
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 11.12.2025
###


# Load the data ================================================================

# Load the conversion table
load("data/conversion_codes.Rda")

# Load the incidence data
load("data/incidence_combined.Rda")

# Classify the infectious codes ================================================

# List the infetions
icpc2_infections <- c(paste0("A", 70:78), paste0("B", 70:71), paste0("D", 70:73), 
                      paste0("F", 70:73), paste0("H", 70:74), paste0("K", 70:71),
                      "L70", paste0("R", 71:83), paste0("S", c("03", "09", 10:11, 70:76, 84:85)),
                      "T70", paste0("U", 70:72), paste0("W", 70:71), paste0("X", c(70:74, 90)),
                      paste0("Y", 70:76))


# Analyze the incidence ========================================================

inclusion_infection <- data.frame(icpc2_infections)
inclusion_infection$include <- NA


#icpc2_infections <- inclusion_infection$icpc2[inclusion_infection$include==1]

for (x in icpc2_infections) {
  cat("-------------------------\n")
  cat("Infection:", x, "\n")
  cat("Condition:", dt_incidence$disease[dt_incidence$icpc2==x], "\n")
  
  # Subbset the data
  tmp <- dt_incidence[icpc2==x, .(incidence_rate=diagnosis/exposure, time, prediction)]
  
  # Plot the different diseases
  tmp_plot <- ggplot(tmp, aes(x=time, y=prediction)) +
    geom_rect(aes(xmin=-9, xmax=0, ymin=0, ymax=max(tmp$incidence_rate), fill="Pregnancy"), alpha=0.2) +
    geom_rect(aes(xmin=0, xmax=12, ymin=0, ymax=max(tmp$incidence_rate), fill="Postpartum"), alpha=0.2) +
    geom_vline(xintercept=0) +
    geom_line(aes(y=prediction), size=1.3) +
    geom_point(aes(y=incidence_rate), alpha=0.3, size=2) +
    scale_x_continuous("Time relative to birth (in months)", breaks=seq(-24, 24, by=3), expand=c(0, 0)) +
    scale_y_continuous("Disease incidence (per 1000 person-months)", trans="log10", expand=c(0, 0)) +
    scale_fill_manual("Period:", values=c("lightblue", "pink")) +
    guides(colour="none") +
    theme(strip.text=element_text(size=8))

  print(tmp_plot)
  
  # Prompt an answer
  tmp_res <- as.numeric(readline("Include it in the analysis? Yes=1/No=0 "))
  
  # Assign the answer to the results
  inclusion_infection$include[inclusion_infection$icpc2_infections==x] <- tmp_res
  
}

# Merge with the conversion codes
inclusion_infection <- merge(conversion_codes, inclusion_infection, by.x="icpc2", by.y="icpc2_infections")
#write.csv(inclusion_infection, file="results/inclusion_infections.csv")

### END ########################################################################