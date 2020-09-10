load("/Volumes/qac/qac201/Studies and Codebooks/GSS/DATA/GSS_all.RData")

library(descr)

gss14 <- gss[gss$YEAR == 2014, c("ID", "HAPPY", "MAJOR1", "MAJOR2", "AGE", "SEX", "RACE", "CLASS")]

library(car)

gss14$HAPPY <- recode(gss14$HAPPY, "1=3; 3=1; 8 = NA; 9 = NA")

gss14$AGE <- recode(gss14$AGE, "98 = NA; 99 = NA")

gss14$CLASS <- recode(gss14$CLASS, "8 = NA; 9 = NA")

stem <- list(41, 11, 8, 33, 69, 14, 42, 4, 15, 18, 24, 27, 34, 36, 38, 48, 51, 58, 68, 69, 71, 72, 76, 80)

gss14$MAJOR1 <- recode(gss14$MAJOR1, "0 = NA")
gss14$MAJOR2 <- recode(gss14$MAJOR2, "0 = NA")

gss14$STEM <- ifelse(gss14$MAJOR1 %in% stem | gss14$MAJOR2 %in% stem, "yes", "no")


by(gss14,
   gss14$SEX,
   function(x) list( chisq.test(x$HAPPY, x$STEM), 
                     chisq.test(x$HAPPY, x$STEM)$observed, 
                     prop.table(chisq.test(x$HAPPY, x$STEM)$observed, 2)))















