score_items <- function(df) {
  
  # score items from judiths questionaire
   
  require(car)
  
  df = df[c("SS04_01", "SS04_02", "SS04_03", "SS04_06", "SS08_01", "SS08_02", "SS08_03", "SS08_04", 
            "SS05_05", "SS03_01", "SS03_02", "SS03_03", "SS03_04", "SB01_01", "SB01_03", "SB01_04", 
            "SB01_02", "SS02", "SB04_01")]
  

  df$SS04_01 <- recode(df$SS04_01, "1=0; 2=0; 3=0; 4=1; 5=2; else=NA")
  df$SS04_02 <- recode(df$SS04_02, "1=0; 2=0; 3=0; 4=1; 5=2; else=NA")
  df$SS04_03 <- recode(df$SS04_03, "1=0; 2=0; 3=0; 4=1; 5=2; else=NA")
  df$SS04_06 <- recode(df$SS04_06, "1=0; 2=0; 3=0; 4=1; 5=2; else=NA")
  
  df$SS08_01 <- recode(df$SS08_01, "1=2; 2=1; 3=0; 4=0; 5=0; else=NA")
  df$SS08_02 <- recode(df$SS08_02, "1=0; 2=0; 3=0; 4=1; 5=2; else=NA")
  df$SS08_03 <- recode(df$SS08_03, "1=0; 2=0; 3=0; 4=1; 5=2; else=NA")
  df$SS08_04 <- recode(df$SS08_04, "1=0; 2=0; 3=0; 4=1; 5=2; else=NA")
  
  df$SS05_05 <- recode(df$SS05_05, "1=2; 2=1; 3=0; 4=0; 5=0; else=NA")
  
  df$SS03_01 <- recode(df$SS03_01, "1=0; 2=0; 3=0; 4=1; 5=2; else=NA")
  df$SS03_02 <- recode(df$SS03_02, "1=2; 2=1; 3=0; 4=0; 5=0; else=NA")
  df$SS03_03 <- recode(df$SS03_03, "1=2; 2=1; 3=0; 4=0; 5=0; else=NA")
  df$SS03_04 <- recode(df$SS03_04, "1=0; 2=0; 3=0; 4=1; 5=2; else=NA")
  
  df$SB01_01 <- recode(df$SB01_01, "1=2; 2=1; 3=0; 4=0; 5=0; else=NA")
  df$SB01_03 <- recode(df$SB01_03, "1=0; 2=0; 3=0; 4=1; 5=2; else=NA")
  df$SB01_04 <- recode(df$SB01_04, "1=2; 2=1; 3=0; 4=0; 5=0; else=NA")
  
  df$SB01_02 <- recode(df$SB01_02, "1=2; 2=1; 3=0; 4=0; 5=0; else=NA")
  
  df$SS02 <- recode(df$SS02, "1=0; 2=0; 3=0; 4=1; 5=2; else=NA")
  
  df$SB04_01 <- recode(df$SB04_01, "1=0; 2=0; 3=0; 4=1; 5=2; else=NA")
  
  
  
  
  
  
  
  return(df)
  
}