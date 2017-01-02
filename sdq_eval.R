sdq_eval <- function(df) {
  
  # sdq evaluation
  # adaptation of the official R function to evaluate the sdq questionnaire
  
  require(car)
  
  # emotion scale
  somatic <- recode(df$PB01_03, "1=0; 2=1; 3=2; else=NA")
  worries <- recode(df$PB01_08, "1=0; 2=1; 3=2; else=NA")
  unhappy <- recode(df$PB02_05, "1=0; 2=1; 3=2; else=NA")
  clingy <- recode(df$PB02_08, "1=0; 2=1; 3=2; else=NA")
  afraid <- recode(df$PB03_08, "1=0; 2=1; 3=2; else=NA")
  
  df.emotion <- data.frame(somatic, worries, unhappy, clingy, afraid)
  nemotion <- apply(df.emotion, 1, function(x) sum(is.na(x)))
  emotion <- ifelse(nemotion<3, rowMeans(df.emotion, na.rm=TRUE), NA)
  emotion <- as.numeric(emotion) * 5
  emotion <- floor(0.5 + emotion)
  
  # conduct scale
  tantrum <- recode(df$PB01_05, "1=0; 2=1; 3=2; else=NA")
  obeys <- recode(df$PB01_07, "1=2; 2=1; 3=0; else=NA")
  fights <- recode(df$PB02_04, "1=0; 2=1; 3=2; else=NA")
  lies <- recode(df$PB03_02, "1=0; 2=1; 3=2; else=NA")
  steals <- recode(df$PB03_06, "1=0; 2=1; 3=2; else=NA")
  
  df.conduct <- data.frame(tantrum, obeys, fights, lies, steals)
  nconduct <- apply(df.conduct, 1, function(x) sum(is.na(x)))
  conduct <- ifelse(nconduct<3, rowMeans(df.conduct, na.rm=TRUE), NA)
  conduct <- as.numeric(conduct) * 5
  conduct <- floor(0.5 + conduct)
  
  # hyperactivity scale
  restless <- recode(df$PB01_02, "1=0; 2=1; 3=2; else=NA")
  fidgety <- recode(df$PB02_02, "1=0; 2=1; 3=2; else=NA")
  distract <- recode(df$PB02_07, "1=0; 2=1; 3=2; else=NA")
  reflect <- recode(df$PB03_05, "1=2; 2=1; 3=0; else=NA")
  attends <- recode(df$PB03_09, "1=2; 2=1; 3=0; else=NA")
  
  df.hyper <- data.frame(restless, fidgety, distract, reflect, attends)
  nhyper <- apply(df.hyper, 1, function(x) sum(is.na(x)))
  hyper <- ifelse(nhyper<3, rowMeans(df.hyper, na.rm=TRUE), NA)
  hyper <- as.numeric(hyper) * 5
  hyper <- floor(0.5 + hyper)
  
  # peer
  loner <- recode(df$PB01_06, "1=0; 2=1; 3=2; else=NA")
  friend <- recode(df$PB02_03, "1=2; 2=1; 3=0; else=NA")
  popular <- recode(df$PB02_06, "1=2; 2=1; 3=0; else=NA")
  bullied <- recode(df$PB03_03, "1=0; 2=1; 3=2; else=NA")
  oldbest <- recode(df$PB03_07, "1=0; 2=1; 3=2; else=NA")
  
  df.peer <- data.frame(loner, friend, popular, bullied, oldbest)
  npeer <- apply(df.peer, 1, function(x) sum(is.na(x)))
  peer <- ifelse(npeer<3, rowMeans(df.peer, na.rm=TRUE), NA)
  peer <- as.numeric(peer) * 5
  peer <- floor(0.5 + peer)
  
  # prososcial
  consid <- recode(df$PB01_01, "1=0; 2=1; 3=2; else=NA")
  shares <- recode(df$PB01_04, "1=0; 2=1; 3=2; else=NA")
  caring <- recode(df$PB02_01, "1=0; 2=1; 3=2; else=NA")
  kind <- recode(df$PB03_01, "1=0; 2=1; 3=2; else=NA")
  helpout <- recode(df$PB03_04, "1=0; 2=1; 3=2; else=NA")
  
  df.prosoc <- data.frame(consid, shares, caring, kind, helpout)
  nprosoc <- apply(df.prosoc, 1, function(x) sum(is.na(x)))
  prosoc <- ifelse(nprosoc<3, rowMeans(df.prosoc, na.rm=TRUE), NA)
  prosoc <- as.numeric(prosoc) * 5
  prosoc <- floor(0.5 + prosoc)
  
  # impact 
  distres <- recode(df$PB06, "1:2=0; 3=1; 4=2; else=NA")
  imphome <- recode(df$PB07_01, "1:2=0; 3=1; 4=2; else=NA")
  impfrie <- recode(df$PB07_02, "1:2=0; 3=1; 4=2; else=NA")
  impclas <- recode(df$PB07_03, "1:2=0; 3=1; 4=2; else=NA")
  impleis <- recode(df$PB07_04, "1:2=0; 3=1; 4=2; else=NA")
  
  df.impact <- data.frame(distres, imphome, impfrie, impclas, impleis)
  nimpact <- apply(df.impact, 1, function(x) sum(is.na(x)))
  impact <- ifelse(!nimpact==5, distres+imphome+impfrie+impclas+impleis, NA)
  impact <- ifelse(df$PB04==1, 0, impact)
  impact <- as.numeric(impact)
  
  sdq_total <- emotion + conduct + hyper + peer
  sdq_external <- conduct + hyper
  sdq_internal <- emotion + peer
  sdq_impact <- impact
  
  sdq = data.frame(sdq_total, sdq_external, sdq_internal, sdq_impact)
  
  return(sdq)
  
}