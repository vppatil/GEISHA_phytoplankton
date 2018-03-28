# code with taxonomy used in algaebase.com 
# taxonomic class ending: ...ceae (-> only few exceptions with Cyanophyceae / Cyanobacteria)
# taxonomic order ending: ...es


traits.to.mfg = function(flagella = NULL, size = NULL, colonial = NULL, filament = NULL,  
                         centric = NULL, gelatinous = NULL, aerotypes = NULL, 
                         class = NULL, order = NULL)
{
  mfg = NA
  if (flagella ==  1 & !is.na(flagella) & class %in% c("Bacillariophyceae","Coscinodiscophyceae","Mediophyceae","Fragilariophyceae")==F) { #making sure that diatoms are excluded from this branch
    if (order == "Volvocales" & !is.na(order)) {
      if (colonial == 1 & !is.na(colonial)) {
        mfg = "3b-ColoPhyto"
        } 
      else {
          mfg = "3a-UnicPhyto"
      }
    }else if(class=='Cryptophyceae' & !is.na(class)){ #reshuffling so all motile cryptophytes go to 2d.
        mfg =  "2d-Crypto"
    }else  
      if (size == "large" & !is.na(size)) {
        if (class == "Chrysophyceae" | class == "Haptophyceae"| class == "Synurophyceae" | class == "Phaeothamniophyceae") {
          mfg = "1a-LargeChry"
          }
        else 
          if (class == "Dinophyceae") {
          mfg = "1b-LargeDino"
          }
          else {
            mfg = "1c-LargeEugl"
          }
        }
      else 
        if (class == "Chrysophyceae" | class == "Haptophyceae" | class == "Synurophyceae" | class == "Phaeothamniophyceae") {
          mfg = "2a-SmallChry1"
          }
        else 
          if (class == "Dinophyceae"){
            mfg = "2b-SmallDino"
          }
    else 
      if (class == "Euglenophyceae") {
              mfg = "2c-SmallEugl"
            }
    else { 
          mfg = NA
      }
    }
  # first node break: flagella == 0
  else 
    if (class == "Cyanophyceae" | class == "Cyanobacteria"){ 
      if (colonial == 1 & !is.na(colonial)){
        if (order == "Oscillatoriales" & !is.na(order)){
          mfg = "5a-FilaCyano"
        }
        else
          if (order == "Nostocales" & !is.na(order)){
            mfg = "5e-Nostocales"
          }
        else
          if (size == "large" & !is.na(size)) {
            if (aerotypes == 1 & !is.na(aerotypes)){
              mfg = "5b-LargeVacC"
            }
            else {
              mfg = "5c-OtherChroo"
            }
          }
        else{
          mfg = "5d-SmallChroo"
        }
      }
      else{
        mfg = "4-UnicCyano"
      }
    }
  else
    if (class == "Bacillariophyceae" | class == "Coscinodiscophyceae"  |
         class == "Mediophyceae" | class == "Fragilariophyceae"){
      if (size == "large" & !is.na(size)) {
        if (centric == 1 & !is.na(centric)) {
          if (colonial == 1 & !is.na(colonial)) {
            mfg = "6a1-LColCent"
          }
          else {
            mfg = "6a2-LUniCent"
          }
        }
        else 
          if (colonial == 1 & !is.na(colonial)){
            mfg = "6b1-LColPenn"
          }
        else {
          mfg = "6b2-LUniPenn"
        }
      }
      else
        if (centric == 1 & !is.na(centric)) {
          mfg = "7a-SmallCent"
        }
      else {
        mfg = "7b-SmallPenn"
      }
    }
  else 
    if (colonial == 1 & !is.na(colonial)){
      if (filament == 1 & !is.na(filament)) {
        if (class == "Chlorophyceae" | class == "Ulvophyceae" | class == "Trebouxiophyceae"){
          mfg = "10a-FilaChlorp"
          }
        else
          if (class == "Conjugatophyceae" | class == "Zygnematophyceae"){
          mfg = "10b-FilaConj"
          }
        else
          if (class == "Xanthophyceae" | class == 'Eustigmatophyceae') {
          mfg = "10c-FilaXant"
          }
      }
      else 
        if (order == "Chlorococcales" | order == "Chlamydomonadales" | order == "Tetrasporales" & !is.na(order)) {
            if (gelatinous == 1 & !is.na(gelatinous)) { ###gelatinous=1 means mucilage is present.
            mfg = "11b-GelaChlor"
            }
            else {
            mfg = "11a-NakeChlor"
            }
          }
      else {
        mfg = "11c-OtherCol"
      }
        }
      else 
    if (size == "large" & !is.na(size)) {
      if (class == "Chlorophyceae" | class == "Conjugatophyceae" | class == "Zygnematophyceae") {
        mfg = "8a-LargeCoCh"
      }
      else {
        mfg = "8b-LargeUnic"
      }
    }
  else
    if (class == "Conjugatophyceae"| class == "Zygnematophyceae"){
      mfg = "9a-SmallConj"
    }
  else 
    if (order == "Chlorococcales" & !is.na(order)) {
      mfg = "9b-SmallChlor"
    }
  else
    if (class == "Chrysophyceae" | class == "Synurophyceae" | class == "Phaeothamniophyceae") {
      mfg = "9c-SmallChry2"
    }
  else {
    mfg = "9d-SmallUnic"
  }
  
  return(mfg)
}

trait.to.mfg.df<-function(dframe,arg.names)
{
  dframe$mfg.from.traits=''
  for(i in 1:dim(dframe)[1])
  {
    dframe$mfg.from.traits[i]=traits.to.mfg(dframe[[arg.names[1]]][i],dframe[[arg.names[2]]][i],dframe[[arg.names[3]]][i],dframe[[arg.names[4]]][i],dframe[[arg.names[5]]][i],dframe[[arg.names[6]]][i],dframe[[arg.names[7]]][i],dframe[[arg.names[8]]][i],dframe[[arg.names[9]]][i])
  }
  
  return(dframe)
}

