#' Assign a morphofunctional group based on binary functional traits and higher taxonomy
#'
#' @param flagella 1 if flagella are present, 0 if they are absent. 
#' @param size Character string: 'large' or 'small'. Classification criteria is left to the user.
#' @param colonial 1 if typically colonial growth form, 0 if typically unicellular.
#' @param filament 1 if dominant growth form is filamentous, 0 if not.
#' @param centric 1 if diatom with centric growth form, 0 if not. NA for non-diatoms.
#' @param gelatinous 1 mucilagenous sheath is typically present, 0 if not.
#' @param aerotopes 1 if aerotopes allowing buoyancy regulation are typically present, 0 if not.
#' @param class The taxonomic class of the species; We recommend algaebase as a standard reference.
#' @param order The taxonomic order of the species; We recommend algaebase as a standard reference.
#' 
#' @return A character string of the species' morphofunctional group
#'
#' @examples
#' traits.to.mfg(flagella = 1, size = 'large', colonial = 1, filament = 0, centric = NA, gelatinous = 0, aerotopes = 0, class = 'Euglenophyceae', order = 'Euglenales')

traits_to_mfg = function(flagella = NULL,
                         size = NULL,
                         colonial = NULL,
                         filament = NULL,  
                         centric = NULL,
                         gelatinous = NULL,
                         aerotopes = NULL, 
                         class = NULL,
                         order = NULL)
{
  mfg = NA
  if (flagella ==  1 &
      !is.na(flagella) &
      (class == "Bacillariophyceae" |
       class == "Coscinodiscophyceae" |
       class == "Mediophyceae" |
       class == "Fragilariophyceae")==F) {#making sure that diatoms are excluded from this branch
    if (order == "Volvocales" & !is.na(order)){
      if (colonial == 1 & !is.na(colonial)) {
        mfg = "3b-ColoPhyto"
      } 
      else {
        mfg = "3a-UnicPhyto"
      }
    }else if(class=='Cryptophyceae' & !is.na(class)){#ensures that all motile cryptophytes go to 2d.
      mfg =  "2d-Crypto"
    }else  
      if (size == "large" & !is.na(size)) {
        if (class == "Chrysophyceae" |
            class == "Haptophyceae" |
            class == "Synurophyceae" |
            class == "Phaeothamniophyceae") {
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
      if (class == "Chrysophyceae" |
          class == "Haptophyceae" |
          class == "Synurophyceae" |
          class == "Phaeothamniophyceae") {
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
    if (class == "Cyanophyceae" | 
        class == "Cyanobacteria"){ 
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
            if (aerotopes == 1 & !is.na(aerotopes)){
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
    if (class == "Bacillariophyceae" |
        class == "Coscinodiscophyceae"  |
        class == "Mediophyceae" |
        class == "Fragilariophyceae"){
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
        if (class == "Chlorophyceae" |
            class == "Ulvophyceae" |
            class == "Trebouxiophyceae"){
          mfg = "10a-FilaChlorp"
        }
        else
          if (class == "Conjugatophyceae" |
              class == "Zygnematophyceae"){
            mfg = "10b-FilaConj"
          }
        else
          if (class == "Xanthophyceae" |
              class == 'Eustigmatophyceae') {
            mfg = "10c-FilaXant"
          }
      }
      else 
        if (order == "Chlorococcales" |
            order == "Chlamydomonadales" |
            order == "Tetrasporales" & !is.na(order)) {
          if (gelatinous == 1 & !is.na(gelatinous)) {
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
      if (class == "Chlorophyceae" |
          class == "Conjugatophyceae" |
          class == "Zygnematophyceae") {
        mfg = "8a-LargeCoCh"
      }
      else {
        mfg = "8b-LargeUnic"
      }
    }
  else
    if (class == "Conjugatophyceae"|
        class == "Zygnematophyceae"){
      mfg = "9a-SmallConj"
    }
  else 
    if (order == "Chlorococcales" & !is.na(order)) {
      mfg = "9b-SmallChlor"
    }
  else
    if (class == "Chrysophyceae" |
        class == "Synurophyceae" |
        class == "Phaeothamniophyceae") {
      mfg = "9c-SmallChry2"
    }
  else {
    mfg = "9d-SmallUnic"
  }
  
  return(mfg)
}