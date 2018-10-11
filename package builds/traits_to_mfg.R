##algae classify function testing

traits_to_mfg(flagella=1,size='large',colonial=1,filament=0,centric=0,gelatinous=1,aerotopes=0,class='Haptophyceae',order=NA) #1a
traits_to_mfg(flagella=1,size='small',colonial=1,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Haptophyceae',order=NA) #2a
traits_to_mfg(flagella=1,size='large',colonial=1,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Dinophyceae',order=NA) #1b
traits_to_mfg(flagella=1,size='small',colonial=1,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Dinophyceae',order=NA) #2b
traits_to_mfg(flagella=1,size='small',colonial=0,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Euglenophyceae',order=NA) #2c
traits_to_mfg(flagella=1,size='large',colonial=0,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Euglenophyceae',order='Chlamydomonadales')

traits_to_mfg(flagella=1,size='large',colonial=1,filament=0,centric=0,gelatinous=1,aerotopes=0,class='Haptophyceae',order='Chlamydomonadales') #1a


##cyanobacteria

traits_to_mfg(flagella=0,size='large',colonial=0,filament=0,centric=0,gelatinous=1,aerotopes=0,class='Cyanobacteria',order=NA) #4
traits_to_mfg(flagella=0,size='large',colonial=1,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Cyanobacteria',order='Oscillatoriales') #5a
traits_to_mfg(flagella=0,size='large',colonial=1,filament=1,centric=0,gelatinous=1,aerotopes=1,class='Cyanobacteria',order='Chroococcales') #5b
traits_to_mfg(flagella=0,size='large',colonial=1,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Cyanobacteria',order='Chroococcales') #5b
traits_to_mfg(flagella=0,size='small',colonial=1,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Cyanobacteria',order='Chroococcales') #5b
traits_to_mfg(flagella=0,size='small',colonial=1,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Cyanobacteria',order='Nostocales') #5b

####Diatoms
traits_to_mfg(flagella=0,size='large',colonial=1,filament=1,centric=1,gelatinous=1,aerotopes=0,class='Bacillariophyceae',order=NA) #5b
#[1] "6a1-LColCent"
 traits_to_mfg(flagella=0,size='large',colonial=1,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Bacillariophyceae',order=NA) #5b
#[1] "6b1-LColPenn"
 traits_to_mfg(flagella=0,size='large',colonial=0,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Bacillariophyceae',order=NA) #5b
#[1] "6b2-LUniPenn"
 traits_to_mfg(flagella=0,size='large',colonial=0,filament=1,centric=1,gelatinous=1,aerotopes=0,class='Bacillariophyceae',order=NA) #5b
#[1] "6a2-LUniCent"
 traits_to_mfg(flagella=0,size='small',colonial=0,filament=1,centric=1,gelatinous=1,aerotopes=0,class='Bacillariophyceae',order=NA) #5b
#[1] "7a-SmallCent"
 traits_to_mfg(flagella=0,size='small',colonial=0,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Bacillariophyceae',order=NA) #5b
#[1] "7b-SmallPenn"

###others
 traits_to_mfg(flagella=0,size='small',colonial=0,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Chlorophyceae',order=NA) #5b
 #[1] "9d-SmallUnic"
 traits_to_mfg(flagella=0,size='large',colonial=1,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Chlorophyceae',order=NA) #5b
 #[1] "10a-FilaChlorp"
 traits_to_mfg(flagella=0,size='large',colonial=0,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Chlorophyceae',order=NA) #5b
 #[1] "8a-LargeCoCh"
 traits_to_mfg(flagella=0,size='large',colonial=0,filament=1,centric=0,gelatinous=1,aerotopes=0,class='',order=NA) #5b
 #[1] "8b-LargeUnic"
 traits_to_mfg(flagella=0,size='large',colonial=0,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Xanthophyceae',order=NA) #5b
 #[1] "8b-LargeUnic"
 traits_to_mfg(flagella=0,size='large',colonial=0,filament=1,centric=0,gelatinous=1,aerotopes=0,class=NA,order=NA) #5b
 #[1] "8b-LargeUnic"
 traits_to_mfg(flagella=0,size='small',colonial=0,filament=1,centric=0,gelatinous=1,aerotopes=0,class=NA,order=NA) #5b
 #[1] "9d-SmallUnic"
 traits_to_mfg(flagella=0,size='small',colonial=0,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Conjugatophyceae',order=NA) #5b
 #[1] "9a-SmallConj"
 traits_to_mfg(flagella=0,size='small',colonial=0,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Chlorophyceae',order='Chlorococcales') #5b
 #[1] "9b-SmallChlor"
 traits_to_mfg(flagella=0,size='small',colonial=0,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Chrysophyceae') #5b
 #[1] "9c-SmallChry2"
 traits_to_mfg(flagella=0,size='small',colonial=1,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Chlorophyceae',order='Chlorococcales') #5b
 #[1] "10a-FilaChlorp"
 traits_to_mfg(flagella=0,size='small',colonial=1,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Conjugatophyceae',order='Chlorococcales') #5b
 #[1] "10b-FilaConj"
 traits_to_mfg(flagella=0,size='small',colonial=1,filament=1,centric=0,gelatinous=1,aerotopes=0,class='Xanthophyceae',order='Chlorococcales') #5b
 #[1] "10c-FilaXant"
 traits_to_mfg(flagella=0,size='small',colonial=1,filament=0,centric=0,gelatinous=1,aerotopes=0,class='Xanthophyceae',order='Chlorococcales') #5b
 #[1] "11b-GelaChlor"
 traits_to_mfg(flagella=0,size='small',colonial=1,filament=0,centric=0,gelatinous=0,aerotopes=0,class='Xanthophyceae',order='Chlorococcales') #5b
 #[1] "11a-NakeChlor"
 traits_to_mfg(flagella=0,size='small',colonial=1,filament=0,centric=0,gelatinous=0,aerotopes=0,class='Xanthophyceae',order='Chroococcales') #5b
 #[1] "11c-OtherCol"
 