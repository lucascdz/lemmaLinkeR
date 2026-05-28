#FUNCTION: Disambiguate links
#ambiguous_df <- MatchResults_list[['ambiguousMatches']]



# Disambiguate <- function(ambiguous_df){}
ambiguous_df <- ambiguous_df[,
  colnames(ambiguous_df) %in%
    c(
      "lemma",
      "upos",
      "feats",
      "token_ctrl",
      "lila_id",
      "label",
      "upostag",
      "fl_cat",
      "gen",
      "src"
    )
]
ambiguous_df <- ambiguous_df[order(ambiguous_df$lila_id), ]
ambiguous_list <- split(ambiguous_df, ambiguous_df$lemma)
ambiguous_cmd <- unlist(lapply(seq_along(ambiguous_list), function(i) {
  paste0(
    '\n#View(ambiguous_list[["',
    names(ambiguous_list)[i],
    '"]])\nambiguous_list[["',
    names(ambiguous_list)[i],
    '"]] <- ambiguous_list[["',
    names(ambiguous_list)[i],
    '"]][ambiguous_list[["',
    names(ambiguous_list)[i],
    '"]]$lila_id == "lilaLemma:XXXXXX",]'
  )
}))
write(ambiguous_cmd, paste0(target_folder, 'ambiguous_cmd.txt'))


#View(ambiguous_list[["abbas"]]) lilaLemma:86838
ambiguous_list[["abbas"]] <- ambiguous_list[["abbas"]][ambiguous_list[["abbas"]]$lila_id == "lilaLemma:86838",]

#View(ambiguous_list[["accedo"]]) lilaLemma:87084
ambiguous_list[["accedo"]] <- ambiguous_list[["accedo"]][ambiguous_list[["accedo"]]$lila_id == "lilaLemma:87084",]

#View(ambiguous_list[["actor"]]) lilaLemma:87309
ambiguous_list[["actor"]] <- ambiguous_list[["actor"]][ambiguous_list[["actor"]]$lila_id == "lilaLemma:87309",]

#View(ambiguous_list[["adiungo"]]) lilaLemma:87493
ambiguous_list[["adiungo"]] <- ambiguous_list[["adiungo"]][ambiguous_list[["adiungo"]]$lila_id == "lilaLemma:87493",]

#View(ambiguous_list[["aduocatus"]]) lilaLemma:87679
ambiguous_list[["aduocatus"]] <- ambiguous_list[["aduocatus"]][ambiguous_list[["aduocatus"]]$lila_id == "lilaLemma:87679",]

#View(ambiguous_list[["Alba"]])
ambiguous_list[["Alba"]] <- ambiguous_list[["Alba"]][ambiguous_list[["Alba"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["aliqui"]])
ambiguous_list[["aliqui"]] <- ambiguous_list[["aliqui"]][ambiguous_list[["aliqui"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["altercatio"]])
ambiguous_list[["altercatio"]] <- ambiguous_list[["altercatio"]][ambiguous_list[["altercatio"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Altissimus"]])
ambiguous_list[["Altissimus"]] <- ambiguous_list[["Altissimus"]][ambiguous_list[["Altissimus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Ambrosius"]])
ambiguous_list[["Ambrosius"]] <- ambiguous_list[["Ambrosius"]][ambiguous_list[["Ambrosius"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["amen"]])
ambiguous_list[["amen"]] <- ambiguous_list[["amen"]][ambiguous_list[["amen"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["amicus"]])
ambiguous_list[["amicus"]] <- ambiguous_list[["amicus"]][ambiguous_list[["amicus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Amicus"]])
ambiguous_list[["Amicus"]] <- ambiguous_list[["Amicus"]][ambiguous_list[["Amicus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["amita"]])
ambiguous_list[["amita"]] <- ambiguous_list[["amita"]][ambiguous_list[["amita"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["amor"]])
ambiguous_list[["amor"]] <- ambiguous_list[["amor"]][ambiguous_list[["amor"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Angelus"]])
ambiguous_list[["Angelus"]] <- ambiguous_list[["Angelus"]][ambiguous_list[["Angelus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["anima"]])
ambiguous_list[["anima"]] <- ambiguous_list[["anima"]][ambiguous_list[["anima"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["animus"]])
ambiguous_list[["animus"]] <- ambiguous_list[["animus"]][ambiguous_list[["animus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["annus"]])
ambiguous_list[["annus"]] <- ambiguous_list[["annus"]][ambiguous_list[["annus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["aqua"]])
ambiguous_list[["aqua"]] <- ambiguous_list[["aqua"]][ambiguous_list[["aqua"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["aquaeductus"]])
ambiguous_list[["aquaeductus"]] <- ambiguous_list[["aquaeductus"]][ambiguous_list[["aquaeductus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["aquilo"]])
ambiguous_list[["aquilo"]] <- ambiguous_list[["aquilo"]][ambiguous_list[["aquilo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["arbor"]])
ambiguous_list[["arbor"]] <- ambiguous_list[["arbor"]][ambiguous_list[["arbor"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["arca"]])
ambiguous_list[["arca"]] <- ambiguous_list[["arca"]][ambiguous_list[["arca"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["area"]])
ambiguous_list[["area"]] <- ambiguous_list[["area"]][ambiguous_list[["area"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Arena"]])
ambiguous_list[["Arena"]] <- ambiguous_list[["Arena"]][ambiguous_list[["Arena"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Arnus"]])
ambiguous_list[["Arnus"]] <- ambiguous_list[["Arnus"]][ambiguous_list[["Arnus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["assero"]])
ambiguous_list[["assero"]] <- ambiguous_list[["assero"]][ambiguous_list[["assero"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["attendo"]])
ambiguous_list[["attendo"]] <- ambiguous_list[["attendo"]][ambiguous_list[["attendo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["augustus"]])
ambiguous_list[["augustus"]] <- ambiguous_list[["augustus"]][ambiguous_list[["augustus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["aurifex"]])
ambiguous_list[["aurifex"]] <- ambiguous_list[["aurifex"]][ambiguous_list[["aurifex"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["auus"]])
ambiguous_list[["auus"]] <- ambiguous_list[["auus"]][ambiguous_list[["auus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["balneum"]])
ambiguous_list[["balneum"]] <- ambiguous_list[["balneum"]][ambiguous_list[["balneum"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["bannio"]])
ambiguous_list[["bannio"]] <- ambiguous_list[["bannio"]][ambiguous_list[["bannio"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["basilica"]])
ambiguous_list[["basilica"]] <- ambiguous_list[["basilica"]][ambiguous_list[["basilica"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["beatus"]])
ambiguous_list[["beatus"]] <- ambiguous_list[["beatus"]][ambiguous_list[["beatus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["bene"]])
ambiguous_list[["bene"]] <- ambiguous_list[["bene"]][ambiguous_list[["bene"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Benedictus"]])
ambiguous_list[["Benedictus"]] <- ambiguous_list[["Benedictus"]][ambiguous_list[["Benedictus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["beneficium"]])
ambiguous_list[["beneficium"]] <- ambiguous_list[["beneficium"]][ambiguous_list[["beneficium"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Berta"]])
ambiguous_list[["Berta"]] <- ambiguous_list[["Berta"]][ambiguous_list[["Berta"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["c"]])
ambiguous_list[["c"]] <- ambiguous_list[["c"]][ambiguous_list[["c"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["caballus"]])
ambiguous_list[["caballus"]] <- ambiguous_list[["caballus"]][ambiguous_list[["caballus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["caesar"]])
ambiguous_list[["caesar"]] <- ambiguous_list[["caesar"]][ambiguous_list[["caesar"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["calco"]])
ambiguous_list[["calco"]] <- ambiguous_list[["calco"]][ambiguous_list[["calco"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Calx"]])
ambiguous_list[["Calx"]] <- ambiguous_list[["Calx"]][ambiguous_list[["Calx"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["campus"]])
ambiguous_list[["campus"]] <- ambiguous_list[["campus"]][ambiguous_list[["campus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Canalis"]])
ambiguous_list[["Canalis"]] <- ambiguous_list[["Canalis"]][ambiguous_list[["Canalis"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["capitulum"]])
ambiguous_list[["capitulum"]] <- ambiguous_list[["capitulum"]][ambiguous_list[["capitulum"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["capra"]])
ambiguous_list[["capra"]] <- ambiguous_list[["capra"]][ambiguous_list[["capra"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["caput"]])
ambiguous_list[["caput"]] <- ambiguous_list[["caput"]][ambiguous_list[["caput"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["caritas"]])
ambiguous_list[["caritas"]] <- ambiguous_list[["caritas"]][ambiguous_list[["caritas"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Carolus"]])
ambiguous_list[["Carolus"]] <- ambiguous_list[["Carolus"]][ambiguous_list[["Carolus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["casa"]])
ambiguous_list[["casa"]] <- ambiguous_list[["casa"]][ambiguous_list[["casa"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["castellum"]])
ambiguous_list[["castellum"]] <- ambiguous_list[["castellum"]][ambiguous_list[["castellum"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Castellum"]])
ambiguous_list[["Castellum"]] <- ambiguous_list[["Castellum"]][ambiguous_list[["Castellum"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Castrum"]])
ambiguous_list[["Castrum"]] <- ambiguous_list[["Castrum"]][ambiguous_list[["Castrum"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["cedo"]])
ambiguous_list[["cedo"]] <- ambiguous_list[["cedo"]][ambiguous_list[["cedo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Celsus"]])
ambiguous_list[["Celsus"]] <- ambiguous_list[["Celsus"]][ambiguous_list[["Celsus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["cena"]])
ambiguous_list[["cena"]] <- ambiguous_list[["cena"]][ambiguous_list[["cena"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["ciuitas"]])
ambiguous_list[["ciuitas"]] <- ambiguous_list[["ciuitas"]][ambiguous_list[["ciuitas"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["claudo"]])
ambiguous_list[["claudo"]] <- ambiguous_list[["claudo"]][ambiguous_list[["claudo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["clementia"]])
ambiguous_list[["clementia"]] <- ambiguous_list[["clementia"]][ambiguous_list[["clementia"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["clericus"]])
ambiguous_list[["clericus"]] <- ambiguous_list[["clericus"]][ambiguous_list[["clericus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["cogito"]])
ambiguous_list[["cogito"]] <- ambiguous_list[["cogito"]][ambiguous_list[["cogito"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Collina"]])
ambiguous_list[["Collina"]] <- ambiguous_list[["Collina"]][ambiguous_list[["Collina"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["collis"]])
ambiguous_list[["collis"]] <- ambiguous_list[["collis"]][ambiguous_list[["collis"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Collis"]])
ambiguous_list[["Collis"]] <- ambiguous_list[["Collis"]][ambiguous_list[["Collis"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Colonia"]])
ambiguous_list[["Colonia"]] <- ambiguous_list[["Colonia"]][ambiguous_list[["Colonia"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Columba"]])
ambiguous_list[["Columba"]] <- ambiguous_list[["Columba"]][ambiguous_list[["Columba"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["columna"]])
ambiguous_list[["columna"]] <- ambiguous_list[["columna"]][ambiguous_list[["columna"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["commendo"]])
ambiguous_list[["commendo"]] <- ambiguous_list[["commendo"]][ambiguous_list[["commendo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["comparatio"]])
ambiguous_list[["comparatio"]] <- ambiguous_list[["comparatio"]][ambiguous_list[["comparatio"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["comparo"]])
ambiguous_list[["comparo"]] <- ambiguous_list[["comparo"]][ambiguous_list[["comparo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["compello"]])
ambiguous_list[["compello"]] <- ambiguous_list[["compello"]][ambiguous_list[["compello"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Compitum"]])
ambiguous_list[["Compitum"]] <- ambiguous_list[["Compitum"]][ambiguous_list[["Compitum"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["conditio"]])
ambiguous_list[["conditio"]] <- ambiguous_list[["conditio"]][ambiguous_list[["conditio"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["condo"]])
ambiguous_list[["condo"]] <- ambiguous_list[["condo"]][ambiguous_list[["condo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["conseruator"]])
ambiguous_list[["conseruator"]] <- ambiguous_list[["conseruator"]][ambiguous_list[["conseruator"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["consolo"]])
ambiguous_list[["consolo"]] <- ambiguous_list[["consolo"]][ambiguous_list[["consolo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["consuetudo"]])
ambiguous_list[["consuetudo"]] <- ambiguous_list[["consuetudo"]][ambiguous_list[["consuetudo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["consulatus"]])
ambiguous_list[["consulatus"]] <- ambiguous_list[["consulatus"]][ambiguous_list[["consulatus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["continentia"]])
ambiguous_list[["continentia"]] <- ambiguous_list[["continentia"]][ambiguous_list[["continentia"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["contingo"]])
ambiguous_list[["contingo"]] <- ambiguous_list[["contingo"]][ambiguous_list[["contingo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["conuersatio"]])
ambiguous_list[["conuersatio"]] <- ambiguous_list[["conuersatio"]][ambiguous_list[["conuersatio"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Creta"]])
ambiguous_list[["Creta"]] <- ambiguous_list[["Creta"]][ambiguous_list[["Creta"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["cum"]])
ambiguous_list[["cum"]] <- ambiguous_list[["cum"]][ambiguous_list[["cum"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["cura"]])
ambiguous_list[["cura"]] <- ambiguous_list[["cura"]][ambiguous_list[["cura"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["datus"]])
ambiguous_list[["datus"]] <- ambiguous_list[["datus"]][ambiguous_list[["datus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Dauid"]])
ambiguous_list[["Dauid"]] <- ambiguous_list[["Dauid"]][ambiguous_list[["Dauid"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["decerno"]])
ambiguous_list[["decerno"]] <- ambiguous_list[["decerno"]][ambiguous_list[["decerno"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["decido"]])
ambiguous_list[["decido"]] <- ambiguous_list[["decido"]][ambiguous_list[["decido"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["dedico"]])
ambiguous_list[["dedico"]] <- ambiguous_list[["dedico"]][ambiguous_list[["dedico"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["defensor"]])
ambiguous_list[["defensor"]] <- ambiguous_list[["defensor"]][ambiguous_list[["defensor"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["desertum"]])
ambiguous_list[["desertum"]] <- ambiguous_list[["desertum"]][ambiguous_list[["desertum"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Deus"]])
ambiguous_list[["Deus"]] <- ambiguous_list[["Deus"]][ambiguous_list[["Deus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["diaconus"]])
ambiguous_list[["diaconus"]] <- ambiguous_list[["diaconus"]][ambiguous_list[["diaconus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["dico"]])
ambiguous_list[["dico"]] <- ambiguous_list[["dico"]][ambiguous_list[["dico"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["dies"]])
ambiguous_list[["dies"]] <- ambiguous_list[["dies"]][ambiguous_list[["dies"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["dolus"]])
ambiguous_list[["dolus"]] <- ambiguous_list[["dolus"]][ambiguous_list[["dolus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["dominus"]])
ambiguous_list[["dominus"]] <- ambiguous_list[["dominus"]][ambiguous_list[["dominus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Dominus"]])
ambiguous_list[["Dominus"]] <- ambiguous_list[["Dominus"]][ambiguous_list[["Dominus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["domus"]])
ambiguous_list[["domus"]] <- ambiguous_list[["domus"]][ambiguous_list[["domus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Draco"]])
ambiguous_list[["Draco"]] <- ambiguous_list[["Draco"]][ambiguous_list[["Draco"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["edo"]])
ambiguous_list[["edo"]] <- ambiguous_list[["edo"]][ambiguous_list[["edo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["episcopatus"]])
ambiguous_list[["episcopatus"]] <- ambiguous_list[["episcopatus"]][ambiguous_list[["episcopatus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["excello"]])
ambiguous_list[["excello"]] <- ambiguous_list[["excello"]][ambiguous_list[["excello"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["excepto"]])
ambiguous_list[["excepto"]] <- ambiguous_list[["excepto"]][ambiguous_list[["excepto"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["excolo"]])
ambiguous_list[["excolo"]] <- ambiguous_list[["excolo"]][ambiguous_list[["excolo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["facultas"]])
ambiguous_list[["facultas"]] <- ambiguous_list[["facultas"]][ambiguous_list[["facultas"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["famulus"]])
ambiguous_list[["famulus"]] <- ambiguous_list[["famulus"]][ambiguous_list[["famulus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Februarius"]])
ambiguous_list[["Februarius"]] <- ambiguous_list[["Februarius"]][ambiguous_list[["Februarius"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Felicitas"]])
ambiguous_list[["Felicitas"]] <- ambiguous_list[["Felicitas"]][ambiguous_list[["Felicitas"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["ficus"]])
ambiguous_list[["ficus"]] <- ambiguous_list[["ficus"]][ambiguous_list[["ficus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["fides"]])
ambiguous_list[["fides"]] <- ambiguous_list[["fides"]][ambiguous_list[["fides"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Figlina"]])
ambiguous_list[["Figlina"]] <- ambiguous_list[["Figlina"]][ambiguous_list[["Figlina"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["finis"]])
ambiguous_list[["finis"]] <- ambiguous_list[["finis"]][ambiguous_list[["finis"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Finis"]])
ambiguous_list[["Finis"]] <- ambiguous_list[["Finis"]][ambiguous_list[["Finis"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Florentia"]])
ambiguous_list[["Florentia"]] <- ambiguous_list[["Florentia"]][ambiguous_list[["Florentia"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Flumen"]])
ambiguous_list[["Flumen"]] <- ambiguous_list[["Flumen"]][ambiguous_list[["Flumen"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["fluuius"]])
ambiguous_list[["fluuius"]] <- ambiguous_list[["fluuius"]][ambiguous_list[["fluuius"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Fons"]])
ambiguous_list[["Fons"]] <- ambiguous_list[["Fons"]][ambiguous_list[["Fons"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Fontana"]])
ambiguous_list[["Fontana"]] <- ambiguous_list[["Fontana"]][ambiguous_list[["Fontana"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Forum"]])
ambiguous_list[["Forum"]] <- ambiguous_list[["Forum"]][ambiguous_list[["Forum"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["fossa"]])
ambiguous_list[["fossa"]] <- ambiguous_list[["fossa"]][ambiguous_list[["fossa"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["franciscus"]])
ambiguous_list[["franciscus"]] <- ambiguous_list[["franciscus"]][ambiguous_list[["franciscus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["fraus"]])
ambiguous_list[["fraus"]] <- ambiguous_list[["fraus"]][ambiguous_list[["fraus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["fructus"]])
ambiguous_list[["fructus"]] <- ambiguous_list[["fructus"]][ambiguous_list[["fructus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["fundo"]])
ambiguous_list[["fundo"]] <- ambiguous_list[["fundo"]][ambiguous_list[["fundo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["gener"]])
ambiguous_list[["gener"]] <- ambiguous_list[["gener"]][ambiguous_list[["gener"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["genitor"]])
ambiguous_list[["genitor"]] <- ambiguous_list[["genitor"]][ambiguous_list[["genitor"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["genus"]])
ambiguous_list[["genus"]] <- ambiguous_list[["genus"]][ambiguous_list[["genus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["gradus"]])
ambiguous_list[["gradus"]] <- ambiguous_list[["gradus"]][ambiguous_list[["gradus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["gratia"]])
ambiguous_list[["gratia"]] <- ambiguous_list[["gratia"]][ambiguous_list[["gratia"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Gurges"]])
ambiguous_list[["Gurges"]] <- ambiguous_list[["Gurges"]][ambiguous_list[["Gurges"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["heres"]])
ambiguous_list[["heres"]] <- ambiguous_list[["heres"]][ambiguous_list[["heres"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["homo"]])
ambiguous_list[["homo"]] <- ambiguous_list[["homo"]][ambiguous_list[["homo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["honor"]])
ambiguous_list[["honor"]] <- ambiguous_list[["honor"]][ambiguous_list[["honor"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["hortus"]])
ambiguous_list[["hortus"]] <- ambiguous_list[["hortus"]][ambiguous_list[["hortus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Iacobus"]])
ambiguous_list[["Iacobus"]] <- ambiguous_list[["Iacobus"]][ambiguous_list[["Iacobus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["ignis"]])
ambiguous_list[["ignis"]] <- ambiguous_list[["ignis"]][ambiguous_list[["ignis"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["inde"]])
ambiguous_list[["inde"]] <- ambiguous_list[["inde"]][ambiguous_list[["inde"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["indignus"]])
ambiguous_list[["indignus"]] <- ambiguous_list[["indignus"]][ambiguous_list[["indignus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["indulgentia"]])
ambiguous_list[["indulgentia"]] <- ambiguous_list[["indulgentia"]][ambiguous_list[["indulgentia"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["insero"]])
ambiguous_list[["insero"]] <- ambiguous_list[["insero"]][ambiguous_list[["insero"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["insula"]])
ambiguous_list[["insula"]] <- ambiguous_list[["insula"]][ambiguous_list[["insula"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["intrinsecus"]])
ambiguous_list[["intrinsecus"]] <- ambiguous_list[["intrinsecus"]][ambiguous_list[["intrinsecus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["inuado"]])
ambiguous_list[["inuado"]] <- ambiguous_list[["inuado"]][ambiguous_list[["inuado"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Italia"]])
ambiguous_list[["Italia"]] <- ambiguous_list[["Italia"]][ambiguous_list[["Italia"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["iudex"]])
ambiguous_list[["iudex"]] <- ambiguous_list[["iudex"]][ambiguous_list[["iudex"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["iugiter"]])
ambiguous_list[["iugiter"]] <- ambiguous_list[["iugiter"]][ambiguous_list[["iugiter"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["ius"]])
ambiguous_list[["ius"]] <- ambiguous_list[["ius"]][ambiguous_list[["ius"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["iustitia"]])
ambiguous_list[["iustitia"]] <- ambiguous_list[["iustitia"]][ambiguous_list[["iustitia"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Julianus"]])
ambiguous_list[["Julianus"]] <- ambiguous_list[["Julianus"]][ambiguous_list[["Julianus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["labor"]])
ambiguous_list[["labor"]] <- ambiguous_list[["labor"]][ambiguous_list[["labor"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Lama"]])
ambiguous_list[["Lama"]] <- ambiguous_list[["Lama"]][ambiguous_list[["Lama"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["latitudo"]])
ambiguous_list[["latitudo"]] <- ambiguous_list[["latitudo"]][ambiguous_list[["latitudo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["latus"]])
ambiguous_list[["latus"]] <- ambiguous_list[["latus"]][ambiguous_list[["latus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["laus"]])
ambiguous_list[["laus"]] <- ambiguous_list[["laus"]][ambiguous_list[["laus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["lectus"]])
ambiguous_list[["lectus"]] <- ambiguous_list[["lectus"]][ambiguous_list[["lectus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["lego"]])
ambiguous_list[["lego"]] <- ambiguous_list[["lego"]][ambiguous_list[["lego"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Leo"]])
ambiguous_list[["Leo"]] <- ambiguous_list[["Leo"]][ambiguous_list[["Leo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Leporaria"]])
ambiguous_list[["Leporaria"]] <- ambiguous_list[["Leporaria"]][ambiguous_list[["Leporaria"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["leuo"]])
ambiguous_list[["leuo"]] <- ambiguous_list[["leuo"]][ambiguous_list[["leuo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["lex"]])
ambiguous_list[["lex"]] <- ambiguous_list[["lex"]][ambiguous_list[["lex"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["libellus"]])
ambiguous_list[["libellus"]] <- ambiguous_list[["libellus"]][ambiguous_list[["libellus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["licentia"]])
ambiguous_list[["licentia"]] <- ambiguous_list[["licentia"]][ambiguous_list[["licentia"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["locus"]])
ambiguous_list[["locus"]] <- ambiguous_list[["locus"]][ambiguous_list[["locus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["lora"]])
ambiguous_list[["lora"]] <- ambiguous_list[["lora"]][ambiguous_list[["lora"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Luca"]])
ambiguous_list[["Luca"]] <- ambiguous_list[["Luca"]][ambiguous_list[["Luca"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["lucanus"]])
ambiguous_list[["lucanus"]] <- ambiguous_list[["lucanus"]][ambiguous_list[["lucanus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Lucia"]])
ambiguous_list[["Lucia"]] <- ambiguous_list[["Lucia"]][ambiguous_list[["Lucia"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["magister"]])
ambiguous_list[["magister"]] <- ambiguous_list[["magister"]][ambiguous_list[["magister"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["maius"]])
ambiguous_list[["maius"]] <- ambiguous_list[["maius"]][ambiguous_list[["maius"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["mando"]])
ambiguous_list[["mando"]] <- ambiguous_list[["mando"]][ambiguous_list[["mando"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["mansio"]])
ambiguous_list[["mansio"]] <- ambiguous_list[["mansio"]][ambiguous_list[["mansio"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Mansio"]])
ambiguous_list[["Mansio"]] <- ambiguous_list[["Mansio"]][ambiguous_list[["Mansio"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Mansus"]])
ambiguous_list[["Mansus"]] <- ambiguous_list[["Mansus"]][ambiguous_list[["Mansus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Marcus"]])
ambiguous_list[["Marcus"]] <- ambiguous_list[["Marcus"]][ambiguous_list[["Marcus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Maria"]])
ambiguous_list[["Maria"]] <- ambiguous_list[["Maria"]][ambiguous_list[["Maria"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Marinus"]])
ambiguous_list[["Marinus"]] <- ambiguous_list[["Marinus"]][ambiguous_list[["Marinus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Maritima"]])
ambiguous_list[["Maritima"]] <- ambiguous_list[["Maritima"]][ambiguous_list[["Maritima"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Martha"]])
ambiguous_list[["Martha"]] <- ambiguous_list[["Martha"]][ambiguous_list[["Martha"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Martinus"]])
ambiguous_list[["Martinus"]] <- ambiguous_list[["Martinus"]][ambiguous_list[["Martinus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["mater"]])
ambiguous_list[["mater"]] <- ambiguous_list[["mater"]][ambiguous_list[["mater"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["medicus"]])
ambiguous_list[["medicus"]] <- ambiguous_list[["medicus"]][ambiguous_list[["medicus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["memoria"]])
ambiguous_list[["memoria"]] <- ambiguous_list[["memoria"]][ambiguous_list[["memoria"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["mens"]])
ambiguous_list[["mens"]] <- ambiguous_list[["mens"]][ambiguous_list[["mens"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["mensis"]])
ambiguous_list[["mensis"]] <- ambiguous_list[["mensis"]][ambiguous_list[["mensis"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Michael"]])
ambiguous_list[["Michael"]] <- ambiguous_list[["Michael"]][ambiguous_list[["Michael"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["minuo"]])
ambiguous_list[["minuo"]] <- ambiguous_list[["minuo"]][ambiguous_list[["minuo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["misericordia"]])
ambiguous_list[["misericordia"]] <- ambiguous_list[["misericordia"]][ambiguous_list[["misericordia"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["missus"]])
ambiguous_list[["missus"]] <- ambiguous_list[["missus"]][ambiguous_list[["missus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["mitto"]])
ambiguous_list[["mitto"]] <- ambiguous_list[["mitto"]][ambiguous_list[["mitto"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Mona"]])
ambiguous_list[["Mona"]] <- ambiguous_list[["Mona"]][ambiguous_list[["Mona"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["moneta"]])
ambiguous_list[["moneta"]] <- ambiguous_list[["moneta"]][ambiguous_list[["moneta"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["mons"]])
ambiguous_list[["mons"]] <- ambiguous_list[["mons"]][ambiguous_list[["mons"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Mons"]])
ambiguous_list[["Mons"]] <- ambiguous_list[["Mons"]][ambiguous_list[["Mons"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["mora"]])
ambiguous_list[["mora"]] <- ambiguous_list[["mora"]][ambiguous_list[["mora"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["morior"]])
ambiguous_list[["morior"]] <- ambiguous_list[["morior"]][ambiguous_list[["morior"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["mundus"]])
ambiguous_list[["mundus"]] <- ambiguous_list[["mundus"]][ambiguous_list[["mundus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["murus"]])
ambiguous_list[["murus"]] <- ambiguous_list[["murus"]][ambiguous_list[["murus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["namque"]])
ambiguous_list[["namque"]] <- ambiguous_list[["namque"]][ambiguous_list[["namque"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["natalis"]])
ambiguous_list[["natalis"]] <- ambiguous_list[["natalis"]][ambiguous_list[["natalis"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Natalis"]])
ambiguous_list[["Natalis"]] <- ambiguous_list[["Natalis"]][ambiguous_list[["Natalis"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["natio"]])
ambiguous_list[["natio"]] <- ambiguous_list[["natio"]][ambiguous_list[["natio"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["necessitas"]])
ambiguous_list[["necessitas"]] <- ambiguous_list[["necessitas"]][ambiguous_list[["necessitas"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["nepos"]])
ambiguous_list[["nepos"]] <- ambiguous_list[["nepos"]][ambiguous_list[["nepos"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Neure"]])
ambiguous_list[["Neure"]] <- ambiguous_list[["Neure"]][ambiguous_list[["Neure"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["notus"]])
ambiguous_list[["notus"]] <- ambiguous_list[["notus"]][ambiguous_list[["notus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Noua"]])
ambiguous_list[["Noua"]] <- ambiguous_list[["Noua"]][ambiguous_list[["Noua"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["nox"]])
ambiguous_list[["nox"]] <- ambiguous_list[["nox"]][ambiguous_list[["nox"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["obitus"]])
ambiguous_list[["obitus"]] <- ambiguous_list[["obitus"]][ambiguous_list[["obitus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["occasio"]])
ambiguous_list[["occasio"]] <- ambiguous_list[["occasio"]][ambiguous_list[["occasio"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["offero"]])
ambiguous_list[["offero"]] <- ambiguous_list[["offero"]][ambiguous_list[["offero"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["oliua"]])
ambiguous_list[["oliua"]] <- ambiguous_list[["oliua"]][ambiguous_list[["oliua"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["oliuetum"]])
ambiguous_list[["oliuetum"]] <- ambiguous_list[["oliuetum"]][ambiguous_list[["oliuetum"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["opera"]])
ambiguous_list[["opera"]] <- ambiguous_list[["opera"]][ambiguous_list[["opera"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["oraculum"]])
ambiguous_list[["oraculum"]] <- ambiguous_list[["oraculum"]][ambiguous_list[["oraculum"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["paganus"]])
ambiguous_list[["paganus"]] <- ambiguous_list[["paganus"]][ambiguous_list[["paganus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["palatium"]])
ambiguous_list[["palatium"]] <- ambiguous_list[["palatium"]][ambiguous_list[["palatium"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["palus"]])
ambiguous_list[["palus"]] <- ambiguous_list[["palus"]][ambiguous_list[["palus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Palus"]])
ambiguous_list[["Palus"]] <- ambiguous_list[["Palus"]][ambiguous_list[["Palus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["papa"]])
ambiguous_list[["papa"]] <- ambiguous_list[["papa"]][ambiguous_list[["papa"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["parium"]])
ambiguous_list[["parium"]] <- ambiguous_list[["parium"]][ambiguous_list[["parium"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["paro"]])
ambiguous_list[["paro"]] <- ambiguous_list[["paro"]][ambiguous_list[["paro"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["pasco"]])
ambiguous_list[["pasco"]] <- ambiguous_list[["pasco"]][ambiguous_list[["pasco"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Pastor"]])
ambiguous_list[["Pastor"]] <- ambiguous_list[["Pastor"]][ambiguous_list[["Pastor"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["pater"]])
ambiguous_list[["pater"]] <- ambiguous_list[["pater"]][ambiguous_list[["pater"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["patricius"]])
ambiguous_list[["patricius"]] <- ambiguous_list[["patricius"]][ambiguous_list[["patricius"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Paulus"]])
ambiguous_list[["Paulus"]] <- ambiguous_list[["Paulus"]][ambiguous_list[["Paulus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["pax"]])
ambiguous_list[["pax"]] <- ambiguous_list[["pax"]][ambiguous_list[["pax"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["petra"]])
ambiguous_list[["petra"]] <- ambiguous_list[["petra"]][ambiguous_list[["petra"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Petrus"]])
ambiguous_list[["Petrus"]] <- ambiguous_list[["Petrus"]][ambiguous_list[["Petrus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["pietas"]])
ambiguous_list[["pietas"]] <- ambiguous_list[["pietas"]][ambiguous_list[["pietas"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Pirus"]])
ambiguous_list[["Pirus"]] <- ambiguous_list[["Pirus"]][ambiguous_list[["Pirus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Pisa"]])
ambiguous_list[["Pisa"]] <- ambiguous_list[["Pisa"]][ambiguous_list[["Pisa"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["placeo"]])
ambiguous_list[["placeo"]] <- ambiguous_list[["placeo"]][ambiguous_list[["placeo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["placitus"]])
ambiguous_list[["placitus"]] <- ambiguous_list[["placitus"]][ambiguous_list[["placitus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["platea"]])
ambiguous_list[["platea"]] <- ambiguous_list[["platea"]][ambiguous_list[["platea"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["poena"]])
ambiguous_list[["poena"]] <- ambiguous_list[["poena"]][ambiguous_list[["poena"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Pons"]])
ambiguous_list[["Pons"]] <- ambiguous_list[["Pons"]][ambiguous_list[["Pons"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["potestas"]])
ambiguous_list[["potestas"]] <- ambiguous_list[["potestas"]][ambiguous_list[["potestas"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["praefatus"]])
ambiguous_list[["praefatus"]] <- ambiguous_list[["praefatus"]][ambiguous_list[["praefatus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["praesaepe"]])
ambiguous_list[["praesaepe"]] <- ambiguous_list[["praesaepe"]][ambiguous_list[["praesaepe"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Praetextatus"]])
ambiguous_list[["Praetextatus"]] <- ambiguous_list[["Praetextatus"]][ambiguous_list[["Praetextatus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["primus"]])
ambiguous_list[["primus"]] <- ambiguous_list[["primus"]][ambiguous_list[["primus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["prior"]])
ambiguous_list[["prior"]] <- ambiguous_list[["prior"]][ambiguous_list[["prior"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["pristinus"]])
ambiguous_list[["pristinus"]] <- ambiguous_list[["pristinus"]][ambiguous_list[["pristinus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["prouidentia"]])
ambiguous_list[["prouidentia"]] <- ambiguous_list[["prouidentia"]][ambiguous_list[["prouidentia"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["proximus"]])
ambiguous_list[["proximus"]] <- ambiguous_list[["proximus"]][ambiguous_list[["proximus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["pullus"]])
ambiguous_list[["pullus"]] <- ambiguous_list[["pullus"]][ambiguous_list[["pullus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["quam"]])
ambiguous_list[["quam"]] <- ambiguous_list[["quam"]][ambiguous_list[["quam"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["quamuis"]])
ambiguous_list[["quamuis"]] <- ambiguous_list[["quamuis"]][ambiguous_list[["quamuis"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["quandoque"]])
ambiguous_list[["quandoque"]] <- ambiguous_list[["quandoque"]][ambiguous_list[["quandoque"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["quartus"]])
ambiguous_list[["quartus"]] <- ambiguous_list[["quartus"]][ambiguous_list[["quartus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["qui"]])
ambiguous_list[["qui"]] <- ambiguous_list[["qui"]][ambiguous_list[["qui"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["recolligo"]])
ambiguous_list[["recolligo"]] <- ambiguous_list[["recolligo"]][ambiguous_list[["recolligo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["recolo"]])
ambiguous_list[["recolo"]] <- ambiguous_list[["recolo"]][ambiguous_list[["recolo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["recta"]])
ambiguous_list[["recta"]] <- ambiguous_list[["recta"]][ambiguous_list[["recta"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["rectus"]])
ambiguous_list[["rectus"]] <- ambiguous_list[["rectus"]][ambiguous_list[["rectus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["refero"]])
ambiguous_list[["refero"]] <- ambiguous_list[["refero"]][ambiguous_list[["refero"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["regina"]])
ambiguous_list[["regina"]] <- ambiguous_list[["regina"]][ambiguous_list[["regina"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["regula"]])
ambiguous_list[["regula"]] <- ambiguous_list[["regula"]][ambiguous_list[["regula"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Regulus"]])
ambiguous_list[["Regulus"]] <- ambiguous_list[["Regulus"]][ambiguous_list[["Regulus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["relego"]])
ambiguous_list[["relego"]] <- ambiguous_list[["relego"]][ambiguous_list[["relego"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["religio"]])
ambiguous_list[["religio"]] <- ambiguous_list[["religio"]][ambiguous_list[["religio"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Ripa"]])
ambiguous_list[["Ripa"]] <- ambiguous_list[["Ripa"]][ambiguous_list[["Ripa"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Roma"]])
ambiguous_list[["Roma"]] <- ambiguous_list[["Roma"]][ambiguous_list[["Roma"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Romania"]])
ambiguous_list[["Romania"]] <- ambiguous_list[["Romania"]][ambiguous_list[["Romania"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Russus"]])
ambiguous_list[["Russus"]] <- ambiguous_list[["Russus"]][ambiguous_list[["Russus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["saccus"]])
ambiguous_list[["saccus"]] <- ambiguous_list[["saccus"]][ambiguous_list[["saccus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["sacerdos"]])
ambiguous_list[["sacerdos"]] <- ambiguous_list[["sacerdos"]][ambiguous_list[["sacerdos"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["sala"]])
ambiguous_list[["sala"]] <- ambiguous_list[["sala"]][ambiguous_list[["sala"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["saluator"]])
ambiguous_list[["saluator"]] <- ambiguous_list[["saluator"]][ambiguous_list[["saluator"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["salus"]])
ambiguous_list[["salus"]] <- ambiguous_list[["salus"]][ambiguous_list[["salus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["sapio"]])
ambiguous_list[["sapio"]] <- ambiguous_list[["sapio"]][ambiguous_list[["sapio"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["secundus"]])
ambiguous_list[["secundus"]] <- ambiguous_list[["secundus"]][ambiguous_list[["secundus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["sed"]])
ambiguous_list[["sed"]] <- ambiguous_list[["sed"]][ambiguous_list[["sed"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Sena"]])
ambiguous_list[["Sena"]] <- ambiguous_list[["Sena"]][ambiguous_list[["Sena"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["senectus"]])
ambiguous_list[["senectus"]] <- ambiguous_list[["senectus"]][ambiguous_list[["senectus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["seo"]])
ambiguous_list[["seo"]] <- ambiguous_list[["seo"]][ambiguous_list[["seo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["separo"]])
ambiguous_list[["separo"]] <- ambiguous_list[["separo"]][ambiguous_list[["separo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["seruitium"]])
ambiguous_list[["seruitium"]] <- ambiguous_list[["seruitium"]][ambiguous_list[["seruitium"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Sicca"]])
ambiguous_list[["Sicca"]] <- ambiguous_list[["Sicca"]][ambiguous_list[["Sicca"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Siculus"]])
ambiguous_list[["Siculus"]] <- ambiguous_list[["Siculus"]][ambiguous_list[["Siculus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["sicut"]])
ambiguous_list[["sicut"]] <- ambiguous_list[["sicut"]][ambiguous_list[["sicut"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["silua"]])
ambiguous_list[["silua"]] <- ambiguous_list[["silua"]][ambiguous_list[["silua"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Simeon"]])
ambiguous_list[["Simeon"]] <- ambiguous_list[["Simeon"]][ambiguous_list[["Simeon"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["sino"]])
ambiguous_list[["sino"]] <- ambiguous_list[["sino"]][ambiguous_list[["sino"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["solarium"]])
ambiguous_list[["solarium"]] <- ambiguous_list[["solarium"]][ambiguous_list[["solarium"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["sors"]])
ambiguous_list[["sors"]] <- ambiguous_list[["sors"]][ambiguous_list[["sors"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Spiculus"]])
ambiguous_list[["Spiculus"]] <- ambiguous_list[["Spiculus"]][ambiguous_list[["Spiculus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Stabulum"]])
ambiguous_list[["Stabulum"]] <- ambiguous_list[["Stabulum"]][ambiguous_list[["Stabulum"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Stagnum"]])
ambiguous_list[["Stagnum"]] <- ambiguous_list[["Stagnum"]][ambiguous_list[["Stagnum"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["statuo"]])
ambiguous_list[["statuo"]] <- ambiguous_list[["statuo"]][ambiguous_list[["statuo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["status"]])
ambiguous_list[["status"]] <- ambiguous_list[["status"]][ambiguous_list[["status"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["summus"]])
ambiguous_list[["summus"]] <- ambiguous_list[["summus"]][ambiguous_list[["summus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["tam"]])
ambiguous_list[["tam"]] <- ambiguous_list[["tam"]][ambiguous_list[["tam"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["tempus"]])
ambiguous_list[["tempus"]] <- ambiguous_list[["tempus"]][ambiguous_list[["tempus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["termen"]])
ambiguous_list[["termen"]] <- ambiguous_list[["termen"]][ambiguous_list[["termen"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Termen"]])
ambiguous_list[["Termen"]] <- ambiguous_list[["Termen"]][ambiguous_list[["Termen"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["terra"]])
ambiguous_list[["terra"]] <- ambiguous_list[["terra"]][ambiguous_list[["terra"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Tertia"]])
ambiguous_list[["Tertia"]] <- ambiguous_list[["Tertia"]][ambiguous_list[["Tertia"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["testis"]])
ambiguous_list[["testis"]] <- ambiguous_list[["testis"]][ambiguous_list[["testis"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["texo"]])
ambiguous_list[["texo"]] <- ambiguous_list[["texo"]][ambiguous_list[["texo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["timor"]])
ambiguous_list[["timor"]] <- ambiguous_list[["timor"]][ambiguous_list[["timor"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["titulus"]])
ambiguous_list[["titulus"]] <- ambiguous_list[["titulus"]][ambiguous_list[["titulus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["totus"]])
ambiguous_list[["totus"]] <- ambiguous_list[["totus"]][ambiguous_list[["totus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Totus"]])
ambiguous_list[["Totus"]] <- ambiguous_list[["Totus"]][ambiguous_list[["Totus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["traho"]])
ambiguous_list[["traho"]] <- ambiguous_list[["traho"]][ambiguous_list[["traho"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["tres"]])
ambiguous_list[["tres"]] <- ambiguous_list[["tres"]][ambiguous_list[["tres"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["tutor"]])
ambiguous_list[["tutor"]] <- ambiguous_list[["tutor"]][ambiguous_list[["tutor"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["uenio"]])
ambiguous_list[["uenio"]] <- ambiguous_list[["uenio"]][ambiguous_list[["uenio"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["uestitus"]])
ambiguous_list[["uestitus"]] <- ambiguous_list[["uestitus"]][ambiguous_list[["uestitus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["uia"]])
ambiguous_list[["uia"]] <- ambiguous_list[["uia"]][ambiguous_list[["uia"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["uictus"]])
ambiguous_list[["uictus"]] <- ambiguous_list[["uictus"]][ambiguous_list[["uictus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["uicus"]])
ambiguous_list[["uicus"]] <- ambiguous_list[["uicus"]][ambiguous_list[["uicus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["uirtus"]])
ambiguous_list[["uirtus"]] <- ambiguous_list[["uirtus"]][ambiguous_list[["uirtus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["uitis"]])
ambiguous_list[["uitis"]] <- ambiguous_list[["uitis"]][ambiguous_list[["uitis"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["uoco"]])
ambiguous_list[["uoco"]] <- ambiguous_list[["uoco"]][ambiguous_list[["uoco"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["uolo"]])
ambiguous_list[["uolo"]] <- ambiguous_list[["uolo"]][ambiguous_list[["uolo"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Ursus"]])
ambiguous_list[["Ursus"]] <- ambiguous_list[["Ursus"]][ambiguous_list[["Ursus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["usus"]])
ambiguous_list[["usus"]] <- ambiguous_list[["usus"]][ambiguous_list[["usus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["ut"]])
ambiguous_list[["ut"]] <- ambiguous_list[["ut"]][ambiguous_list[["ut"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Vallis"]])
ambiguous_list[["Vallis"]] <- ambiguous_list[["Vallis"]][ambiguous_list[["Vallis"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Venus"]])
ambiguous_list[["Venus"]] <- ambiguous_list[["Venus"]][ambiguous_list[["Venus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Vitulus"]])
ambiguous_list[["Vitulus"]] <- ambiguous_list[["Vitulus"]][ambiguous_list[["Vitulus"]]$lila_id == "lilaLemma:XXXXXX",]

#View(ambiguous_list[["Vitus"]])
ambiguous_list[["Vitus"]] <- ambiguous_list[["Vitus"]][ambiguous_list[["Vitus"]]$lila_id == "lilaLemma:XXXXXX",]
