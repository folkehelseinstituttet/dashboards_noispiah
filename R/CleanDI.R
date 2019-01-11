#' CleanDI
#' @param di a
#' @param type a
#' @export CleanDI
CleanDI <- function(di, type="sykehjem"){

  if(type=="sykehjem"){
    di[,NumberPeople:=AntallBeboereKl8]
    di[,NumberPeopleMedInfeksjon:=AntallBeboereMedInfeksjon]

    di[,antallInfeksjonerHAI := AntallUrinveisInfeksjonerUtenUrinveiskateter_EgenInstitusjon +
         AntallUrinveisInfeksjonerUtenUrinveiskateter_AnnetSykehus +
         AntallUrinveisInfeksjonerUtenUrinveiskateter_AnnetSykehjem +
         AntallUrinveisInfeksjonerMedUrinveiskateter_EgenInstitusjon +
         AntallUrinveisInfeksjonerMedUrinveiskateter_AnnetSykehus +
         AntallUrinveisInfeksjonerMedUrinveiskateter_AnnetSykehjem +
         AntallNedreLuftveisInfeksjoner_EgenInstitusjon +
         AntallNedreLuftveisInfeksjoner_AnnetSykehus +
         AntallNedreLuftveisInfeksjoner_AnnetSykehjem +
         AntallOverflatiskePostOpSarinfeksjoner_EgenInstitusjon +
         AntallOverflatiskePostOpSarinfeksjoner_AnnetSykehus +
         AntallOverflatiskePostOpSarinfeksjoner_AnnetSykehjem +
         AntallDypePostOpSarinfeksjoner_EgenInstitusjon +
         AntallDypePostOpSarinfeksjoner_AnnetSykehus +
         AntallDypePostOpSarinfeksjoner_AnnetSykehjem +
         AntallHudInfeksjoner_EgenInstitusjon +
         AntallHudInfeksjoner_AnnetSykehus +
         AntallHudInfeksjoner_AnnetSykehjem]

  } else {
    di[,NumberPeople:=AntallPasienterKl8]
    di[,NumberPeopleMedInfeksjon:=AntallPasienterMedInfeksjon]

    di[,antallInfeksjonerHAI :=
         AntallUrinveisInfeksjoner_EgenInstitusjon +
         AntallUrinveisInfeksjoner_AnnetSykehus +
         AntallUrinveisInfeksjoner_AnnetSykehjem +
         AntallNedreLuftveisInfeksjoner_EgenInstitusjon +
         AntallNedreLuftveisInfeksjoner_AnnetSykehus +
         AntallNedreLuftveisInfeksjoner_AnnetSykehjem +
         AntallOverflatiskePostOpSarinfeksjoner_EgenInstitusjon +
         AntallOverflatiskePostOpSarinfeksjoner_AnnetSykehus +
         AntallOverflatiskePostOpSarinfeksjoner_AnnetSykehjem +
         AntallDypePostOpSarinfeksjoner_EgenInstitusjon +
         AntallDypePostOpSarinfeksjoner_AnnetSykehus +
         AntallDypePostOpSarinfeksjoner_AnnetSykehjem +
         AntallPostOpInfeksjonerIndreOrganHulrom_EgenInstitusjon +
         AntallPostOpInfeksjonerIndreOrganHulrom_AnnetSykehus +
         AntallPostOpInfeksjonerIndreOrganHulrom_AnnetSykehjem +
         AntallPrimareBlodbaneinfeksjoner_EgenInstitusjon +
         AntallPrimareBlodbaneinfeksjoner_AnnetSykehus +
         AntallPrimareBlodbaneinfeksjoner_AnnetSykehjem +
         AntallSekundareBlodbaneinfeksjoner_EgenInstitusjon +
         AntallSekundareBlodbaneinfeksjoner_AnnetSykehus +
         AntallSekundareBlodbaneinfeksjoner_AnnetSykehjem +
         AntallInfeksjonerBenLedd_EgenInstitusjon +
         AntallInfeksjonerBenLedd_AnnetSykehus +
         AntallInfeksjonerBenLedd_AnnetSykehjem +
         AntallInfeksjonerSentralnervesystemet_EgenInstitusjon +
         AntallInfeksjonerSentralnervesystemet_AnnetSykehus +
         AntallInfeksjonerSentralnervesystemet_AnnetSykehjem +
         AntallInfeksjonerHjerteKar_EgenInstitusjon +
         AntallInfeksjonerHjerteKar_AnnetSykehus +
         AntallInfeksjonerHjerteKar_AnnetSykehjem +
         AntallInfeksjonerOyeOreNeseHalsMunn_EgenInstitusjon +
         AntallInfeksjonerOyeOreNeseHalsMunn_AnnetSykehus +
         AntallInfeksjonerOyeOreNeseHalsMunn_AnnetSykehjem +
         AntallInfeksjonerMageTarm_EgenInstitusjon +
         AntallInfeksjonerMageTarm_AnnetSykehus +
         AntallInfeksjonerMageTarm_AnnetSykehjem +
         AntallInfeksjonerKjonnsorganene_EgenInstitusjon +
         AntallInfeksjonerKjonnsorganene_AnnetSykehus +
         AntallInfeksjonerKjonnsorganene_AnnetSykehjem +
         AntallInfeksjonerHudBlotvev_EgenInstitusjon +
         AntallInfeksjonerHudBlotvev_AnnetSykehus +
         AntallInfeksjonerHudBlotvev_AnnetSykehjem +
         AntallInfeksjonerAndreDisseminerte_EgenInstitusjon +
         AntallInfeksjonerAndreDisseminerte_AnnetSykehus +
         AntallInfeksjonerAndreDisseminerte_AnnetSykehjem
       ]

    di[,antallInfeksjonerHAIIperasjonsOmradet :=
         AntallOverflatiskePostOpSarinfeksjoner_EgenInstitusjon +
         AntallOverflatiskePostOpSarinfeksjoner_AnnetSykehus +
         AntallOverflatiskePostOpSarinfeksjoner_AnnetSykehjem +
         AntallDypePostOpSarinfeksjoner_EgenInstitusjon +
         AntallDypePostOpSarinfeksjoner_AnnetSykehus +
         AntallDypePostOpSarinfeksjoner_AnnetSykehjem +
         AntallPostOpInfeksjonerIndreOrganHulrom_EgenInstitusjon +
         AntallPostOpInfeksjonerIndreOrganHulrom_AnnetSykehus +
         AntallPostOpInfeksjonerIndreOrganHulrom_AnnetSykehjem]

    di[,antallInfeksjonerHAIUrinveis :=
         AntallUrinveisInfeksjoner_EgenInstitusjon +
         AntallUrinveisInfeksjoner_AnnetSykehus +
         AntallUrinveisInfeksjoner_AnnetSykehjem]

    di[,antallInfeksjonerHAINedreLuftveis :=
         AntallNedreLuftveisInfeksjoner_EgenInstitusjon +
         AntallNedreLuftveisInfeksjoner_AnnetSykehus +
         AntallNedreLuftveisInfeksjoner_AnnetSykehjem]

    di[,antallInfeksjonerHAIBlodbane :=
         AntallPrimareBlodbaneinfeksjoner_EgenInstitusjon +
         AntallPrimareBlodbaneinfeksjoner_AnnetSykehus +
         AntallPrimareBlodbaneinfeksjoner_AnnetSykehjem +
         AntallSekundareBlodbaneinfeksjoner_EgenInstitusjon +
         AntallSekundareBlodbaneinfeksjoner_AnnetSykehus +
         AntallSekundareBlodbaneinfeksjoner_AnnetSykehjem]

    CleanSpesialitet(di)
  }

}
