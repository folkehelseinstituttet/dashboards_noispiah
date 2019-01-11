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
         AntallDypePostOpSarinfeksjoner_AnnetSykehjem ]
  }

}
