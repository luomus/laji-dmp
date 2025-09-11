module Views.Errors exposing (..)

import DmpApi exposing (BadStatusResponse)
import Html exposing (Html)
import Html exposing (p)
import Html exposing (text)
import DmpApi exposing (ErrorResponse)
import Html exposing (div)
import DmpApi exposing (decodeServerDecodeError)
import DmpApi exposing (ServerDecodeError)
import Array
import Html exposing (h6)

localizeDmpConstructor : String -> String
localizeDmpConstructor str =
  case str of
    -- Dmp
    "dmpId" -> "ID"
    "dmpCreated" -> "Luotu"
    "dmpDescription" -> "Kuvaus"
    "dmpLanguage" -> "Kieli"
    "dmpModified" -> "Muokattu"
    "dmpNextReviewDmp" -> "Seuraava tarkistus"
    "dmpOrgId" -> "Organisaation tunnus"
    "dmpTitle" -> "Otsikko"
    "dmpTypeDmp" -> "Dmp:n tyyppi"
    "dmpContact" -> "Kontakti"
    "dmpDmpId" -> "Dmp:n tunniste"
    "dmpContributors" -> "Osallistujat"
    "dmpDataLifeCycle" -> "Datan elinkaari"
    "dmpDatasets" -> "Aineistot"
    "dmpEthicalIssues" -> "Eettiset haasteet"
    "dmpProjects" -> "Projektit"

    -- DmpId
    "dmpIdIdentifier" -> "Tunniste"
    "dmpIdType" -> "Tyyppi"

    -- Contact
    "contactMbox" -> "Sähköpostiosoite"
    "contactName" -> "Nimi"
    "contactOrganization" -> "Organisaatio"
    "contactContactId" -> "Kontaktin tunniste"

    -- ContactId
    "contactIdIdentifier" -> "Tunniste"
    "contactIdType" -> "Tyyppi"

    -- Contributor
    "contributorMbox" -> "Sähköpostiosoite"
    "contributorName" -> "Nimi"
    "contributorOrganization" -> "Organisaatio"
    "contributorRole" -> "Rooli"
    "contributorContributorId" -> "Osallistujan tunniste"

    -- ContributorId
    "contributorIdIdentifier" -> "Tunniste"
    "contributorIdType" -> "Tyyppi"

    -- DataLifeCycle
    "dataLifeCycleArchivingServicesData" -> "Arkistointi"
    "dataLifeCycleBackupData" -> "Varmuuskopiointi"
    "dataLifeCycleDeletionData" -> "Datan poisto"
    "dataLifeCycleDeletionWhenData" -> "Datan poistamispäivä"

    -- Dataset
    "datasetDataQualityAssurance" -> "Laadunvarmistuksen kuvaus"
    "datasetDataSharingIssues" -> "Datanjakamisen haasteet"
    "datasetDescription" -> "Kuvaus"
    "datasetIssued" -> "Aineiston tuotantoajankohta"
    "datasetKeywords" -> "Avainsanat"
    "datasetLanguage" -> "Kieli"
    "datasetPersonalData" -> "Henkilötiedot"
    "datasetSensitiveData" -> "Sensitiivinen data"
    "datasetReuseDataset" -> "Aineiston uudelleenkäyttö"
    "datasetTitle" -> "Otsikko"
    "datasetType" -> "Tyyppi"
    "datasetDatasetId" -> "Aineiston tunniste"
    "datasetDistributions" -> "Julkaisut"
    "datasetMetadata" -> "Metadata"
    "datasetRightsRelatedToData" -> "Datan oikeudet"
    "datasetSecurityAndPrivacy" -> "Tietoturva"

    -- DatasetId
    "datasetIdIdentifier" -> "Tunniste"
    "datasetIdType" -> "Tyyppi"

    -- Distribution
    "distributionAccessUrl" -> "Julkaisun osoite"
    "distributionDataAccess" -> "Saatavuus"
    "distributionDescription" -> "Kuvaus"
    "distributionDownloadUri" -> "Latausosoite"
    "distributionFormat" -> "Tiedostotyyppi"
    "distributionTitle" -> "Otsikko"
    "distributionLicenses" -> "Lisenssit"

    -- EthicalIssue
    "ethicalIssueDescription" -> "Kuvaus"
    "ethicalIssueExist" -> "Eettisiä haasteita on"
    "ethicalIssueReport" -> "Raportti"

    -- License
    "licenseRef" -> "Käytetty lisenssi"
    "licenseStartDate" -> "Lisenssin käyttöönottopäivä"

    -- Metadata
    "metadataAccessDocumentation" -> "Dokumentaation avoimuus"
    "metadataDataModel" -> "Datamalli"
    "metadataDescription" -> "Kuvaus"
    "metadataLanguage" -> "Kieli"
    "metadataLocationDocumentation" -> "Dokumentaation sijainti"
    "metadataOpen" -> "Metadatan avoimuus"
    "metadataLocation" -> "Metadatan osoite"
    "metadataSchema" -> "Metadata perustuu tietomalliin"
    "metadataMetadataId" -> "Metadatan tunniste"

    -- MetadataId
    "metadataIdIdentifier" -> "Tunniste"
    "metadataIdType" -> "Tyyppi"

    -- Project
    "projectDescription" -> "Kuvaus"
    "projectEndDate" -> "Loppumispäivä"
    "projectStartDate" -> "Alkamispäivä"
    "projectTitle" -> "Otsikko"

    -- RightsRelatedToData
    "rightsOwnershipDataRight" -> "Datan omistaja"

    -- SecurityAndPrivacy
    "securityDescription" -> "Kuvaus"
    "securityTitle" -> "Otsikko"

    _ ->
      str

localizeErrorPathSeg : String -> String
localizeErrorPathSeg str =
  case String.toInt str of
    Just n -> String.fromInt (n + 1)
    Nothing -> localizeDmpConstructor str

localizeServerDecodeErrorMsg : String -> String
localizeServerDecodeErrorMsg str = case str of
  "EMPTY_TEXT" -> "Kenttä on pakollinen."
  _ -> str

badStatusView : BadStatusResponse -> List (Html msg)
badStatusView res =
  [ h6 [] [ text <| "Virhe " ++ String.fromInt res.meta.statusCode ]
  ] ++ (case decodeServerDecodeError res.body of
    Ok de ->
      [ p [] [ text <| "Virhe kentässä: " ++ (String.join " -> " (Array.toList (Array.map localizeErrorPathSeg de.path))) ]
      , p [] [ text <| localizeServerDecodeErrorMsg de.message ]
      ]
    Err str -> [ text str ]
  )

errorResponseView : ErrorResponse -> Html msg
errorResponseView err = div [] <| case err of
  DmpApi.BadUrl str -> [ text <| "Virheellinen URL: " ++ str ]
  DmpApi.Timeout -> [ text "Aikakatkaisu" ]
  DmpApi.NetworkError -> [ text "Verkkovirhe" ]
  DmpApi.BadStatus res -> badStatusView res
  DmpApi.BadBody str -> [ text <| "Virhe: " ++ str ]

