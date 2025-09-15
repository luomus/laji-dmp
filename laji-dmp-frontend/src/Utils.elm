module Utils exposing (..)

import Models exposing (..)
import Http
import DmpApi exposing (ErrorResponse)
import DmpApi exposing (ServerDecodeError)
import Array
import DmpApi exposing (BadStatusResponse)
import DmpApi exposing (decodeServerDecodeError)
import Dict
import Organization exposing (Organization)
import Dict exposing (Dict)
import Organization

showUtcTime : UTCTime -> String
showUtcTime (UTCTime str) = str

showDay : Day -> String
showDay (Day str) = str

langFromStr : String -> LanguageType
langFromStr s = case s of
  "LanguageTypeEn" -> LanguageTypeEn
  "LanguageTypeSv" -> LanguageTypeSv
  _ -> LanguageTypeFi

langToStr : LanguageType -> String
langToStr l = case l of
  LanguageTypeEn -> "LanguageTypeEn"
  LanguageTypeSv -> "LanguageTypeSv"
  _ -> "LanguageTypeFi"

showLanguage : LanguageType -> String
showLanguage lang = case lang of
  LanguageTypeFi -> "Suomi"
  LanguageTypeEn -> "Englanti"
  LanguageTypeSv -> "Ruotsi"

dmpTypeFromStr : String -> DmpType
dmpTypeFromStr s = case s of
  "DmpTypeStudent" -> DmpTypeStudent
  "DmpTypeAcademic" -> DmpTypeAcademic
  "DmpTypeNational" -> DmpTypeNational
  "DmpTypeInternational" -> DmpTypeInternational
  "DmpTypePriodiversityLife" -> DmpTypePriodiversityLife
  _ -> DmpTypeOrganizational

dmpTypeToStr : DmpType -> String
dmpTypeToStr v =
  case v of
    DmpTypeStudent       -> "DmpTypeStudent"
    DmpTypeAcademic      -> "DmpTypeAcademic"
    DmpTypeNational      -> "DmpTypeNational"
    DmpTypeInternational -> "DmpTypeInternational"
    DmpTypeOrganizational -> "DmpTypeOrganizational"
    DmpTypePriodiversityLife -> "DmpTypePriodiversityLife"

showDmpType : DmpType -> String
showDmpType a = case a of
  DmpTypeStudent -> "Opiskelijaprojekti"
  DmpTypeAcademic -> "Akateeminen"
  DmpTypeNational -> "Kansallinen"
  DmpTypeInternational -> "Kansainvälinen"
  DmpTypeOrganizational -> "Organisaation sisäinen"
  DmpTypePriodiversityLife -> "Priodiversity LIFE"

documentIdTypeFromStr : String -> DocumentIdType
documentIdTypeFromStr s = case s of
  "DocumentIdTypeHandle" -> DocumentIdTypeHandle
  "DocumentIdTypeDoi" -> DocumentIdTypeDoi
  "DocumentIdTypeArk" -> DocumentIdTypeArk
  "DocumentIdTypeUrl" -> DocumentIdTypeUrl
  "DocumentIdTypeOther" -> DocumentIdTypeOther
  _                    -> DocumentIdTypeNone

documentIdTypeToStr : DocumentIdType -> String
documentIdTypeToStr v =
  case v of
    DocumentIdTypeHandle -> "DocumentIdTypeHandle"
    DocumentIdTypeDoi    -> "DocumentIdTypeDoi"
    DocumentIdTypeArk    -> "DocumentIdTypeArk"
    DocumentIdTypeUrl    -> "DocumentIdTypeUrl"
    DocumentIdTypeOther  -> "DocumentIdTypeOther"
    DocumentIdTypeNone   -> "DocumentIdTypeNone"

showDocumentIdType : DocumentIdType -> String
showDocumentIdType a = case a of
  DocumentIdTypeHandle -> "handle"
  DocumentIdTypeDoi -> "doi"
  DocumentIdTypeArk -> "ark"
  DocumentIdTypeUrl -> "url"
  DocumentIdTypeOther -> "Muu"
  DocumentIdTypeNone -> "Ei tiedossa"

roleTypeFromStr : String -> RoleType
roleTypeFromStr s = case s of
  "RoleTypeWorkPackageLeader"     -> RoleTypeWorkPackageLeader
  "RoleTypeDataController"        -> RoleTypeDataController
  "RoleTypePrincipleInvestigator" -> RoleTypePrincipleInvestigator
  "RoleTypeAuthorOfDataSet"       -> RoleTypeAuthorOfDataSet
  _                               -> RoleTypeOther

roleTypeToStr : RoleType -> String
roleTypeToStr v =
  case v of
    RoleTypeWorkPackageLeader     -> "RoleTypeWorkPackageLeader"
    RoleTypeDataController        -> "RoleTypeDataController"
    RoleTypePrincipleInvestigator -> "RoleTypePrincipleInvestigator"
    RoleTypeAuthorOfDataSet       -> "RoleTypeAuthorOfDataSet"
    RoleTypeOther                 -> "RoleTypeOther"

showRoleType : RoleType -> String
showRoleType role = case role of
  RoleTypeWorkPackageLeader -> "Työpaketin johtaja"
  RoleTypeDataController -> "Datan hallinnoija"
  RoleTypePrincipleInvestigator -> "Pääasiallinen tutkija"
  RoleTypeAuthorOfDataSet -> "Aineiston tekijä"
  RoleTypeOther -> "Muu"

personIdTypeFromStr : String -> PersonIdType
personIdTypeFromStr s = case s of
  "PersonIdTypeOrcid"  -> PersonIdTypeOrcid
  "PersonIdTypeIsni"   -> PersonIdTypeIsni
  "PersonIdTypeOpenid" -> PersonIdTypeOpenid
  "PersonIdTypeOther"  -> PersonIdTypeOther
  _                    -> PersonIdTypeNone

personIdTypeToStr : PersonIdType -> String
personIdTypeToStr v =
  case v of
    PersonIdTypeOrcid  -> "PersonIdTypeOrcid"
    PersonIdTypeIsni   -> "PersonIdTypeIsni"
    PersonIdTypeOpenid -> "PersonIdTypeOpenid"
    PersonIdTypeOther  -> "PersonIdTypeOther"
    PersonIdTypeNone   -> "PersonIdTypeNone"

showPersonIdType : PersonIdType -> String
showPersonIdType t = case t of
  PersonIdTypeOrcid -> "Orcid"
  PersonIdTypeIsni -> "Isni"
  PersonIdTypeOpenid -> "OpenId"
  PersonIdTypeOther -> "Other"
  PersonIdTypeNone -> "Ei tiedossa"
  
deletionDataTypeFromStr : String -> DeletionDataType
deletionDataTypeFromStr s = case s of
  "DeletionDataTypeYes"     -> DeletionDataTypeYes
  "DeletionDataTypeNo"      -> DeletionDataTypeNo
  _                         -> DeletionDataTypeUnknown

deletionDataTypeToStr : DeletionDataType -> String
deletionDataTypeToStr v =
  case v of
    DeletionDataTypeYes     -> "DeletionDataTypeYes"
    DeletionDataTypeNo      -> "DeletionDataTypeNo"
    DeletionDataTypeUnknown -> "DeletionDataTypeUnknown"

personalDataTypeFromStr : String -> PersonalDataType
personalDataTypeFromStr s = case s of
  "PersonalDataTypeYes"     -> PersonalDataTypeYes
  "PersonalDataTypeNo"      -> PersonalDataTypeNo
  _                         -> PersonalDataTypeUnknown

personalDataTypeToStr : PersonalDataType -> String
personalDataTypeToStr p = case p of
  PersonalDataTypeYes -> "PersonalDataTypeYes"
  PersonalDataTypeNo -> "PersonalDataTypeNo"
  PersonalDataTypeUnknown -> "PersonalDataTypeUnknown"

sensitiveDataTypeFromStr : String -> SensitiveDataType
sensitiveDataTypeFromStr s = case s of
  "SensitiveDataTypeYes"     -> SensitiveDataTypeYes
  "SensitiveDataTypeNo"      -> SensitiveDataTypeNo
  _                          -> SensitiveDataTypeUnknown

sensitiveDataTypeToStr : SensitiveDataType -> String
sensitiveDataTypeToStr v =
  case v of
    SensitiveDataTypeYes -> "SensitiveDataTypeYes"
    SensitiveDataTypeNo -> "SensitiveDataTypeNo"
    SensitiveDataTypeUnknown -> "SensitiveDataTypeUnknown"

ethicalIssuesTypeFromStr : String -> EthicalIssuesType
ethicalIssuesTypeFromStr s = case s of
  "EthicalIssuesTypeYes" -> EthicalIssuesTypeYes
  "EthicalIssuesTypeNo"  -> EthicalIssuesTypeNo
  _                      -> EthicalIssuesTypeUnknown

ethicalIssuesTypeToStr : EthicalIssuesType -> String
ethicalIssuesTypeToStr v =
  case v of
    EthicalIssuesTypeYes -> "EthicalIssuesTypeYes"
    EthicalIssuesTypeNo -> "EthicalIssuesTypeNo"
    EthicalIssuesTypeUnknown -> "EthicalIssuesTypeUnknown"

dataAccessTypeFromStr : String -> DataAccessType
dataAccessTypeFromStr s = case s of
  "DataAccessTypeShared"     -> DataAccessTypeShared
  "DataAccessTypeClosed"     -> DataAccessTypeClosed
  _                          -> DataAccessTypeOpen

dataAccessTypeToStr : DataAccessType -> String
dataAccessTypeToStr s = case s of
  DataAccessTypeShared     -> "DataAccessTypeShared"
  DataAccessTypeClosed     -> "DataAccessTypeClosed"
  _                        -> "DataAccessTypeOpen"

metadataIdTypeFromStr : String -> MetadataIdType
metadataIdTypeFromStr s = case s of
  "MetadataIdTypeUrl"       -> MetadataIdTypeUrl
  "MetadataIdTypeOther"     -> MetadataIdTypeOther
  _                         -> MetadataIdTypeNone

metadataIdTypeToStr : MetadataIdType -> String
metadataIdTypeToStr v =
  case v of
    MetadataIdTypeUrl -> "MetadataIdTypeUrl"
    MetadataIdTypeOther -> "MetadataIdTypeOther"
    MetadataIdTypeNone -> "MetadataIdTypeNone"

showDeletionDataType : DeletionDataType -> String
showDeletionDataType d = case d of
  DeletionDataTypeYes -> "Kyllä"
  DeletionDataTypeNo -> "Ei"
  DeletionDataTypeUnknown -> "Ei tiedossa"

showPersonalDataType : PersonalDataType -> String
showPersonalDataType p = case p of
  PersonalDataTypeYes -> "Kyllä"
  PersonalDataTypeNo -> "Ei"
  PersonalDataTypeUnknown -> "Ei tiedossa"

showSensitiveDataType : SensitiveDataType -> String
showSensitiveDataType p = case p of
  SensitiveDataTypeYes -> "Kyllä"
  SensitiveDataTypeNo -> "Ei"
  SensitiveDataTypeUnknown -> "Ei tiedossa"

showEthicalIssuesType : EthicalIssuesType -> String
showEthicalIssuesType p = case p of
  EthicalIssuesTypeYes -> "Kyllä"
  EthicalIssuesTypeNo -> "Ei"
  EthicalIssuesTypeUnknown -> "Ei tiedossa"

showDataAccessType : DataAccessType -> String
showDataAccessType d = case d of
  DataAccessTypeOpen -> "Avoin"
  DataAccessTypeShared -> "Jaettavissa"
  DataAccessTypeClosed -> "Suljettu"

showMetadataIdType : MetadataIdType -> String
showMetadataIdType m = case m of
  MetadataIdTypeUrl -> "url"
  MetadataIdTypeOther -> "Muu"
  MetadataIdTypeNone -> "Ei tiedossa"

parseMaybe : String -> Maybe String
parseMaybe s = case s of
  "" -> Nothing
  ss -> Just ss

httpErrorToString : Http.Error -> String
httpErrorToString err = case err of
  Http.BadUrl str -> "Virheellinen URL: " ++ str
  Http.Timeout -> "Aikakatkaisu"
  Http.NetworkError -> "Verkkovirhe"
  Http.BadStatus int -> "Virhe " ++ String.fromInt int
  Http.BadBody str -> "Virhe: " ++ str

boolToString : Bool -> String
boolToString b = if b then "True" else "False"

boolFromString : String -> Bool
boolFromString b = case b of
  "True" -> True
  _ -> False

showBool : Bool -> String
showBool b = case b of
  True -> "Kyllä"
  False -> "Ei"

showOrgName : Dmp -> Organization.OrgLookup -> String
showOrgName dmp orgs = case Dict.get dmp.dmpOrgId orgs of
  Just org -> String.join " - " <| List.filterMap identity [org.organizationLevel1, org.organizationLevel2, org.organizationLevel3, org.organizationLevel4]
  Nothing -> dmp.dmpOrgId

