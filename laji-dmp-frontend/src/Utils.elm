module Utils exposing (..)

import Models exposing (..)

showUtcTime : UTCTime -> String
showUtcTime (UTCTime str) = str

showDay : Day -> String
showDay (Day str) = str

langFromStr : String -> LanguageType
langFromStr s = case s of
  "LanguageTypeEn" -> LanguageTypeEn
  "LanguageTypeSv" -> LanguageTypeSv
  _ -> LanguageTypeFi

showLanguage : LanguageType -> String
showLanguage lang = case lang of
  LanguageTypeFi -> "finnish"
  LanguageTypeEn -> "english"
  LanguageTypeSv -> "swedish"

dmpTypeFromStr : String -> DmpType
dmpTypeFromStr s = case s of
  "DmpTypeStudent" -> DmpTypeStudent
  "DmpTypeAcademic" -> DmpTypeAcademic
  "DmpTypeNational" -> DmpTypeNational
  "DmpTypeInternational" -> DmpTypeInternational
  _ -> DmpTypeOrganizational

showDmpType : DmpType -> String
showDmpType a = case a of
  DmpTypeStudent -> "student"
  DmpTypeAcademic -> "academic"
  DmpTypeNational -> "national"
  DmpTypeInternational -> "international"
  DmpTypeOrganizational -> "organizational"

documentIdTypeFromStr : String -> DocumentIdType
documentIdTypeFromStr s = case s of
  "DocumentIdTypeHandle" -> DocumentIdTypeHandle
  "DocumentIdTypeDoi" -> DocumentIdTypeDoi
  "DocumentIdTypeArk" -> DocumentIdTypeArk
  "DocumentIdTypeUrl" -> DocumentIdTypeUrl
  "DocumentIdTypeOther" -> DocumentIdTypeOther
  _                    -> DocumentIdTypeNone

showDocumentIdType : DocumentIdType -> String
showDocumentIdType a = case a of
  DocumentIdTypeHandle -> "handle"
  DocumentIdTypeDoi -> "doi"
  DocumentIdTypeArk -> "ark"
  DocumentIdTypeUrl -> "url"
  DocumentIdTypeOther -> "other"
  DocumentIdTypeNone -> "none"

roleTypeFromStr : String -> RoleType
roleTypeFromStr s = case s of
  "RoleTypeWorkPackageLeader"     -> RoleTypeWorkPackageLeader
  "RoleTypeDataController"        -> RoleTypeDataController
  "RoleTypePrincipleInvestigator" -> RoleTypePrincipleInvestigator
  "RoleTypeAuthorOfDataSet"       -> RoleTypeAuthorOfDataSet
  _                               -> RoleTypeOther

showRoleType : RoleType -> String
showRoleType role = case role of
  RoleTypeWorkPackageLeader -> "Work package leader"
  RoleTypeDataController -> "Data controller"
  RoleTypePrincipleInvestigator -> "Principle investigator"
  RoleTypeAuthorOfDataSet -> "Author of dataset"
  RoleTypeOther -> "Other"

personIdTypeFromStr : String -> PersonIdType
personIdTypeFromStr s = case s of
  "PersonIdTypeOrcid"  -> PersonIdTypeOrcid
  "PersonIdTypeIsni"   -> PersonIdTypeIsni
  "PersonIdTypeOpenid" -> PersonIdTypeOpenid
  "PersonIdTypeOther"  -> PersonIdTypeOther
  _                    -> PersonIdTypeNone

showPersonIdType : PersonIdType -> String
showPersonIdType t = case t of
  PersonIdTypeOrcid -> "Orcid"
  PersonIdTypeIsni -> "Isni"
  PersonIdTypeOpenid -> "OpenId"
  PersonIdTypeOther -> "Other"
  PersonIdTypeNone -> "None"
  
deletionDataTypeFromStr : String -> DeletionDataType
deletionDataTypeFromStr s = case s of
  "DeletionDataTypeYes"     -> DeletionDataTypeYes
  "DeletionDataTypeNo"      -> DeletionDataTypeNo
  _                         -> DeletionDataTypeUnknown

personalDataTypeFromStr : String -> PersonalDataType
personalDataTypeFromStr s = case s of
  "PersonalDataTypeYes"     -> PersonalDataTypeYes
  "PersonalDataTypeNo"      -> PersonalDataTypeNo
  _                         -> PersonalDataTypeUnknown

sensitiveDataTypeFromStr : String -> SensitiveDataType
sensitiveDataTypeFromStr s = case s of
  "SensitiveDataTypeYes"     -> SensitiveDataTypeYes
  "SensitiveDataTypeNo"      -> SensitiveDataTypeNo
  _                          -> SensitiveDataTypeUnknown

ethicalIssuesTypeFromStr : String -> EthicalIssuesType
ethicalIssuesTypeFromStr s = case s of
  "EthicalIssuesTypeYes" -> EthicalIssuesTypeYes
  "EthicalIssuesTypeNo"  -> EthicalIssuesTypeNo
  _                      -> EthicalIssuesTypeUnknown

dataAccessTypeFromStr : String -> DataAccessType
dataAccessTypeFromStr s = case s of
  "DataAccessTypeShared"     -> DataAccessTypeShared
  "DataAccessTypeClosed"     -> DataAccessTypeClosed
  _                          -> DataAccessTypeOpen

metadataIdTypeFromStr : String -> MetadataIdType
metadataIdTypeFromStr s = case s of
  "MetadataIdTypeUrl"       -> MetadataIdTypeUrl
  "MetadataIdTypeOther"     -> MetadataIdTypeOther
  _                         -> MetadataIdTypeNone

showDeletionDataType : DeletionDataType -> String
showDeletionDataType d = case d of
  DeletionDataTypeYes -> "Yes"
  DeletionDataTypeNo -> "No"
  DeletionDataTypeUnknown -> "Unknown"

showPersonalDataType : PersonalDataType -> String
showPersonalDataType p = case p of
  PersonalDataTypeYes -> "yes"
  PersonalDataTypeNo -> "no"
  PersonalDataTypeUnknown -> "unknown"

showSensitiveDataType : SensitiveDataType -> String
showSensitiveDataType p = case p of
  SensitiveDataTypeYes -> "yes"
  SensitiveDataTypeNo -> "no"
  SensitiveDataTypeUnknown -> "unknown"

showEthicalIssuesType : EthicalIssuesType -> String
showEthicalIssuesType p = case p of
  EthicalIssuesTypeYes -> "yes"
  EthicalIssuesTypeNo -> "no"
  EthicalIssuesTypeUnknown -> "unknown"

showDataAccessType : DataAccessType -> String
showDataAccessType d = case d of
  DataAccessTypeOpen -> "open"
  DataAccessTypeShared -> "shared"
  DataAccessTypeClosed -> "closed"

showMetadataIdType : MetadataIdType -> String
showMetadataIdType m = case m of
  MetadataIdTypeUrl -> "url"
  MetadataIdTypeOther -> "other"
  MetadataIdTypeNone -> "none"

parseMaybe : String -> Maybe String
parseMaybe s = case s of
  "" -> Nothing
  ss -> Just ss

