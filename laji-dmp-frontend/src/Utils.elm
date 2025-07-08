module Utils exposing (..)

import Models exposing (..)

showUtcTime : UTCTime -> String
showUtcTime (UTCTime str) = str

showDay : Day -> String
showDay (Day str) = str

showLanguage : LanguageType -> String
showLanguage lang = case lang of
  LanguageTypeFi -> "finnish"
  LanguageTypeEn -> "english"
  LanguageTypeSv -> "swedish"

showDmpType : DmpType -> String
showDmpType a = case a of
  DmpTypeStudent -> "student"
  DmpTypeAcademic -> "academic"
  DmpTypeNational -> "national"
  DmpTypeInternational -> "international"
  DmpTypeOrganizational -> "organizational"

showDocumentIdType : DocumentIdType -> String
showDocumentIdType a = case a of
  DocumentIdTypeHandle -> "handle"
  DocumentIdTypeDoi -> "doi"
  DocumentIdTypeArk -> "ark"
  DocumentIdTypeUrl -> "url"
  DocumentIdTypeOther -> "other"
  DocumentIdTypeNone -> "none"

showRoleType : RoleType -> String
showRoleType role = case role of
  RoleTypeWorkPackageLeader -> "Work package leader"
  RoleTypeDataController -> "Data controller"
  RoleTypePrincipleInvestigator -> "Principle investigator"
  RoleTypeAuthorOfDataSet -> "Author of dataset"
  RoleTypeOther -> "Other"

showPersonIdType : PersonIdType -> String
showPersonIdType t = case t of
  PersonIdTypeOrcid -> "Orcid"
  PersonIdTypeIsni -> "Isni"
  PersonIdTypeOpenid -> "OpenId"
  PersonIdTypeOther -> "Other"
  PersonIdTypeNone -> "None"
  
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

