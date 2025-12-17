ALTER TABLE dmps
  DROP COLUMN language,
  DROP COLUMN type_dmp;

DROP TYPE dmp_type;

ALTER TABLE datasets
  DROP COLUMN type;
