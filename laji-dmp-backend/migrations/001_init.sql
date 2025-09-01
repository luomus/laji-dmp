DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_type WHERE typname = 'data_access_type'
  ) THEN
    CREATE TYPE data_access_type AS ENUM ('open', 'shared', 'closed');
    CREATE TYPE deletion_data_type AS ENUM ('yes', 'no', 'unknown');
    CREATE TYPE dmp_type AS ENUM ('student', 'academic', 'national', 'international', 'organizational');
    CREATE TYPE document_id_type AS ENUM ('handle', 'doi', 'ark', 'url', 'other', 'none');
    CREATE TYPE ethical_issues_type AS ENUM ('yes', 'no', 'unknown');
    CREATE TYPE language_type AS ENUM ('fi', 'sv', 'en');
    CREATE TYPE metadata_id_type AS ENUM ('url', 'other', 'none');
    CREATE TYPE person_id_type AS ENUM ('orcid', 'isni', 'openid', 'other', 'none');
    CREATE TYPE personal_data_type AS ENUM ('yes', 'no', 'unknown');
    CREATE TYPE role_type AS ENUM ('work_package_leader', 'data_controller', 'principle_investigator', 'author_of_data_set', 'other');
    CREATE TYPE sensitive_data_type AS ENUM ('yes', 'no', 'unknown');
  END IF;
END
$$;

CREATE TABLE IF NOT EXISTS dmps (
  created TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  description TEXT,
  id SERIAL PRIMARY KEY NOT NULL,
  language language_type NOT NULL,
  modified TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  nextreview_dmp DATE,
  org_id TEXT NOT NULL,
  title VARCHAR(80) NOT NULL,
  type_dmp dmp_type NOT NULL
);

CREATE TABLE IF NOT EXISTS contacts (
  dmp_id INT UNIQUE NOT NULL REFERENCES dmps(id) ON DELETE CASCADE,
  id SERIAL PRIMARY KEY NOT NULL,
  mbox VARCHAR(80) NOT NULL,
  name VARCHAR(80) NOT NULL,
  organization VARCHAR(80)
);

CREATE TABLE IF NOT EXISTS contact_ids (
  contact_id INT UNIQUE NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
  id SERIAL PRIMARY KEY NOT NULL,
  identifier VARCHAR(80),
  type person_id_type NOT NULL
);

CREATE TABLE IF NOT EXISTS contributors (
  id SERIAL PRIMARY KEY NOT NULL,
  dmp_id INT NOT NULL REFERENCES dmps(id) ON DELETE CASCADE,
  mbox VARCHAR(80),
  name VARCHAR(80) NOT NULL,
  organization VARCHAR(80),
  role role_type NOT NULL
);

CREATE TABLE IF NOT EXISTS contributor_ids (
  contributor_id INT UNIQUE NOT NULL REFERENCES contributors(id) ON DELETE CASCADE,
  id SERIAL PRIMARY KEY NOT NULL,
  identifier VARCHAR(80),
  type person_id_type NOT NULL
);

CREATE TABLE IF NOT EXISTS data_life_cycles (
  archiving_services_data BOOLEAN NOT NULL,
  backup_data TEXT NOT NULL,
  id SERIAL PRIMARY KEY NOT NULL,
  deletion_data deletion_data_type NOT NULL,
  deletion_when_data DATE,
  dmp_id INT NOT NULL REFERENCES dmps(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS datasets (
  data_quality_assurance TEXT,
  data_sharing_issues TEXT,
  id SERIAL PRIMARY KEY NOT NULL,
  description TEXT,
  dmp_id INT NOT NULL REFERENCES dmps(id) ON DELETE CASCADE,
  issued DATE,
  keywords VARCHAR(80)[],
  language language_type,
  personal_data personal_data_type NOT NULL,
  sensitive_data sensitive_data_type NOT NULL,
  reuse_dataset BOOLEAN,
  title VARCHAR(80) NOT NULL,
  type VARCHAR(80)
);

CREATE TABLE IF NOT EXISTS dataset_ids (
  dataset_id INT NOT NULL REFERENCES datasets(id) ON DELETE CASCADE,
  id SERIAL PRIMARY KEY NOT NULL,
  identifier VARCHAR(80),
  type document_id_type NOT NULL
);

CREATE TABLE IF NOT EXISTS distributions (
  access_url VARCHAR(80),
  data_access data_access_type,
  dataset_id INT UNIQUE NOT NULL REFERENCES datasets(id) ON DELETE CASCADE,
  description TEXT,
  id SERIAL PRIMARY KEY NOT NULL,
  download_uri VARCHAR(80),
  format VARCHAR(80),
  title VARCHAR(80) NOT NULL
);

CREATE TABLE IF NOT EXISTS dmp_ids (
  dmp_id INT UNIQUE NOT NULL REFERENCES dmps(id) ON DELETE CASCADE,
  id SERIAL PRIMARY KEY NOT NULL,
  identifier VARCHAR(80),
  type document_id_type NOT NULL
);

CREATE TABLE IF NOT EXISTS ethical_issues (
  dmp_id INT UNIQUE NOT NULL REFERENCES dmps(id) ON DELETE CASCADE,
  ethical_issues_description TEXT,
  ethical_issues_exist ethical_issues_type NOT NULL,
  ethical_issues_report VARCHAR(80),
  id SERIAL PRIMARY KEY NOT NULL
);

CREATE TABLE IF NOT EXISTS licenses (
  distribution_id INT NOT NULL REFERENCES distributions(id) ON DELETE CASCADE,
  id SERIAL PRIMARY KEY NOT NULL,
  license_ref VARCHAR(80) NOT NULL,
  start_date DATE NOT NULL
);

CREATE TABLE IF NOT EXISTS metadata (
  access_documentation BOOLEAN,
  dataset_id INT NOT NULL REFERENCES datasets(id) ON DELETE CASCADE,
  data_model VARCHAR(80),
  description TEXT,
  language language_type NOT NULL,
  location_documentation VARCHAR(80),
  id SERIAL PRIMARY KEY NOT NULL,
  metadata_open BOOLEAN,
  metadata_location VARCHAR(80),
  schema BOOLEAN
);

CREATE TABLE IF NOT EXISTS metadata_ids (
  metadata_id INT UNIQUE NOT NULL REFERENCES metadata(id) ON DELETE CASCADE,
  id SERIAL PRIMARY KEY NOT NULL,
  identifier VARCHAR(80),
  type metadata_id_type NOT NULL
);

CREATE TABLE IF NOT EXISTS projects (
  description TEXT NOT NULL,
  end_date DATE,
  dmp_id INT NOT NULL REFERENCES dmps(id) ON DELETE CASCADE,
  id SERIAL PRIMARY KEY NOT NULL,
  start_date DATE NOT NULL,
  title VARCHAR(80) NOT NULL
);

CREATE TABLE IF NOT EXISTS rights_related_to_data (
  dataset_id INT NOT NULL REFERENCES datasets(id) ON DELETE CASCADE,
  ownership_data_right VARCHAR(80),
  id SERIAL PRIMARY KEY NOT NULL
);

CREATE TABLE IF NOT EXISTS security_and_privacy (
  description TEXT NOT NULL,
  dataset_id INT NOT NULL REFERENCES datasets(id) ON DELETE CASCADE,
  id SERIAL PRIMARY KEY NOT NULL,
  title VARCHAR(80) NOT NULL
);
