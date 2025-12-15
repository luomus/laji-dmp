CREATE TYPE data_type AS ENUM (
  'citizen_science_data',
  'collection',
  'field_observation',
  'laser_scanning',
  'model',
  'molecular_biology',
  'remote_sensing',
  'report',
  'satellite_images_and_ortophotos',
  'spatial_data',
  'other'
);

ALTER TABLE datasets
  ADD COLUMN responsible_party_title TEXT NOT NULL DEFAULT '',
  ADD COLUMN responsible_party_email TEXT NOT NULL DEFAULT '',
  ADD COLUMN lineage TEXT,
  ADD COLUMN share_to_syke BOOLEAN NOT NULL DEFAULT FALSE,
  ADD COLUMN data_type data_type NOT NULL DEFAULT 'other';
