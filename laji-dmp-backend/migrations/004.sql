DROP TABLE rights_related_to_data;

ALTER TABLE data_life_cycles DROP COLUMN deletion_data;
DROP TYPE deletion_data_type;

ALTER TABLE data_life_cycles ADD COLUMN update_frequency TEXT;

ALTER TABLE data_life_cycles DROP COLUMN dmp_id;
ALTER TABLE data_life_cycles ADD COLUMN dataset_id INT NOT NULL REFERENCES datasets(id) ON DELETE CASCADE;

ALTER TABLE metadata DROP COLUMN schema;
ALTER TABLE metadata DROP COLUMN data_model;
ALTER TABLE metadata DROP COLUMN location_documentation;
ALTER TABLE metadata DROP COLUMN access_documentation;
ALTER TABLE metadata ADD COLUMN standards TEXT[];

ALTER TABLE datasets ADD COLUMN vocabulary TEXT[];

ALTER TYPE language_type ADD VALUE 'other';

ALTER TABLE datasets ALTER COLUMN language SET NOT NULL;

ALTER TYPE data_access_type ADD VALUE 'classified';
ALTER TYPE data_access_type ADD VALUE 'embargoed';

ALTER TYPE role_type RENAME TO role_type_old;
CREATE TYPE role_type AS ENUM ('project_data_controller', 'data_owner', 'organization_data_controller', 'dataset_author', 'other');

ALTER TABLE contributors
  ALTER COLUMN role TYPE role_type
  USING 'other'::role_type;

DROP TYPE role_type_old;

ALTER TABLE metadata DROP COLUMN description;

