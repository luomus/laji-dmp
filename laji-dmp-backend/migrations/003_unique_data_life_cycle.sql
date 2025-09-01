DELETE FROM data_life_cycles;

ALTER TABLE data_life_cycles
  ADD UNIQUE (dmp_id);
