DELETE FROM dmps d
WHERE NOT EXISTS (
  SELECT 1
  FROM contacts c
  WHERE c.dmp_id = d.id
)
OR NOT EXISTS (
  SELECT 1
  FROM dmp_ids di
  WHERE di.dmp_id = d.id
);
