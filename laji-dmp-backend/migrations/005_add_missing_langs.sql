UPDATE datasets
SET language = 'other'::language_type
WHERE language IS NULL;
