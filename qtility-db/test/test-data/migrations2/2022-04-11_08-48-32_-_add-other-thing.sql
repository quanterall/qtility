CREATE TABLE other_thing (
  id bigserial PRIMARY KEY,
  name text NOT NULL
);

-- DOWN

DROP TABLE other_thing;