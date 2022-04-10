CREATE TABLE that_thing (
  id bigserial PRIMARY KEY,
  name text NOT NULL,
  extra_field text,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL
);

-- DOWN

DROP TABLE that_thing CASCADE;