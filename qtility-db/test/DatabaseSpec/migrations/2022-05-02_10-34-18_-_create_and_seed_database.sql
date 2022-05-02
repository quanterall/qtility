CREATE TABLE "test_has_one" (
    "id" bigserial PRIMARY KEY,
    "name" text NOT NULL,
    "created_at" timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO "test_has_one" ("name") VALUES ('test1');

CREATE TABLE "test_has_none" (
    "id" bigserial PRIMARY KEY,
    "name" text NOT NULL,
    "created_at" timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE "test_has_many" (
    "id" bigserial PRIMARY KEY,
    "name" text NOT NULL,
    "created_at" timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO "test_has_many" ("name") VALUES ('test1'), ('test2');

-- DOWN

DROP TABLE "test_has_one";
DROP TABLE "test_has_none";
DROP TABLE "test_has_many";
