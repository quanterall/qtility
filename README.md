# qtility

## Packages

A monorepo containing utility libraries for application development.

- `qtility-environment`: Utilities for dealing with environment loading and management.
- `qtility-data`: Utilities for data, i.e. functions like `note` & `hush`.
- `qaws`: Utilities for common general AWS tasks via `amazonka`
- `qaws-sqs`: Utilities for common usage of AWS Simple Queue Service (SQS).
- `qaws-s3`: Utilities for common usage of AWS Simple Storage Service (S3).
- `quanterall-brick`: Utilities for writing `brick` applications.
- `mortred`: Utilities for web scraping/frontend testing via `hs-webdriver` & Selenium.

## Depending on this repo

If you want to use this repository and its packages, add the following to your `stack.yaml`:

```yaml
extra-deps:
- github: quanterall/qtility
  commit: 84290f3dd5d9fb8629a3c60888a590eba81dca1c
  subdirs:
  - qtility-data
  - qtility-environment
  - quanterall-brick
```

Note that you probably want the latest commit.
