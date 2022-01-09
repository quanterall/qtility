# qtility

A monorepo containing utility libraries for application development. The repository is built on some
basic ideas that follow throughout:

- `MonadReader` is a good basis for most applications as well as a good way to make dependencies for
  functions work across many different contexts.
- MTL-style in general is how we make code easier to reuse.
- If we have lenses/prisms in mind for the code we write, we'll have an easier time using library
  functionality.
- Using `try`, `mapException`, `fromEither`, `fromEitherM`, etc. makes it so that we don't have to
  worry so much about exactly which error interface we're using, but can decide more flexibly at the
  call-site what to do.

## `qtility`

This is a standard library/prelude version of several qtility libraries as well as common exports.
This includes:

- `RIO`
- Common parts of `Data.Aeson` (including lenses for `AesonOptions`)
- Re-works of `fromX` functions that take `Maybe`/`Either` and throw from them (`qtility-data`)
- TemplateHaskell helpers for JSON & lenses (`qtility-th`)
- Utilities for dealing with environment variables/`.env` files (`qtility-environment`)

## Other, more specific libraries

- `qaws`: Utilities for common general AWS tasks via `amazonka`
- `qaws-sqs`: Utilities for common usage of AWS Simple Queue Service (SQS).
- `qaws-s3`: Utilities for common usage of AWS Simple Storage Service (S3).
- `qtility-brick`: Utilities for writing `brick` applications.
- `qtility-code`: Utilities for executing/interpreting Haskell code.
- `mortred`: Utilities for web scraping/frontend testing via `hs-webdriver` & Selenium.

## Depending on this repo

If you want to use this repository and its packages, add the following to your `stack.yaml`:

```yaml
extra-deps:
- github: quanterall/qtility
  commit: b3648dd7a6cef282a956bd01cce6428d0ba58bc9
  subdirs:
  - qtility
  - qtility-brick
  - qaws
  - qaws-s3
```

Note that you probably want the latest commit.
