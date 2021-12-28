# qaws

Useful abstractions for dealing with core `amazonka` usage.

## `loadAWSEnvironment`

Loads an `EnvironmentFile` value into the current process and attempts to create a AWS environment
from it. This involves looking in your environment values, expecting the following to be set in the
file:

- AWS_ACCESS_KEY_ID
- AWS_SECRET_ACCESS_KEY
- AWS_REGION

## `runAWS`

Executes an `AWSRequest a` and returns the associated result for that command. This looks for the
AWS environment in your current monad via `MonadReader` and `AWS.HasEnv`, so you do not need to
provide one to the function.

## `tryRunAWS`

While `runAWS` returns a `m (AWS.Rs a)`, this instead returns a `m (Either AWS.Error (AWS.Rs a))`,
which makes it easier to take care of common AWS error types and to guarantee that you are not just
either swallowing them or getting them as runtime exceptions.

## `runAWS'`

A version of `runAWS` that instead takes `AWS.Env` as an argument, allowing more flexible use if
needed.

## `tryRunAWS'`

A version of `runAWS'` that takes care to either return an `AWS.Error` or a succesful response.

## `loadEnvFile`

Imperatively loads a `.env` file into the current process. This uses `parseDotEnv` internally.

## `parseDotEnv`

Parses a `.env` file and returns a `[(String, String)]`.
