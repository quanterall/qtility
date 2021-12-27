# qtility

## Packages

A monorepo containing utility libraries for application development.

- `qtility-environment`: Utilities for dealing with environment loading and management.
- `qtility-data`: Misc. utilities for data, i.e. functions like `note` & `hush`.

## Depending on this repo

If you want to use this repository and its packages, add the following to your `stack.yaml`:

```yaml
extra-deps:
- github: quanterall/qtility
  commit: 84290f3dd5d9fb8629a3c60888a590eba81dca1c
  subdirs:
  - qtility-data
  - qtility-environment
```

Note that you probably want the latest commit.
