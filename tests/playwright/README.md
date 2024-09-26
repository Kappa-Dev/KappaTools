This directory contains the webapp tests, that use playwright.

### Install/update playwright

Regularly update playwright to test on up to date browsers, and match the CI version.

```sh
npm install -D @playwright/test@latest
npx playwright install --with-deps
```

### Install deps

```sh
npm install --save-dev @types/node @types/yauzl
```

### Recompute screenshots and downloaded exports

`UPDATE_EXPORTS=true` is similar to playwright `--update-snapshots`, but for downloaded files

```sh
UPDATE_EXPORTS=true npx playwright test --update-snapshots
UPDATE_EXPORTS=true npx playwright test --update-snapshots --project firefox
UPDATE_EXPORTS=true npx playwright test procedure.spec.ts:449 --update-snapshots
```
