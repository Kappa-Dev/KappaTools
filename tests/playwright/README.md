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

### Electron

Local tests with electron need to have access to the actual screen, it seems. 
So they should be run with `-j 1` to have a single worker, and the electron window has to stay visible at least.

Downloads open a dialog that is not handled by playwright, so there is no testing of electron downloads.
They may be enabled and completed manually by changing the boolean in `project_electron_param.ts` (untested).
