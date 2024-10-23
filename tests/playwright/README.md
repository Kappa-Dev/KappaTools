This directory contains the webapp tests, that use playwright.

### Install deps

```sh
npm install --save-dev @types/node @types/yauzl
```

### Recompute screenshots and downloaded exports

```sh
UPDATE_EXPORTS=true npx playwright test --update-snapshots --project firefox
```

for a given test or test group

```sh
UPDATE_EXPORTS=true npx playwright procedure.spec.ts:449 files --update-snapshots --project firefox     
```
