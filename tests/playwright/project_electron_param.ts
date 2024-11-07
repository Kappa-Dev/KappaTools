// Allows to define an electron project and use this param
// See https://playwright.dev/docs/test-parameterize#parameterized-projects
import { test as test_base } from '@playwright/test';
import { _electron as electron } from 'playwright';

const electron_app_entry_point_path = 'build/Kappapp/resources/app/main.js';
const electron_exe_path = 'build/Kappapp/kappapp';

export type TestOptions = {
  run_in_electron: boolean;
}

export const RUN_DOWNLOADS_IN_ELECTRON = false;

// Note: for adding features https://playwright.dev/docs/api/class-test#test-extend
export const test = test_base.extend<TestOptions>({
  // Define an option and provide a default value.
  // We can later override it in the config.
  run_in_electron: [false, { option: true }],

  // setup electron page if necessary
  page: async ({ page, run_in_electron }, use) => {
    if (run_in_electron) {
      // TODO: check this: probably need to setup this as a fixture ?
      console.info("Setting up electron app");
      const electronApp = await electron.launch({
        args: [
          "--ignore-gpu-blacklist",
          // "--enable-logging",
          electron_app_entry_point_path
        ],
        executablePath: electron_exe_path
      });

      // TODO: remove?
      // Evaluation expression in the Electron context.
      const appPath = await electronApp.evaluate(async ({ app }) => {
        // This runs in the main Electron process, parameter here is always
        // the result of the require('electron') in the main app script.
        return app.getAppPath();
      });
      console.log("App path: ", appPath);

      const window = await electronApp.firstWindow();
      // Direct Electron console to Node terminal.
      window.on('console', console.log);

      console.log(await window.title());

      // TODO: add this to a fixture to close the app
      // await electronApp.close();

      await use(window);
      window
    } else {
      // TODO: check if useful
      await use(page);
      page
    }
  },
});

// TODO: fixture for launching electron if selected, and setting up page ? YES
