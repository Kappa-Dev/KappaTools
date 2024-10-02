// Kappapp tests
// Note: trace snapshots that should be taken by playwright are not (absent on right of ui `npx playwright test --ui`)

import { test, expect, type Page, type Download } from '@playwright/test';
// import * as fs from 'fs';
// import * as path from 'path';
import fs from 'node:fs';
import path from 'node:path';

test.describe.configure({ mode: 'parallel' });

test.beforeEach(async ({ page }, testInfo) => {
  page; // TODO: remove
  testInfo.setTimeout(testInfo.timeout + 30000);
});

// const url = 'https://tools.kappalanguage.org/try/'
const url = 'http://127.0.0.1:12345/index.html'
const arg_set_model = '?model=https%3A'
const abc_ka = '//raw.githubusercontent.com/Kappa-Dev/KappaTools/master/examples/abc.ka'

const referencesDir = 'tests/playwright/refs/'

async function open_app_with_model(page: Page, url_protocol_relative: string) {
  await page.goto(url + arg_set_model + url_protocol_relative);
  await expect(page.locator('.nodeArcPath').first()).toBeVisible();
}

function get_error_field(page: Page) {
  return page.locator('#configuration_error_div');
}

async function set_pause_if(page: Page, s: string) {
  await page.getByPlaceholder('[T] >').click();
  await page.getByPlaceholder('[T] >').fill(s);
  await page.getByPlaceholder('[T] >').press('Enter');
}


async function write_ref(download: Download, fileName: string) {
  const filePath = await download.path();
  expect.soft(filePath).toBeTruthy();
  const refFilePath = path.join(referencesDir, fileName);
  fs.copyFileSync(filePath, refFilePath);
}

async function compare_download_to_ref(download: Download, fileName: string) {
  const filePath = await download.path();
  expect.soft(filePath).toBeTruthy();
  const refFilePath = path.join(referencesDir, fileName);
  const downloadedContent = await fs.promises.readFile(filePath, 'utf8');
  const referenceContent = await fs.promises.readFile(refFilePath, 'utf8');
  expect.soft(downloadedContent).toBe(referenceContent);
}

async function testExports(page: Page, exportLocatorPrefix: string, fileBaseName: string, extensions: string[], writeRef: boolean = false) {
  const exportFilenameLoc = page.locator(exportLocatorPrefix + "_filename");
  const exportSelectLoc = page.locator(exportLocatorPrefix + "_select");

  for (const extension of extensions) {
    const fileName = `${fileBaseName}.${extension}`;
    await exportSelectLoc.selectOption(extension);
    await exportFilenameLoc.click();
    await exportFilenameLoc.fill(fileName);

    const downloadPromise = page.waitForEvent('download');
    await page.getByRole('button', { name: 'export' }).click();
    const download = await downloadPromise;

    if (writeRef || process.env.UPDATE_EXPORTS === 'true') {
      await write_ref(download, fileName);
    }
    await compare_download_to_ref(download, fileName);
  }
}

async function setSeed(page: Page, seed: number) {
  await page.getByRole('list').locator('a').nth(2).click();
  await page.getByRole('spinbutton', { name: 'Seed' }).click();
  await page.getByRole('spinbutton', { name: 'Seed' }).fill(seed.toString());
  await page.getByRole('button', { name: 'Set', exact: true }).click();
  // wait for modal to close
  await expect(page.locator('div:nth-child(5) > .col-md-2')).toBeHidden();
}

test.describe('Editor tab', () => {

  test('editor', async ({ page }) => {
    await open_app_with_model(page, abc_ka);

    const editor = page.locator('#editor-panel').getByRole('textbox');
    async function editor_to_line(n: number): Promise<void> {
      await page.locator('div:nth-child(' + n + ') > .CodeMirror-line').click();
    }
    async function editor_cancel(): Promise<void> {
      await editor.press('ControlOrMeta+z');
    }
    const error_field = get_error_field(page);
    async function expect_error(text: string[]): Promise<void> {
      await expect.soft(error_field).toHaveText(text);
    }

    await page.locator('div:nth-child(21) > .CodeMirror-line').click();
    await editor_to_line(21);
    // Make a syntax error
    await editor.press('Backspace');
    // (useless comment to match brackets { {)
    await expect_error([
      " « 1/1 » [abc.ka] invalid internal state or missing '}' ",
      " invalid internal state or missing '}' ",
    ]);
    await editor_cancel();
    await expect_error([
      " «  » ",
      "",
    ]);
    await editor_to_line(24);
    await editor.fill('\n%agent: D(a{u p})');
    await expect_error([
      " « 1/1 » [abc.ka] Dead agent D ",
      " Dead agent D ",
    ]);
    await editor_to_line(25);
    await editor.fill("\n'd' D(a{p}) -> D(a{u}) @ 1");
    await expect_error([
      " « 1/4 » [abc.ka] Dead agent D ",
      " Dead agent D ",
    ]);
    await editor_cancel();
    await editor_cancel();
    await expect_error([
      " «  » ",
      "",
    ]);
  });

  test('contact_map', async ({ page }) => {
    await open_app_with_model(page, abc_ka);
    const contact_map = page.locator('#map-container');
    await expect.soft(contact_map).toHaveScreenshot();
    await page.getByRole('checkbox', { name: 'Interactive Mode' }).check();
    await expect.soft(contact_map).toHaveScreenshot();
    await page.getByRole('checkbox', { name: 'Interactive Mode' }).uncheck();
    await expect.soft(contact_map).toHaveScreenshot();
    await page.getByRole('button', { name: 'Show All States' }).hover();
    await page.getByRole('button', { name: 'Show All States' }).click();
    await expect.soft(contact_map).toHaveScreenshot();
    await page.getByRole('button', { name: 'Hide All States' }).click();
    await expect.soft(contact_map).toHaveScreenshot();
    await contact_map.hover();
    await page.mouse.wheel(0, 100);
    await expect.soft(contact_map).toHaveScreenshot();
    await page.getByRole('button', { name: 'Reset Zoom' }).click();
    await expect.soft(contact_map).toHaveScreenshot();

    //export
    await testExports(page, '#export_contact-export', 'map', ['svg', 'png', 'json']);
  });

  test('influences', async ({ page }) => {
    const opts_screen = { threshold: 0.4 }
    await open_app_with_model(page, abc_ka);
    await page.getByRole('tab', { name: 'influences' }).click();
    await expect.soft(page.locator('#influences-table')).toHaveScreenshot();
    await expect.soft(page.getByRole('cell', { name: 'Navigate through the nodes' })).toBeVisible();
    await page.getByRole('button', { name: 'First node' }).click();
    await expect.soft(page.locator('#influences-table')).toHaveScreenshot();
    await page.locator('#influence-rendering').selectOption('graph');
    await expect.soft(page.getByRole('img')).toHaveScreenshot(opts_screen);
    await page.locator('#influence-accuracy').selectOption('high');
    await expect.soft(page.getByRole('img')).toHaveScreenshot(opts_screen);
    await page.getByRole('button', { name: 'Next' }).click();
    await expect.soft(page.getByRole('img')).toHaveScreenshot(opts_screen);
    await page.getByRole('spinbutton', { name: 'Navigate' }).fill('2');
    await page.getByRole('spinbutton', { name: 'Navigate' }).press('Enter');
    await expect.soft(page.getByRole('img')).toHaveScreenshot(opts_screen);
    await page.locator('#influence-rendering').selectOption('tabular');
    await page.getByRole('button', { name: 'Track cursor' }).click();
    //await page.locator('.CodeMirror-activeline > .CodeMirror-line > span > span:nth-child(7)').click();
    // TODO: assert text
    // await expect.soft(page.locator('#influence-table')).toHaveScreenshot();
    await page.locator('div:nth-child(10) > .CodeMirror-line > span > span:nth-child(7)').scrollIntoViewIfNeeded();
    await page.locator('div:nth-child(10) > .CodeMirror-line > span > span:nth-child(7)').click();
    await expect.soft(page.locator('#influences-table')).toHaveScreenshot();
    await page.locator('div:nth-child(11) > .CodeMirror-line > span > span:nth-child(7)').click();
    await expect.soft(page.locator('#influences-table')).toHaveScreenshot();
    await page.getByRole('button', { name: 'Next' }).click();
    await expect.soft(page.locator('#influences-table')).toHaveScreenshot();
    await page.getByRole('button', { name: 'Previous' }).click();
    await expect.soft(page.locator('#influences-table')).toHaveScreenshot();
    //export
    await testExports(page, '#export_influence-export', 'influences', ['json']);
  });

  test('constraints_and_polymers', async ({ page }) => {
    await open_app_with_model(page, abc_ka);
    await page.getByRole('tab', { name: 'constraints' }).click();
    await expect.soft(page.locator('.panel-scroll > div > .panel-body').first()).toHaveText(
      `A(c)  =>  [ A(c[.]) v A(c[x1.C]) v A(c[x2.C]) ]
A(x)  =>  [ A(x[.]) v A(x[x.B]) ]
B(x)  =>  [ B(x[.]) v B(x[x.A]) ]
C(x1)  =>  [ C(x1{u}) v C(x1{p}) ]
C(x1)  =>  [ C(x1[.]) v C(x1[c.A]) ]
C(x2)  =>  [ C(x2[.]) v C(x2[c.A]) ]
C(x2)  =>  [ C(x2{u}) v C(x2{p}) ]
`
    );
    await expect.soft(page.locator('div:nth-child(2) > .panel-body')).toHaveText(
      "C()  =>  [ C(x1{p}[.],x2{u}[.]) v C(x1{p}[.],x2{u}[c.A]) v C(x1{p}[.],x2{p}[.]) v C(x1{u}[c.A],x2{u}[.]) v C(x1{u}[.],x2{u}[.]) ]"

    );
    await page.getByRole('tab', { name: 'polymers' }).click();
    await expect.soft(page.getByRole('paragraph')).toHaveText(
      "The size of biomolecular compounds is uniformly bounded."
    );
  });
})

test.describe('Simulation tools', () => {

  test('Simulation, plot', async ({ page }) => {
    async function wait_for_sim_stop(page: Page) {
      await page.waitForTimeout(200);
      await expect(page.getByRole('button', { name: 'intervention' })).toBeVisible({ timeout: 20000 });
    }
    await open_app_with_model(page, abc_ka);
    await setSeed(page, 1);
    // Run simulation to 30, then 100, then test plot options
    await set_pause_if(page, '[T] > 30');
    await page.getByRole('button', { name: 'start' }).click();
    await wait_for_sim_stop(page);
    await page.getByRole('tab', { name: 'plot New' }).click();
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await set_pause_if(page, '[T] > 100');
    await page.getByRole('button', { name: 'continue' }).click();
    await wait_for_sim_stop(page);
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await page.getByRole('checkbox', { name: 'Log X' }).check();
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await page.getByRole('checkbox', { name: 'Log Y' }).check();
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await page.getByRole('checkbox', { name: 'Log X' }).uncheck();
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await page.getByRole('checkbox', { name: 'Log Y' }).uncheck();
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await page.locator('#plot-axis-select').getByRole('combobox').selectOption('0');
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await page.locator('#plot-axis-select').getByRole('combobox').selectOption('4');
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await page.getByRole('spinbutton', { name: 'Row to plot' }).click();
    await page.getByRole('spinbutton', { name: 'Row to plot' }).fill('50');
    await page.getByRole('spinbutton', { name: 'Row to plot' }).press('Enter');
    await page.locator('.panel-footer').click(); // needed for update
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await page.getByRole('spinbutton', { name: 'Row to plot' }).click();
    await page.getByRole('spinbutton', { name: 'Row to plot' }).fill('1000'); // previous default value
    await page.getByRole('spinbutton', { name: 'Row to plot' }).press('Enter');
    await page.locator('.panel-footer').click(); // needed for update
    await expect.soft(page.getByRole('img')).toHaveScreenshot();

    await testExports(page, '#export_plot-export', 'plot', ['svg', 'csv', 'json', 'tsv', 'png']);

    // Test larger plots, slider
    await set_pause_if(page, '[T] > 2000');
    await page.getByRole('button', { name: 'continue' }).click();
    await wait_for_sim_stop(page);
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await page.getByRole('button', { name: 'continue' }).click();
    await page.getByPlaceholder('offset').fill('0');
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await page.getByPlaceholder('offset').fill('83');
    await expect.soft(page.getByRole('img')).toHaveScreenshot();

    await set_pause_if(page, '');
    await page.getByRole('button', { name: 'continue' }).click();
    await expect.soft(page.locator('.row > .col-xs-3').first()).toHaveText("events");
    await expect.soft(page.locator('.col-md-5 > div:nth-child(2) > .col-xs-3')).toHaveText("time");
    await page.getByRole('button', { name: 'pause' }).click();
  });

  test('DIN', async ({ page }) => {
    await open_app_with_model(page, abc_ka);
    await setSeed(page, 1);

    async function expectScreenShotDINTable() {
      await expect.soft(page.getByRole('cell', { name: 'affects' })).toBeVisible();
      await expect.soft(page.locator('#DIN div').first()).toHaveScreenshot({ mask: [page.locator('#export_din-export_form')] });
    }

    // Run simulation to 30, then 100, then test plot options
    await set_pause_if(page, '[T] > 30');
    await page.getByRole('button', { name: 'start' }).click();
    await page.getByRole('tab', { name: 'DIN' }).click();
    await expectScreenShotDINTable();

    await testExports(page, '#export_din-export', 'flux', ['json', 'dot', 'html']);

    await set_pause_if(page, '[T] > 60');
    await page.getByRole('button', { name: 'continue' }).click();
    await expectScreenShotDINTable();
    await page.getByRole('combobox').first().selectOption('flux.json');
    await expectScreenShotDINTable();

    await testExports(page, '#export_din-export', 'flux_json', ['json', 'dot', 'html']);

    await page.getByRole('combobox').first().selectOption('flux.html');
    await expectScreenShotDINTable();
  });

  test('snapshots, outputs', async ({ page }) => {
    await open_app_with_model(page, abc_ka);
    await setSeed(page, 1);
    // Run simulation to 30, then 100, then test plot options
    set_pause_if(page, '[T] > 30');
  });
});

