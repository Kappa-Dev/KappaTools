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
  await expect(page.getByRole('button', { name: 'Show All States' })).toBeVisible();
}

function get_error_field(page: Page) {
  return page.locator('#configuration_error_div');
}

// TODO: fix unused warning
async function write_ref(download: Download, fileName: string) {
  const filePath = await download.path();
  expect(filePath).toBeTruthy();
  const refFilePath = path.join(referencesDir, fileName);
  fs.copyFileSync(filePath, refFilePath);
}

async function compare_download_to_ref(download: Download, fileName: string) {
  const filePath = await download.path();
  expect(filePath).toBeTruthy();
  const refFilePath = path.join(referencesDir, fileName);
  const downloadedContent = await fs.promises.readFile(filePath, 'utf8');
  const referenceContent = await fs.promises.readFile(refFilePath, 'utf8');
  expect(downloadedContent).toBe(referenceContent);
}


test.describe('Editor', () => {

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
    const mapFileName = 'map.svg'
    await page.locator('#export_contact-export_filename').fill(mapFileName);
    const downloadPromise = page.waitForEvent('download');
    await page.getByRole('button', { name: 'export' }).click();
    const download = await downloadPromise;
    await compare_download_to_ref(download, mapFileName);
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
    await page.locator('#export_influence-export_filename').click();
    const fileName = 'influences.json'
    await page.locator('#export_influence-export_filename').fill(fileName);
    const downloadPromise = page.waitForEvent('download');
    await page.getByRole('button', { name: 'export' }).click();
    const download = await downloadPromise;
    await compare_download_to_ref(download, fileName);
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
