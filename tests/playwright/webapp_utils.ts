import { expect, type Page, type Download, type Locator } from '@playwright/test';
import fs from 'node:fs';
import path from 'node:path';
import yauzl from 'yauzl';

// TODO: uniformize all functions to camlcase, take care of induced collisions
// Note: waitForTimeout should not be used to await loading of the page, but here is only used to wait for the trigger of the action before having a expect, should be ok

// const url = 'https://tools.kappalanguage.org/try/'
const url = 'http://127.0.0.1:12345/index.html'
const arg_set_model = '?model=https%3A'

const referencesDir = 'tests/playwright/refs/'

function timeout_of_options(options?: { timeout?: number | undefined; visible?: boolean | undefined; } | undefined) {
  var timeout = 5000;
  if (options !== undefined) {
    if (options.timeout !== undefined) {
      timeout = options.timeout;
    }
  }
  return timeout;
}

// TODO: test for contains and not ==

// Used as playwright does not seemed to offer a way to have this logic
async function expect_locator_toHaveInnerHtml(page: Page, locator: Locator, value: any, timeout: number, retry_timeout: number = 500, allow_include: boolean = false) {
  const end_time: number = Date.now() + timeout;
  var is_equal: boolean = false;
  while (Date.now() < end_time && !(is_equal)) {
    const html = (await locator.innerHTML());
    if (allow_include) {
      is_equal = (html.includes(value));
    } else {
      is_equal = (html == value);
    };
    await page.waitForTimeout(retry_timeout);
  }
  expect(is_equal).toBeTruthy();
}

export async function wait_for_project_ready_status(page: Page, options?: { timeout?: number | undefined; visible?: boolean | undefined; } | undefined, check_busy: boolean = false, project_name: string | undefined) {
  const timeout = timeout_of_options(options);
  const locator_first_tab = page.getByRole('list').locator('a').first();

  if (check_busy) {
    // wait for the icon to go to busy state before waiting to be back to ready state
    // [retry_timeout] is low to try not to miss if the change to "refresh" is fast
    // (useful for stories computation that at the moment don't change the icon state rightaway)
    await expect_locator_toHaveInnerHtml(page, locator_first_tab, "\"glyphicon glyphicon-refresh\"", timeout, 50, true);
  }

  // wait for project change if provided
  if (project_name !== undefined) {
    await expect_locator_toHaveInnerHtml(page, locator_first_tab, project_name, timeout, 500, true);
  }

  // wait for icon in project to be checkmark
  await expect_locator_toHaveInnerHtml(page, locator_first_tab, "\"glyphicon glyphicon-ok\"", timeout, 500, true);
}

export async function wait_for_sim_stop(page: Page, options?: { timeout?: number | undefined; visible?: boolean | undefined; } | undefined) {
  await page.waitForTimeout(200);
  // TODO: simplify?
  await expect(page.getByRole('button', { name: 'intervention' })).toBeVisible(options);
  await wait_for_project_ready_status(page);
}

export async function text_of_url(url_protocol_relative: string) {
  const response = await fetch('https://' + url_protocol_relative);
  expect(response.ok).toBeTruthy();
  return await response.text();
}
export async function wait_for_file_load(page: Page, options?: { timeout?: number | undefined; visible?: boolean | undefined; } | undefined) {
  await page.waitForTimeout(200);
  // wait for contact_map display to appear, as it's the last thing to visibly load
  await expect(page.locator('.nodeArcPath').first()).toBeVisible(options);
  await wait_for_project_ready_status(page);
}

export async function input_in_editor_from_str(page: Page, model: string): Promise<void> {
  await page.locator('.CodeMirror-scroll').click();
  await page.locator('#editor-panel').getByRole('textbox').fill(model);
  await wait_for_file_load(page, { timeout: 10000 });
}

export async function input_in_editor_from_url(page: Page, url_protocol_relative: string): Promise<void> {
  const model = await text_of_url(url_protocol_relative);
  await input_in_editor_from_str(page, model);
}

export async function open_app_with_model(page: Page, url_protocol_relative: string, paste_in_editor: boolean = false, timeout: number = 10000) {
  if (paste_in_editor) {
    // download the file and paste it in the editor
    await page.goto(url);
    await wait_for_project_ready_status(page);
    await input_in_editor_from_url(page, url_protocol_relative);
  } else {
    // provide the file as model argument (ok with github domains, but e.g. not di.ens.fr)
    await page.goto(url + arg_set_model + url_protocol_relative);
  }
  // Note: if fails in input_in_editor_from_str, it won't wait for second timeout as expect is not expect.soft
  await wait_for_file_load(page, { timeout: timeout });
}

export function get_error_field(page: Page) {
  // return page.locator('#configuration_error_div');
  return page.locator('#configuration_alert_div');
}

export async function expect_error(page: Page, text: string[]): Promise<void> {
  await expect.soft(get_error_field(page)).toHaveText(text);
}

export async function expect_no_error(page: Page): Promise<void> {
  await expect_error(page, [
    " «  » "
  ]);
}

export async function set_pause_if(page: Page, s: string) {
  await page.getByPlaceholder('[T] >').click();
  await page.getByPlaceholder('[T] >').fill(s);
  await page.getByPlaceholder('[T] >').press('Enter');
}

export async function setSeed(page: Page, seed: number) {
  await page.getByRole('list').locator('a').nth(2).click();
  await page.getByRole('spinbutton', { name: 'Seed' }).click();
  await page.getByRole('spinbutton', { name: 'Seed' }).fill(seed.toString());
  await page.getByRole('button', { name: 'Set', exact: true }).click();
  // wait for modal to close
  await expect(page.locator('div:nth-child(5) > .col-md-2')).toBeHidden();
}

export async function apply_perturbation(page: Page, s: string) {
  await page.getByPlaceholder('Simulation Perturbation').click();
  await page.getByPlaceholder('Simulation Perturbation').fill(s);
  await page.getByRole('button', { name: 'intervention' }).click();
}

export async function write_ref(filePath: string, fileName: string) {
  expect.soft(filePath).toBeTruthy();
  const refFilePath = path.join(referencesDir, fileName);
  fs.copyFileSync(filePath, refFilePath);
}

export async function compare_download_to_ref(download: Download, fileName: string, writeRef: boolean = false) {
  const filePath = await download.path();
  expect.soft(filePath).toBeTruthy();
  if (writeRef || process.env.UPDATE_EXPORTS === 'true') {
    await write_ref(filePath, fileName);
  }
  const refFilePath = path.join(referencesDir, fileName);
  const downloadedContent = await fs.promises.readFile(filePath, 'utf8');
  const referenceContent = await fs.promises.readFile(refFilePath, 'utf8');
  expect.soft(downloadedContent).toBe(referenceContent);
}

export async function write_str_ref(test: string, fileName: string) {
  const filePath = path.join(referencesDir, fileName);
  fs.writeFile(filePath, test,
    err => {
      if (err) {
        console.error(`Error writing to file: ${err}`);
      }
    });
}
export async function get_ref(fileName: string) {
  const refFilePath = path.join(referencesDir, fileName);
  const referenceContent = await fs.promises.readFile(refFilePath, 'utf8');
  return referenceContent;
}

export async function compare_str_to_ref(text: string, fileName: string, writeRef: boolean = false) {
  if (writeRef || process.env.UPDATE_EXPORTS === 'true') {
    await write_str_ref(text, fileName);
  }
  const refFilePath = path.join(referencesDir, fileName);
  const referenceContent = await fs.promises.readFile(refFilePath, 'utf8');
  expect.soft(text).toBe(referenceContent);
}

export async function testExports(page: Page, exportLocatorPrefix: string, fileBaseName: string, extensions: string[], writeRef: boolean = false) {
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

    await compare_download_to_ref(download, fileName, writeRef);
  }
}

export async function compare_zip_files_list_with_ref(download: Download, referenceFiles: string[]): Promise<void> {
  // TODO: debug this
  const downloadPath = await download.path();
  expect(downloadPath).toBeTruthy();

  // const extractDir = fs.mkdtemp(path.join(os.tmpdir(), 'extract-'),
  //   (err, folder) => {
  //     if (err) throw err;
  //   });

  const downloadedFiles: string[] = [];
  yauzl.open(downloadPath, { lazyEntries: true }, function(err, zipfile: yauzl.ZipFile) {
    if (err) throw err;
    zipfile.readEntry();
    zipfile.on("entry", function(entry: yauzl.Entry) {
      downloadedFiles.push(entry.fileName);
    });
  });

  expect(downloadedFiles.sort()).toEqual(referenceFiles.sort());
}

