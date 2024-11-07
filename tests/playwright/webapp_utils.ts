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

// Interface for screenshots options to be able to add them when needed.
// Extend when needed 
// The original type seems not to be accessible from here? 
// ( https://github.com/microsoft/playwright/blob/910ecdf5566e29f2f7c021e75a46fba35a7d0cf1/packages/playwright/src/matchers/toMatchSnapshot.ts#L47-L63 )
// TODO: add original type instead if possible
export interface ScreenshotOptions {
  maxDiffPixels?: number,
  mask?: Array<Locator>;
}


function timeout_of_options(options: { timeout?: number | undefined; visible?: boolean | undefined; } | undefined, default_timeout: number = 5000) {
  var timeout = default_timeout;
  if (options !== undefined) {
    if (options.timeout !== undefined) {
      timeout = options.timeout;
    }
  }
  return timeout;
}

// TODO: test for contains and not ==

// Used as playwright does not seemed to offer a way to have this logic
export async function expect_locator_toHaveInnerHtml(page: Page, locator: Locator, value: any, timeout: number, retry_timeout: number = 500, allow_include: boolean = false) {
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

export async function wait_for_project_ready_status(page: Page, options?: { timeout?: number | undefined; visible?: boolean | undefined; } | undefined, check_busy_first: boolean = false, project_name: string | undefined = undefined) {
  const timeout = timeout_of_options(options, 15000);
  const locator_first_tab = page.getByRole('list').locator('a').first();

  if (check_busy_first) {
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

export async function open_app_with_model_from_text(page: Page, model_text: string, run_in_electron: boolean, timeout: number = 10000) {
  if (!run_in_electron) {
    // load the app if not in electron
    // if in electron, the page is already loaded, and we just need to enter the file in the editor
    await page.goto(url);
  }
  await wait_for_project_ready_status(page);
  await input_in_editor_from_str(page, model_text);

  // Note: if fails in input_in_editor_from_str, it won't wait for second timeout as expect is not expect.soft
  await wait_for_file_load(page, { timeout: timeout });
}

// TODO: paste_in_editor is now always true, as default is true
export async function open_app_with_model(page: Page, url_protocol_relative: string, run_in_electron: boolean, paste_in_editor: boolean = true, timeout: number = 10000) {
  if (paste_in_editor || run_in_electron) {
    // download the file and paste it in the editor
    if (!run_in_electron) {
      // load the app if not in electron
      // if in electron, the page is already loaded, and we just need to enter the file in the editor
      await page.goto(url);
    }
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
  // Note: alternative emplacement, showing different info, in case it's relevant
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

// Write in `referencesDir` the file named `fileName` as a copy of `filePath`
async function write_ref(filePath: string, fileName: string) {
  expect.soft(filePath).toBeTruthy();
  const refFilePath = path.join(referencesDir, fileName);
  fs.copyFileSync(filePath, refFilePath);
}

function write_str_ref(test: string, fileName: string) {
  const filePath = path.join(referencesDir, fileName);
  fs.writeFile(filePath, test,
    err => {
      if (err) {
        console.error(`Error writing to file: ${err}`);
      }
    });
}

function regex_of_str(s: string, option: string = "g"): RegExp {
  try {
    const r = new RegExp(s, option);
    return r;
  } catch (error) {
    // Next line: assert(false) through playwright
    expect(false).toBe(true);
  }
  return /should_not_happen/;
}

function escape_string_for_regex(s: string): string {
  return s.replace(/[/\-\\^$*+?.()|[\]{}]/g, '\\$&');
}

function add_to_filename_before_ext(name: string, to_add: string) {
  var name_splitted = name.split('.');
  const extension = name_splitted.pop();
  return (name_splitted.join('.') + to_add + '.' + extension);
}

function make_filename_with_browserName(name: string, browserName: string | undefined) {
  if (browserName == undefined) {
    return name;
  } else {
    return add_to_filename_before_ext(name, '_' + browserName);
  }
}

// Returns a regex matching [s] with matches of [pattern] replaced by the [pattern]
// Used to avoid mismatching colors in exports, as it's different according to how playwright is run…
function regex_matching_str_with_pattern_replaced(s: string, pattern: string): RegExp {
  const s_escaped = escape_string_for_regex(s);
  const regex_pattern = regex_of_str(pattern, "g");
  // console.debug(s_escaped);
  // console.debug(regex_pattern);
  return regex_of_str(s_escaped.replace(regex_pattern, pattern), "");
}

export async function compare_download_to_ref(download: Download, name: string, pattern_ignore: string = "", browserName: string | undefined = undefined) {
  const fileName = make_filename_with_browserName(name, browserName);
  const downloadPath = await download.path();
  expect.soft(downloadPath).toBeTruthy();
  if (process.env.UPDATE_EXPORTS === 'true') {
    console.info(`Writing ref file: ${fileName}`);
    await write_ref(downloadPath, fileName);
  }
  const refFilePath = path.join(referencesDir, fileName);
  const downloadedContent = await fs.promises.readFile(downloadPath, 'utf8');
  const referenceContent = await fs.promises.readFile(refFilePath, 'utf8');

  if (pattern_ignore == "") {
    expect.soft(downloadedContent).toBe(referenceContent);
  } else {
    const regex_ref_test: RegExp = regex_matching_str_with_pattern_replaced(referenceContent, pattern_ignore);
    expect.soft(downloadedContent).toMatch(regex_ref_test);
  }

}

export async function get_ref(fileName: string) {
  const refFilePath = path.join(referencesDir, fileName);
  const referenceContent = await fs.promises.readFile(refFilePath, 'utf8');
  return referenceContent;
}

export async function compare_str_to_ref(text: string, fileName: string) {
  if (process.env.UPDATE_EXPORTS === 'true') {
    write_str_ref(text, fileName);
  }
  const refFilePath = path.join(referencesDir, fileName);
  const referenceContent = await fs.promises.readFile(refFilePath, 'utf8');
  expect.soft(text).toBe(referenceContent);
}

export async function testExports(page: Page, exportLocatorPrefix: string, fileBaseName: string, extensions: string[], pattern_ignores: string[] = [], browserName: string | undefined = undefined) {
  const exportFilenameLoc = page.locator(exportLocatorPrefix + "_filename");
  const exportSelectLoc = page.locator(exportLocatorPrefix + "_select");

  const pattern_ignores_present = pattern_ignores.length == extensions.length;

  for (let i = 0; i < extensions.length; i++) {
    const extension = extensions[i];
    const fileName = `${fileBaseName}.${extension}`;
    await exportSelectLoc.selectOption(extension);
    await exportFilenameLoc.click();
    await exportFilenameLoc.fill(fileName);

    const downloadPromise = page.waitForEvent('download');
    await page.getByRole('button', { name: 'export' }).click();
    const download = await downloadPromise;

    const pattern_ignore = pattern_ignores_present ? pattern_ignores[i] : "";
    await compare_download_to_ref(download, fileName, pattern_ignore, browserName);
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

