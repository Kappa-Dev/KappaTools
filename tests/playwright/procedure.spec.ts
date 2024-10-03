// Kappapp tests
// Note: trace snapshots that should be taken by playwright are not (absent on right of ui `npx playwright test --ui`)

import { test, expect, type Page } from '@playwright/test';

import * as utils from './webapp_utils';

test.describe.configure({ mode: 'parallel' });

test.beforeEach(async ({ page }, testInfo) => {
  page; // TODO: remove
  testInfo.setTimeout(testInfo.timeout + 30000);
});

const abc_ka = '//raw.githubusercontent.com/Kappa-Dev/KappaTools/master/examples/abc.ka'

const poly_ka = '//raw.githubusercontent.com/Kappa-Dev/KappaTools/master/examples/poly.ka'
const local_views_slide_69_ka = '//www.di.ens.fr/~feret/teaching/2023-2024/MPRI.2.19/activities/local_views/local_views_slide_69.ka'
const counter_2_ka = '//raw.githubusercontent.com/Kappa-Dev/KappaTools/master/tests/integration/compiler/counters_2_levels/counter_2.ka'
const minikai_counters_ka = '//raw.githubusercontent.com/Kappa-Dev/KappaTools/master/examples/large/minikai/minikai_counters.ka'

test.describe('Editor tab', () => {

  test('editor', async ({ page }) => {
    await utils.open_app_with_model(page, abc_ka);

    const editor = page.locator('#editor-panel').getByRole('textbox');
    async function editor_to_line(n: number): Promise<void> {
      await page.locator('div:nth-child(' + n + ') > .CodeMirror-line').click();
    }
    async function editor_cancel(): Promise<void> {
      await editor.press('ControlOrMeta+z');
    }
    const error_field = utils.get_error_field(page);
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
    await utils.open_app_with_model(page, abc_ka);
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
    await utils.testExports(page, '#export_contact-export', 'map', ['svg', 'png', 'json']);
  });

  test('influences', async ({ page }) => {
    const opts_screen = { threshold: 0.4 }
    await utils.open_app_with_model(page, abc_ka);
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
    await utils.testExports(page, '#export_influence-export', 'influences', ['json']);
  });

  test('constraints_and_polymers_1', async ({ page }) => {
    await utils.open_app_with_model(page, abc_ka);
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

  test('constraints_and_polymers_2', async ({ page }) => {
    await utils.open_app_with_model(page, poly_ka);
    await page.getByRole('tab', { name: 'constraints' }).click();
    await page.getByRole('tab', { name: 'polymers' }).click();
    await expect.soft(page.getByRole('paragraph')).toHaveText(
      "The size of biomolecular compounds is uniformly bounded."
    );
  });

  test('constraints_and_polymers_3', async ({ page }) => {
    await utils.open_app_with_model(page, local_views_slide_69_ka);
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
    await expect.soft(page.locator('div:nth-child(3) > .panel-body')).toHaveText(
      "C()  =>  [ C(x1{p}[.],x2{u}[.]) v C(x1{p}[.],x2{u}[c.A]) v C(x1{p}[.],x2{p}[.]) v C(x1{u}[c.A],x2{u}[.]) v C(x1{u}[.],x2{u}[.]) ]"

    );
    await expect.soft(page.locator('div:nth-child(4) > .panel-body')).toHaveText(
      "C()  =>  [ C(x1{p}[.],x2{u}[.]) v C(x1{p}[.],x2{u}[c.A]) v C(x1{p}[.],x2{p}[.]) v C(x1{u}[c.A],x2{u}[.]) v C(x1{u}[.],x2{u}[.]) ]"

    );
    await expect.soft(page.locator('div:nth-child(5) > .panel-body')).toHaveText(
      "C()  =>  [ C(x1{p}[.],x2{u}[.]) v C(x1{p}[.],x2{u}[c.A]) v C(x1{p}[.],x2{p}[.]) v C(x1{u}[c.A],x2{u}[.]) v C(x1{u}[.],x2{u}[.]) ]"

    );
  });

  test('constraints_and_polymers_4', async ({ page }) => {
    await utils.open_app_with_model(page, counter_2_ka);
    await page.getByRole('tab', { name: 'constraints' }).click();
    await expect.soft(page.locator('div:nth-child(5) > .panel-body')).toHaveText(
      "C()  =>  [ C(x1{p}[.],x2{u}[.]) v C(x1{p}[.],x2{u}[c.A]) v C(x1{p}[.],x2{p}[.]) v C(x1{u}[c.A],x2{u}[.]) v C(x1{u}[.],x2{u}[.]) ]"

    );
  });

  test('contact_map_accuracy', async ({ page }) => {
    await utils.open_app_with_model(page, minikai_counters_ka);
    const contact_map = page.locator('#map-container');
    await expect.soft(contact_map).toHaveScreenshot();
    await page.locator('#contact_map-accuracy').selectOption('high');
    await expect.soft(contact_map).toHaveScreenshot();
  });

})

test.describe('Simulation tools', () => {

  test('Simulation, plot', async ({ page }) => {
    async function wait_for_sim_stop(page: Page) {
      await page.waitForTimeout(200);
      await expect(page.getByRole('button', { name: 'intervention' })).toBeVisible({ timeout: 20000 });
    }
    await utils.open_app_with_model(page, abc_ka);
    await utils.setSeed(page, 1);
    // Run simulation to 30, then 100, then test plot options
    await utils.set_pause_if(page, '[T] > 30');
    await page.getByRole('button', { name: 'start' }).click();
    await wait_for_sim_stop(page);
    await page.getByRole('tab', { name: 'plot New' }).click();
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await utils.set_pause_if(page, '[T] > 100');
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

    await utils.testExports(page, '#export_plot-export', 'plot', ['svg', 'csv', 'json', 'tsv', 'png']);

    // Test larger plots, slider
    await utils.set_pause_if(page, '[T] > 2000');
    await page.getByRole('button', { name: 'continue' }).click();
    await wait_for_sim_stop(page);
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await page.getByRole('button', { name: 'continue' }).click();
    await page.getByPlaceholder('offset').fill('0');
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await page.getByPlaceholder('offset').fill('83');
    await expect.soft(page.getByRole('img')).toHaveScreenshot();

    await utils.set_pause_if(page, '');
    await page.getByRole('button', { name: 'continue' }).click();
    await expect.soft(page.locator('.row > .col-xs-3').first()).toHaveText("events");
    await expect.soft(page.locator('.col-md-5 > div:nth-child(2) > .col-xs-3')).toHaveText("time");
    await page.getByRole('button', { name: 'pause' }).click();
  });

  test('DIN', async ({ page }) => {
    await utils.open_app_with_model(page, abc_ka);
    await utils.setSeed(page, 1);

    async function expectScreenShotDINTable() {
      await expect.soft(page.getByRole('cell', { name: 'affects' })).toBeVisible();
      await expect.soft(page.locator('#DIN div').first()).toHaveScreenshot({ mask: [page.locator('#export_din-export_form')] });
    }

    // Run simulation to 30, then 100, then test plot options
    await utils.set_pause_if(page, '[T] > 30');
    await page.getByRole('button', { name: 'start' }).click();
    await page.getByRole('tab', { name: 'DIN' }).click();
    await expectScreenShotDINTable();

    await utils.testExports(page, '#export_din-export', 'flux', ['json', 'dot', 'html']);

    await utils.set_pause_if(page, '[T] > 60');
    await page.getByRole('button', { name: 'continue' }).click();
    await expectScreenShotDINTable();
    await page.getByRole('combobox').first().selectOption('flux.json');
    await expectScreenShotDINTable();

    await utils.testExports(page, '#export_din-export', 'flux_json', ['json', 'dot', 'html']);

    await page.getByRole('combobox').first().selectOption('flux.html');
    await expectScreenShotDINTable();
  });

  test('snapshots', async ({ page }) => {
    await utils.open_app_with_model(page, abc_ka);
    await utils.setSeed(page, 1);

    // Generate two snapshots
    await utils.set_pause_if(page, '[T] > 30');
    await page.getByRole('button', { name: 'start' }).click();
    await utils.apply_perturbation(page, '');
    await expect(utils.get_error_field(page)).toHaveText("TODO");
    await utils.apply_perturbation(page, '$SNAPSHOT');
    await utils.set_pause_if(page, '[T] > 60');
    await page.getByRole('button', { name: 'continue' }).click();
    await utils.apply_perturbation(page, '$SNAPSHOT "T60"');

    // check log page
    await page.getByRole('tab', { name: 'log New' }).click();
    await expect(page.locator('#log div')).toHaveText("TODO");

    // go to tab snapshots and test display
    await page.getByRole('tab', { name: 'snapshot' }).click();
    const snapshot_display_loc = page.locator('.navcontent-view > .panel-scroll').first();
    //const snapshot_map_display_loc = page.locator('#snapshot-map-display #map-container').first();
    //const snapshot_map_display2_loc = page.locator('#snapshot-map-display div').first();

    await expect(snapshot_display_loc).toHaveText("TODO");
    await page.locator('#snapshot-select-id').selectOption('1');
    await expect(snapshot_display_loc).toHaveText("TODO");
    await page.locator('#snapshot-select-id').selectOption('0');
    await expect(snapshot_display_loc).toHaveText("TODO");
    await page.locator('#format_select_id').selectOption('Graph');
    await expect(snapshot_display_loc).toHaveScreenshot();
    await page.locator('.navcontent-view > div:nth-child(3)').click();
    await expect(snapshot_display_loc).toHaveScreenshot();
    await page.getByRole('button', { name: 'Back to root' }).click();
    await expect(snapshot_display_loc).toHaveScreenshot();
    await page.getByRole('radio', { name: 'Count' }).check();
    await expect(snapshot_display_loc).toHaveScreenshot();
    await page.getByRole('radio', { name: 'Size' }).check();
    await expect(snapshot_display_loc).toHaveScreenshot();
    // TODO: does something?
    // await page.getByRole('button', { name: 'Reset Zoom' }).click();

    // TODO: clicks in graph, link with contact map
    await page.locator('[id="root\\.mixture1"] rect').nth(1).click();
    await expect(snapshot_display_loc).toHaveScreenshot();
    // await page.locator('#force-container circle').nth(1).click();
    // await page.locator('#force-container circle').first().click();

    // Test exports
    await page.locator('#format_select_id').selectOption('Kappa');
    await utils.testExports(page, "#export_snapshot_kappa", "snapshot_kappa", ["json", "kappa", "dot"]);
    await page.locator('#format_select_id').selectOption('Graph');
    await utils.testExports(page, "#export_snapshot_graph", "snapshot_graph", ["json", "kappa", "dot", "svg", "png"]);
  });

  test('outputs', async ({ page }) => {
    await utils.open_app_with_model(page, abc_ka);
    await utils.setSeed(page, 1);

    // Generate two snapshots
    await utils.set_pause_if(page, '[T] > 30');
    await page.getByRole('button', { name: 'start' }).click();
    await utils.apply_perturbation(page, '');
    await expect(utils.get_error_field(page)).toHaveText("TODO");
    await utils.apply_perturbation(page, '$SNAPSHOT');
    await utils.set_pause_if(page, '[T] > 60');
    await page.getByRole('button', { name: 'continue' }).click();
    await utils.apply_perturbation(page, '$SNAPSHOT "T60"');

    // check log page
    await page.getByRole('tab', { name: 'log New' }).click();
    await expect(page.locator('#log div')).toHaveText("TODO");

    // go to tab snapshots and test display
    await page.getByRole('tab', { name: 'snapshot' }).click();
    const snapshot_display_loc = page.locator('.navcontent-view > .panel-scroll').first();
    //const snapshot_map_display_loc = page.locator('#snapshot-map-display #map-container').first();
    //const snapshot_map_display2_loc = page.locator('#snapshot-map-display div').first();

    await expect(snapshot_display_loc).toHaveText("TODO");
    await page.locator('#snapshot-select-id').selectOption('1');
    await expect(snapshot_display_loc).toHaveText("TODO");
    await page.locator('#snapshot-select-id').selectOption('0');
    await expect(snapshot_display_loc).toHaveText("TODO");
    await page.locator('#format_select_id').selectOption('Graph');
    await expect(snapshot_display_loc).toHaveScreenshot();
    await page.locator('.navcontent-view > div:nth-child(3)').click();
    await expect(snapshot_display_loc).toHaveScreenshot();
    await page.getByRole('button', { name: 'Back to root' }).click();
    await expect(snapshot_display_loc).toHaveScreenshot();
    await page.getByRole('radio', { name: 'Count' }).check();
    await expect(snapshot_display_loc).toHaveScreenshot();
    await page.getByRole('radio', { name: 'Size' }).check();
    await expect(snapshot_display_loc).toHaveScreenshot();
    // TODO: does something?
    // await page.getByRole('button', { name: 'Reset Zoom' }).click();

    // TODO: clicks in graph, link with contact map
    await page.locator('[id="root\\.mixture1"] rect').nth(1).click();
    await expect(snapshot_display_loc).toHaveScreenshot();
    // await page.locator('#force-container circle').nth(1).click();
    // await page.locator('#force-container circle').first().click();

    // Test exports
    await page.locator('#format_select_id').selectOption('Kappa');
    await utils.testExports(page, "#export_snapshot_kappa", "snapshot_kappa", ["json", "kappa", "dot"]);
    await page.locator('#format_select_id').selectOption('Graph');
    await utils.testExports(page, "#export_snapshot_graph", "snapshot_graph", ["json", "kappa", "dot", "svg", "png"]);
  });

  test('outputs', async ({ page }) => {
    await utils.open_app_with_model(page, abc_ka);
    await utils.setSeed(page, 1);

    // Generate two snapshots
    await utils.set_pause_if(page, '[T] > 30');
    await page.getByRole('button', { name: 'start' }).click();
    await utils.apply_perturbation(page, '$PRINT "time: ".[T] > "time.txt"');
    await utils.apply_perturbation(page, '$PRINT \'AB\' > "ab.txt"');
    await utils.set_pause_if(page, '[T] > 60');
    await page.getByRole('button', { name: 'continue' }).click();
    await utils.apply_perturbation(page, '$PRINT "time: ".[T] > "time.txt"');
    await utils.apply_perturbation(page, '$PRINT \'AB\' > "ab.txt"');

    // check log page
    await page.getByRole('tab', { name: 'log New' }).click();
    await expect(page.locator('#log div')).toHaveText("TODO");

    await page.locator('#output-select-id').selectOption('time.txt');
    await expect(page.getByRole('paragraph')).toHaveText("TODO");
    await page.locator('.list-group-item').click();
    await page.locator('#output-select-id').selectOption('ab.txt');
    await expect(page.getByRole('paragraph')).toHaveText("TODO");

    const downloadPromise = page.waitForEvent('download');
    await page.getByRole('button', { name: 'All outputs' }).click();
    const download = await downloadPromise;

    utils.compare_zip_files_list_with_ref(download, []);
  });

});

test.describe('stories', () => {
});

test.describe('projects_and_files', () => {
});
