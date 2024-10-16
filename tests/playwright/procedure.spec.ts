// Kappapp tests
// Note: trace snapshots that should be taken by playwright are not (absent on right of ui `npx playwright test --ui`)

import { test, expect, type Page } from '@playwright/test';

import * as utils from './webapp_utils';

test.describe.configure({ mode: 'parallel' });

test.beforeEach(async ({ page }, testInfo) => {
  page; // TODO: remove when `page` used in function
  testInfo.setTimeout(testInfo.timeout + 30000);
});

const abc_ka = '//raw.githubusercontent.com/Kappa-Dev/KappaTools/master/examples/abc.ka'

const poly_ka = '//raw.githubusercontent.com/Kappa-Dev/KappaTools/master/examples/poly.ka'
const local_views_slide_69_ka = '//www.di.ens.fr/~feret/teaching/2023-2024/MPRI.2.19/activities/local_views/local_views_slide_69.ka'
const counter_2_ka = '//raw.githubusercontent.com/Kappa-Dev/KappaTools/master/tests/integration/compiler/counters_2_levels/counter_2.ka'
const minikai_counters_ka = '//raw.githubusercontent.com/Kappa-Dev/KappaTools/master/examples/large/minikai/minikai_counters.ka'
const causality_slide_10_ka = '//www.di.ens.fr/~feret/teaching/2023-2024/MPRI.2.19/activities/causality/causality_slide_10.ka'

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

    await page.locator('div:nth-child(21) > .CodeMirror-line').click();
    await editor_to_line(21);
    // Make a syntax error
    await editor.press('Backspace');
    // (useless comment to match brackets { {)
    await utils.expect_error(page, [
      " « 1/1 » [abc.ka] invalid internal state or missing '}' ",
      " invalid internal state or missing '}' ",
    ]);
    await editor_cancel();
    await utils.expect_no_error(page);
    await editor_to_line(24);
    await editor.fill('\n%agent: D(a{u p})');

    await utils.expect_error(page, [
      " « 1/1 » [abc.ka] Dead agent D ",
      " Dead agent D ",
    ]);

    await editor_to_line(25);
    await editor.fill("\n%init: 10 D()");
    await utils.expect_no_error(page);

    await editor_to_line(26);
    await editor.fill("\n'd' D(a{p}) -> D(a{u}) @ 1");
    // await page.locator('#panel_preferences_message_nav_inc_id').click();
    await utils.expect_error(page, [
      " « 1/1 » [abc.ka] Dead rule 'd' ",
      " Dead rule 'd' ",
    ]);
    await editor_cancel();
    await editor_cancel();
    await editor_cancel();
    await utils.expect_no_error(page);
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

  function constraint_locator(page: Page, n: number) {
    return (page.locator('#constraints > .panel-scroll > div > .panel-body').nth(n));
  }

  test('constraints_and_polymers_1', async ({ page }) => {
    await utils.open_app_with_model(page, abc_ka);
    await page.getByRole('tab', { name: 'constraints' }).click();
    await expect.soft(constraint_locator(page, 0)).toHaveText(
      `A(c)  =>  [ A(c[.]) v A(c[x1.C]) v A(c[x2.C]) ]
A(x)  =>  [ A(x[.]) v A(x[x.B]) ]
B(x)  =>  [ B(x[.]) v B(x[x.A]) ]
C(x1)  =>  [ C(x1{u}) v C(x1{p}) ]
C(x1)  =>  [ C(x1[.]) v C(x1[c.A]) ]
C(x2)  =>  [ C(x2[.]) v C(x2[c.A]) ]
C(x2)  =>  [ C(x2{u}) v C(x2{p}) ]
`
    );
    await expect.soft(constraint_locator(page, 1)).toHaveText(
      "C()  =>  [ C(x1{p}[.],x2{u}[.]) v C(x1{p}[.],x2{u}[c.A]) v C(x1{p}[.],x2{p}[.]) v C(x1{u}[c.A],x2{u}[.]) v C(x1{u}[.],x2{u}[.]) ]"

    );
    await page.getByRole('tab', { name: 'polymers' }).click();
    await expect.soft(page.getByRole('paragraph')).toHaveText(
      "The size of biomolecular compounds is uniformly bounded."
    );
  });

  test('constraints_and_polymers_2', async ({ page }) => {
    await utils.open_app_with_model(page, poly_ka);
    await page.getByRole('tab', { name: 'polymers' }).click();
    await expect.soft(page.getByRole('paragraph')).toHaveText(
      `The following bonds may form arbitrary long chains of agents:

C(a[1]),A(c[1])
B(c[1]),C(b[1])
A(b[1]),B(a[1])

B(a[1]),A(b[1])
C(b[1]),B(c[1])
A(c[1]),C(a[1])

`);
  });

  test('constraints_and_polymers_3', async ({ page }) => {
    await utils.open_app_with_model(page, local_views_slide_69_ka, true);
    await page.getByRole('tab', { name: 'constraints' }).click();
    await expect.soft(constraint_locator(page, 0)).toHaveText(
      `E(x)  =>  [ E(x[.]) v E(x[x.R]) ]
R(C)  =>  [ R(C[.]) v R(C[CN.R]) ]
R(CN)  =>  [ R(CN[.]) v R(CN[C.R]) ]
R(CR)  =>  [ R(CR[.]) v R(CR[CR.R]) ]
R(x)  =>  [ R(x[.]) v R(x[x.E]) ]
`
    );
    await expect.soft(constraint_locator(page, 1)).toHaveText(
      `R()  =>  [ R(C[.],CN[C.R],CR[CR.R]) v R(C[.],CN[.],CR[CR.R]) v R(C[CN.R],CN[.],CR[CR.R]) v R(C[.],CN[.],CR[.]) ]
R(CR[CR.R])  =>  R(CR[CR.R],x[x.E])
`
    );
    await expect.soft(constraint_locator(page, 2)).toHaveText(
      `R(CR[1]),R(CR[1])  =>  [ R(C[CN.R],CR[1]),R(C[.],CR[1]) v R(C[.],CR[1]),R(C[.],CR[1]) v R(C[.],CR[1]),R(C[CN.R],CR[1]) ]
R(CR[1]),R(CR[1])  =>  [ R(CN[C.R],CR[1]),R(CN[.],CR[1]) v R(CN[.],CR[1]),R(CN[.],CR[1]) v R(CN[.],CR[1]),R(CN[C.R],CR[1]) ]
`
    );

    // TODO delete await page.locator('div:nth-child(3) > .CodeMirror-line > span > span:nth-child(7)').scrollIntoViewIfNeeded();
    await expect.soft(constraint_locator(page, 3)).toHaveText(
      `R(C[CN.R],CR[CR.R])  =>  R(C[2],CR[1]),R(CN[2],CR[1])
R(CN[C.R],CR[CR.R])  =>  R(CN[2],CR[1]),R(C[2],CR[1])
`
    );
    await expect.soft(constraint_locator(page, 4)).toHaveText(
      ""
    );
    await utils.expect_error(page, [
      " « 1/1 » [model.ka] Dead rule R(CR[1] C[2]), R(CN[2]), R(CR[1]) -> R(CR[1] C[2]), R(CN[2]), R(CR[1]) ",
      " Dead rule R(CR[1] C[2]), R(CN[2]), R(CR[1]) -> R(CR[1] C[2]), R(CN[2]), R(CR[1]) "
    ]);
  });

  test('constraints_and_polymers_4', async ({ page }) => {
    await utils.open_app_with_model(page, counter_2_ka);
    await page.getByRole('tab', { name: 'constraints' }).click();
    await expect.soft(constraint_locator(page, 4)).toHaveText(
      `A()  =>  A(c{[0 .. 2]})
`
    );
  });

  test('contact_map_accuracy', async ({ page }) => {
    await utils.open_app_with_model(page, minikai_counters_ka, false, 50000);
    const contact_map = page.locator('#map-container');
    await expect.soft(contact_map).toHaveScreenshot();
    await page.locator('#contact_map-accuracy').selectOption('high');
    await expect.soft(contact_map).toHaveScreenshot();
  });

})

test.describe('Simulation tools', () => {

  test('Simulation, plot', async ({ page }) => {
    await utils.open_app_with_model(page, abc_ka);
    await utils.setSeed(page, 1);
    // Run simulation to 30, then 100, then test plot options
    await utils.set_pause_if(page, '[T] > 30');
    await page.getByRole('button', { name: 'start' }).click();
    await utils.wait_for_sim_stop(page, { timeout: 20000 });
    await page.getByRole('tab', { name: 'plot New' }).click();
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await utils.set_pause_if(page, '[T] > 100');
    await page.getByRole('button', { name: 'continue' }).click();
    await utils.wait_for_sim_stop(page, { timeout: 20000 });
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
    await utils.wait_for_sim_stop(page, { timeout: 20000 });
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
    await expect.soft(utils.get_error_field(page)).toHaveText("TODO");
    await utils.apply_perturbation(page, '$SNAPSHOT');
    await utils.set_pause_if(page, '[T] > 60');
    await page.getByRole('button', { name: 'continue' }).click();
    await utils.apply_perturbation(page, '$SNAPSHOT "T60"');

    // check log page
    await page.getByRole('tab', { name: 'log New' }).click();
    await expect.soft(page.locator('#log div')).toHaveText(
      `Building initial simulation conditions...	 -variable declarations	 -rules	 -interventions	 -observables	 -update_domain construction	 21 (sub)observables 37 navigation steps	 -initial conditionsUser-set seed used for simulation: 1%mod: [E] = 14599 do $SNAPSHOT ;%mod: [E] = 28929 do $SNAPSHOT \"T60\";
`
    );

    // go to tab snapshots and test display
    await page.getByRole('tab', { name: 'snapshot' }).click();
    const snapshot_display_loc = page.locator('.navcontent-view > .panel-scroll').first();
    //const snapshot_map_display_loc = page.locator('#snapshot-map-display #map-container').first();
    //const snapshot_map_display2_loc = page.locator('#snapshot-map-display div').first();

    await expect.soft(snapshot_display_loc).toHaveText(
      `// Snapshot [Event: 14599]
%def: "T0" "30.003207872859807"

%init: 60 /*3 agents*/ A(x[1] c[2]), B(x[1]), C(x1{p}[.] x2{u}[2])
%init: 111 /*3 agents*/ B(x[1]), A(x[1] c[2]), C(x1{u}[2] x2{u}[.])
%init: 20 /*2 agents*/ A(x[.] c[1]), C(x1{u}[1] x2{u}[.])
%init: 112 /*2 agents*/ A(x[.] c[1]), C(x1{p}[.] x2{u}[1])
%init: 1852 /*1 agents*/ C(x1{p}[.] x2{p}[.])
%init: 2107 /*1 agents*/ C(x1{p}[.] x2{u}[.])
%init: 5738 /*1 agents*/ C(x1{u}[.] x2{u}[.])
%init: 223 /*2 agents*/ A(x[1] c[.]), B(x[1])
%init: 606 /*1 agents*/ B(x[.])
%init: 474 /*1 agents*/ A(x[.] c[.])


`);
    await page.locator('#snapshot-select-id').selectOption('1');
    await expect.soft(snapshot_display_loc).toHaveText("TODO");
    await page.locator('#snapshot-select-id').selectOption('0');
    await expect.soft(snapshot_display_loc).toHaveText("TODO");
    await page.locator('#format_select_id').selectOption('Graph');
    await expect.soft(snapshot_display_loc).toHaveScreenshot();
    await page.locator('.navcontent-view > div:nth-child(3)').click();
    await expect.soft(snapshot_display_loc).toHaveScreenshot();
    await page.getByRole('button', { name: 'Back to root' }).click();
    await expect.soft(snapshot_display_loc).toHaveScreenshot();
    await page.getByRole('radio', { name: 'Count' }).check();
    await expect.soft(snapshot_display_loc).toHaveScreenshot();
    await page.getByRole('radio', { name: 'Size' }).check();
    await expect.soft(snapshot_display_loc).toHaveScreenshot();
    // TODO: does something?
    // await page.getByRole('button', { name: 'Reset Zoom' }).click();

    // TODO: clicks in graph, link with contact map
    await page.locator('[id="root\\.mixture1"] rect').nth(1).click();
    await expect.soft(snapshot_display_loc).toHaveScreenshot();
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
    // await page.getByRole('tab', { name: 'log New' }).click();
    await page.locator('#navtabs').nth(1).click();
    await expect.soft(page.locator('#log div')).toHaveText(
      ` + Building initial simulation conditions...	 -variable declarations	 -rules	 -interventions	 -observables	 -update_domain construction	 21 (sub)observables 37 navigation steps	 -initial conditionsUser-set seed used for simulation: 1%mod: [E] = 14599 do $PRINTF ("time: ".[T]) > "time.txt";%mod: [E] = 14599 do $PRINTF (AB) > "ab.txt";%mod: [E] = 28929 do $PRINTF ("time: ".[T]) > "time.txt";%mod: [E] = 28929 do $PRINTF (AB) > "ab.txt";`
    );
    await page.locator('#navtabs').nth(5).click();

    await page.locator('#output-select-id').selectOption('time.txt');
    await expect.soft(page.getByRole('paragraph')).toHaveText("TODO");
    await page.locator('.list-group-item').click();
    await page.locator('#output-select-id').selectOption('ab.txt');
    await expect.soft(page.getByRole('paragraph')).toHaveText("TODO");

    const downloadPromise = page.waitForEvent('download');
    await page.getByRole('button', { name: 'All outputs' }).click();
    const download = await downloadPromise;

    utils.compare_zip_files_list_with_ref(download, []);
  });

});

test.describe('stories', () => {

  test('stories', async ({ page }) => {
    await utils.open_app_with_model(page, causality_slide_10_ka, true);
    await utils.setSeed(page, 1);

    // Enable trace
    await page.getByRole('list').locator('a').nth(2).click();
    await page.locator('.col-md-offset-2 > label').first().click();
    await page.getByRole('button', { name: 'Set', exact: true }).click();
    // gather trace
    await page.getByRole('button', { name: 'start' }).click();
    await utils.wait_for_sim_stop(page, { timeout: 20000 });

    async function expect_texts_story_logs(page: Page, text_info_log: string, text_computation_log: string) {
      await page.getByRole('tab', { name: 'story_info_log' }).click();
      await expect.soft(page.locator('#story_info_log')).toHaveText(text_info_log);
      await page.getByRole('tab', { name: 'stories_computation_log' }).click();
      await expect.soft(page.locator('#story_computation_log')).toHaveText(text_computation_log);
      await page.getByRole('tab', { name: 'story_info_log' }).click();
    }

    async function computeStoriesAndWait(page: Page) {
      await page.getByRole('button', { name: 'Launch' }).click();
      // wait for icon in project to be checkmark
      await utils.wait_for_project_ready_status(page, { timeout: 30000 });
    }

    async function computeStoriesAndTest(page: Page, text_info_log: string, text_computation_log: string) {
      await computeStoriesAndWait(page);
      await expect_texts_story_logs(page, text_info_log, text_computation_log);
      await expect.soft(page.getByRole('img')).toHaveScreenshot();
    }

    // compare stories
    await page.getByRole('tab', { name: 'stories' }).click();

    await page.getByRole('checkbox', { name: 'Weakly' }).uncheck();
    await computeStoriesAndTest(page, "TODO", "TODO");

    await page.getByRole('checkbox', { name: 'Strongly' }).check();
    await page.getByRole('checkbox', { name: 'Strongly' }).uncheck();
    await computeStoriesAndTest(page, "TODO", "TODO");
    await page.getByRole('checkbox', { name: 'Causal' }).check();
    await page.getByRole('checkbox', { name: 'Causal' }).uncheck();
    await computeStoriesAndTest(page, "TODO", "TODO");

    await page.getByRole('combobox').selectOption('0');
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await expect_texts_story_logs(page, "TODO", "TODO");
    await page.getByRole('combobox').selectOption('2');
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await expect_texts_story_logs(page, "TODO", "TODO");

    await page.getByRole('checkbox', { name: 'Weakly' }).check();
    await page.getByRole('checkbox', { name: 'Strongly' }).check();
    await computeStoriesAndTest(page, "TODO", "TODO");

    const downloadPromise = page.waitForEvent('download');
    await page.getByRole('button', { name: 'get trace' }).click();
    const download = await downloadPromise;
    await utils.compare_download_to_ref(download, "stories_trace");
  });

});

test.describe('projects_and_files', () => {
  test('project', async ({ page }) => {
    await utils.open_app_with_model(page, causality_slide_10_ka, true);
    await utils.setSeed(page, 1);

    // open new project
    await page.getByRole('list').locator('a').nth(1).click();
    await page.getByPlaceholder('project new').click();
    await page.getByPlaceholder('project new').fill('test');
    await page.getByPlaceholder('project new').press('Enter');
    // add new file "abc.ka" and fill it
    await page.getByRole('button', { name: 'File' }).click();
    await page.locator('#menu-editor-file-new-li').click();
    await page.getByRole('textbox', { name: 'file name' }).click();
    await page.getByRole('textbox', { name: 'file name' }).fill('abc.ka');
    await page.getByRole('button', { name: 'Create File' }).click();
    await page.locator('.CodeMirror-scroll').click();
    await utils.input_in_editor_from_url(page, abc_ka);

    // switch and check contact_map change
    const contact_map = page.locator('#map-container');
    await expect.soft(contact_map).toHaveScreenshot();
    await page.getByRole('list').locator('a').nth(1).click();
    await expect.soft(contact_map).toHaveScreenshot();
    await page.getByRole('list').locator('a').first().click();
    await expect.soft(contact_map).toHaveScreenshot();

    // TODO: Could also check simulation results
  });

  test('files', async ({ page }) => {
    await utils.open_app_with_model(page, abc_ka, false);
    await utils.setSeed(page, 1);

    // download file
    await page.getByRole('button', { name: 'File' }).click();
    const downloadPromise = page.waitForEvent('download');
    await page.locator('#menu-editor-file-export-li').click();
    const download = await downloadPromise;
    await utils.compare_download_to_ref(download, "abc_download.ka");
    const downloaded_path = await download.path();

    // close it, and reopen it
    await page.getByRole('button', { name: 'File' }).click();
    await page.locator('#menu-editor-file-close-li').click();
    await page.getByRole('button', { name: 'File' }).click();
    await page.locator('#menu-editor-file-open-li').click();
    await page.locator('body').setInputFiles(downloaded_path);

    // write other file and check contact map
    await page.getByRole('button', { name: 'File' }).click();
    await page.locator('#menu-editor-file-new-li').click();
    await page.getByRole('textbox', { name: 'file name' }).click();
    await page.getByRole('textbox', { name: 'file name' }).fill('test.ka');
    await page.getByRole('textbox', { name: 'file name' }).press('Enter');
    await page.locator('.CodeMirror-scroll').click();
    await utils.input_in_editor_from_str(page,
      `%agent: K(x)
%agent: S(a b{u p} c{u p})
%init: 1000 K()
%init: 1000 S()
'K.S' K(x[.]),S(a[.]) -> K(x[1]),S(a[1]) @ 1
'b+' K(x[1]),S(a[1] b{u}) -> K(x[1]),S(a[1] b{p}) @ 1
'K..S' S(a[_]) -> S(a[.]) @ 10
 'c+' K(x[1]),S(a[1] c{u}) -> K(x[1]),S(a[1] c{p}) @ 1
%obs: 'S++' |S(b{p} c{p})|
%mod: [true] do $TRACK 'S++' [true] ;
`
    );
    const contact_map = page.locator('#map-container');
    await expect.soft(contact_map).toHaveScreenshot();

    // simulate and test screenshot
    await utils.set_pause_if(page, '[T] > 30');
    await page.getByRole('button', { name: 'start' }).click();
    await utils.wait_for_sim_stop(page, { timeout: 20000 });
    await page.getByRole('tab', { name: 'plot New' }).click();
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
  });

});
