// Kappapp tests
// Note: trace snapshots that should be taken by playwright are not (absent on right of ui `npx playwright test --ui`)

// TODO: test with embedded Kasim? deprecate it?

import { test, expect, type Page } from '@playwright/test';

import * as utils from './webapp_utils';

test.describe.configure({ mode: 'parallel' });

test.beforeEach(async ({ page }, testInfo) => {
  testInfo.setTimeout(60000);
  // Wait on fonts, might be useful on CI
  await page.waitForFunction(() => document.fonts.ready);
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
      " « 1/1 » [abc.ka] invalid internal state or missing '}' "
    ]);
    await editor_cancel();
    await utils.expect_no_error(page);
    await editor_to_line(24);
    await editor.fill('\n%agent: D(a{u p})');

    await utils.expect_error(page, [
      " « 1/1 » [abc.ka] Dead agent D "
    ]);

    await editor_to_line(25);
    await editor.fill("\n%init: 10 D()");
    await utils.expect_no_error(page);

    await editor_to_line(26);
    await editor.fill("\n'd' D(a{p}) -> D(a{u}) @ 1");
    // await page.locator('#panel_preferences_message_nav_inc_id').click();
    await utils.expect_error(page, [
      " « 1/1 » [abc.ka] Dead rule 'd' "
    ]);
    await editor_cancel();
    await editor_cancel();
    await editor_cancel();
    await utils.expect_no_error(page);
  });

  test('contact_map', async ({ page, browserName }) => {
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

    await utils.testExports(page, '#export_contact-export', 'map', ['svg', 'json'], undefined, browserName);
    if (browserName != "chromium") {
      await utils.testExports(page, '#export_contact-export', 'map', ['png'], undefined, browserName);
      // TODO: pngs doesn't match on CI's chromium. check if we can test them in some way
    }

  });

  test('influences', async ({ page }) => {
    const opts_screen = { maxDiffPixels: 60 }
    const opts_screen_lenient = { maxDiffPixels: 150, threshold: 0.4 }

    await utils.open_app_with_model(page, abc_ka);
    await page.locator('#navinfluences').click();
    const table = page.locator('#influences-table');

    await expect.soft(table).toHaveScreenshot();
    await expect.soft(page.getByRole('cell', { name: 'Navigate through the nodes' })).toBeVisible();
    await page.getByRole('button', { name: 'First node' }).click();
    await expect.soft(table).toHaveScreenshot();
    await page.locator('#influence-rendering').selectOption('graph');
    await expect.soft(page.getByRole('img')).toHaveScreenshot(opts_screen_lenient);
    await page.locator('#influence-accuracy').selectOption('high');
    await expect.soft(page.getByRole('img')).toHaveScreenshot(opts_screen);
    await page.getByRole('button', { name: 'Next' }).click();
    await expect.soft(page.getByRole('img')).toHaveScreenshot(opts_screen);
    await page.getByRole('spinbutton', { name: 'Navigate' }).fill('2');
    await page.getByRole('spinbutton', { name: 'Navigate' }).press('Enter');
    await expect.soft(page.getByRole('img')).toHaveScreenshot(opts_screen_lenient); // TODO: check why there is more issues on chromium?
    await page.locator('#influence-rendering').selectOption('tabular');
    await page.getByRole('button', { name: 'Track cursor' }).click();
    await page.locator('div:nth-child(10) > .CodeMirror-line > span > span:nth-child(7)').scrollIntoViewIfNeeded();
    await page.locator('div:nth-child(10) > .CodeMirror-line > span > span:nth-child(7)').click();
    await expect.soft(table).toHaveScreenshot();
    await page.locator('div:nth-child(11) > .CodeMirror-line > span > span:nth-child(7)').click();
    await expect.soft(table).toHaveScreenshot();
    await page.getByRole('button', { name: 'Next' }).click();
    await expect.soft(table).toHaveScreenshot();
    await page.getByRole('button', { name: 'Previous' }).click();
    await expect.soft(table).toHaveScreenshot();
    //export
    await utils.testExports(page, '#export_influence-export', 'influences', ['json']);
  });

  function constraint_locator(page: Page, n: number) {
    return (page.locator('#constraints > .panel-scroll > div > .panel-body').nth(n));
  }

  test('constraints_and_polymers_1', async ({ page }) => {
    await utils.open_app_with_model(page, abc_ka);
    await page.locator('#navconstraints').click();
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
    await page.locator('#navpolymers').click();
    await expect.soft(page.getByRole('paragraph')).toHaveText(
      "The size of biomolecular compounds is uniformly bounded."
    );
  });

  test('constraints_and_polymers_2', async ({ page }) => {
    await utils.open_app_with_model(page, poly_ka);
    await page.locator('#navpolymers').click();
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
    await utils.open_app_with_model(page, local_views_slide_69_ka, true, 20000);
    await page.locator('#navconstraints').click();
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
      " « 1/1 » [model.ka] Dead rule R(CR[1] C[2]), R(CN[2]), R(CR[1]) -> R(CR[1] C[2]), R(CN[2]), R(CR[1]) "
    ]);
  });

  test('constraints_and_polymers_4', async ({ page }) => {
    await utils.open_app_with_model(page, counter_2_ka);
    await page.locator('#navconstraints').click();
    await expect.soft(constraint_locator(page, 4)).toHaveText(
      `A()  =>  A(c{[0 .. 2]})
`
    );
  });

  test('contact_map_accuracy', async ({ page }) => {
    // TODO: find a smaller example so that execution is faster
    test.setTimeout(180000)
    await utils.open_app_with_model(page, minikai_counters_ka, false, 120000);
    const contact_map = page.locator('#map-container');
    await expect.soft(contact_map).toHaveScreenshot();
    await page.locator('#contact_map-accuracy').selectOption('high');
    await expect.soft(contact_map).toHaveScreenshot();
  });

})

test.describe('Simulation tools', () => {

  test('Simulation, plot', async ({ page, browserName }) => {
    await utils.open_app_with_model(page, abc_ka);
    await utils.setSeed(page, 1);
    // Run simulation to 30, then 100, then test plot options
    await utils.set_pause_if(page, '[T] > 30');
    await page.getByRole('button', { name: 'start' }).click();
    await utils.wait_for_sim_stop(page, { timeout: 20000 });
    await page.locator('#navplot').click();
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

    await utils.testExports(page, '#export_plot-export', 'plot', ['csv', 'json', 'tsv'], undefined);
    await utils.testExports(page, '#export_plot-export', 'plot', ['svg'], undefined, browserName);
    if (browserName != "chromium") {
      await utils.testExports(page, '#export_plot-export', 'plot', ['png'], undefined, browserName);
      // TODO: pngs doesn't match on CI's chromium. check if we can test them in some way
    }

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

  test('DIN', async ({ page, browserName }) => {
    await utils.open_app_with_model(page, abc_ka);
    await utils.setSeed(page, 1);

    async function expectScreenShotDINTable(chromium_maxDiffPixels: number = 0) {
      await expect.soft(page.getByRole('cell', { name: 'affects' })).toBeVisible();
      const DIN_table_locator = page.locator('#DIN div').first();

      var opts_screen: utils.ScreenshotOptions = { mask: [page.locator('#export_din-export_form')] }
      // Chromium spacing between _numbers_ can be different across machines for some reason, here between CI and local
      // e.g. https://stackoverflow.com/questions/34814993/letter-spacing-is-different-with-same-browser-font-size-ect-in-chrome 
      if (browserName == "chromium") {
        opts_screen = {
          mask: opts_screen.mask,
          maxDiffPixels: chromium_maxDiffPixels
        }
      }
      await expect.soft(DIN_table_locator).toHaveScreenshot(opts_screen);
    }

    // Run simulation to 30, then 100, then test plot options
    await utils.set_pause_if(page, '[T] > 30');
    await page.getByRole('button', { name: 'start' }).click();

    await page.waitForTimeout(3000);
    // await utils.wait_for_file_load(page, { timeout: 20000 }); // TODO: fix: glyphicon doesn't show properly
    //
    await page.locator('#navDIN').click();
    await expectScreenShotDINTable(350);

    await utils.testExports(page, '#export_din-export', 'flux', ['json', 'dot', 'html']);

    await utils.set_pause_if(page, '[T] > 60');
    await page.getByRole('button', { name: 'continue' }).click();

    await page.waitForTimeout(3000);
    // await utils.wait_for_file_load(page, { timeout: 20000 }); // TODO: fix: glyphicon doesn't show properly
    //
    await expectScreenShotDINTable(350);
    await page.getByRole('combobox').first().selectOption('flux.json');
    await expectScreenShotDINTable(3000);

    await utils.testExports(page, '#export_din-export', 'flux_json', ['json', 'dot', 'html']);

    await page.getByRole('combobox').first().selectOption('flux.html');
    await expectScreenShotDINTable(350);
  });

  test('snapshots', async ({ page, browserName }) => {
    await utils.open_app_with_model(page, abc_ka);
    await utils.setSeed(page, 1);

    // Generate two snapshots
    await utils.set_pause_if(page, '[T] > 30');
    await page.getByRole('button', { name: 'start' }).click();
    await utils.wait_for_sim_stop(page, { timeout: 10000 });
    await utils.apply_perturbation(page, '');
    await expect.soft(utils.get_error_field(page)).toHaveText(" « 1/1 » [] Problematic effect list ");
    await utils.apply_perturbation(page, '$SNAPSHOT');
    await utils.set_pause_if(page, '[T] > 60');
    await page.getByRole('button', { name: 'continue' }).click();
    await utils.wait_for_sim_stop(page, { timeout: 10000 });
    await utils.apply_perturbation(page, '$SNAPSHOT "T60"');

    // check log page
    await page.locator('#navlog').click();
    await expect.soft(page.locator('#log div')).toHaveText(
      `+ Building initial simulation conditions...	 -variable declarations	 -rules	 -interventions	 -observables	 -update_domain construction	 21 (sub)observables 37 navigation steps	 -initial conditionsUser-set seed used for simulation: 1%mod: [E] = 14599 do $SNAPSHOT ;%mod: [E] = 28929 do $SNAPSHOT \"T60\";`
    );

    // go to tab snapshots and test display
    await page.locator('#navsnapshot').click();
    const snapshot_display_text_loc = page.locator('.navcontent-view > .panel-scroll').first();
    const snapshot_display_graph_loc = page.locator('.navcontent-view > div:nth-child(3)');

    const snapshot0 = `// Snapshot [Event: 14599]
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


`;

    const snapshot1 = `// Snapshot [Event: 28929]
%def: "T0" "60.00175023164761"

%init: 60 /*3 agents*/ B(x[1]), A(x[1] c[2]), C(x1{p}[.] x2{u}[2])
%init: 75 /*3 agents*/ A(x[1] c[2]), B(x[1]), C(x1{u}[2] x2{u}[.])
%init: 3 /*2 agents*/ A(x[.] c[1]), C(x1{u}[1] x2{u}[.])
%init: 94 /*2 agents*/ A(x[.] c[1]), C(x1{p}[.] x2{u}[1])
%init: 2925 /*1 agents*/ C(x1{u}[.] x2{u}[.])
%init: 5022 /*1 agents*/ C(x1{p}[.] x2{p}[.])
%init: 1821 /*1 agents*/ C(x1{p}[.] x2{u}[.])
%init: 258 /*2 agents*/ B(x[1]), A(x[1] c[.])
%init: 607 /*1 agents*/ B(x[.])
%init: 510 /*1 agents*/ A(x[.] c[.])


`;

    await expect.soft(snapshot_display_text_loc).toHaveText(snapshot0);
    await page.locator('#snapshot-select-id').selectOption('1');
    await expect.soft(snapshot_display_text_loc).toHaveText(snapshot1);
    await page.locator('#snapshot-select-id').selectOption('0');
    await expect.soft(snapshot_display_text_loc).toHaveText(snapshot0);
    await page.locator('#format_select_id').selectOption('Graph');
    await expect.soft(snapshot_display_graph_loc).toHaveScreenshot();
    await page.locator('.navcontent-view > div:nth-child(3)').click();
    await expect.soft(snapshot_display_graph_loc).toHaveScreenshot({ maxDiffPixels: 10 });
    await page.getByRole('button', { name: 'Back to root' }).click();
    await expect.soft(snapshot_display_graph_loc).toHaveScreenshot();
    await page.getByRole('radio', { name: 'Count' }).check();
    await expect.soft(snapshot_display_graph_loc).toHaveScreenshot();
    await page.getByRole('radio', { name: 'Size' }).check();
    await expect.soft(snapshot_display_graph_loc).toHaveScreenshot();
    // TODO: does something?
    // await page.getByRole('button', { name: 'Reset Zoom' }).click();

    // TODO: clicks in graph, link with contact map
    await page.locator('[id="root\\.mixture1"] rect').nth(1).click();
    await expect.soft(snapshot_display_graph_loc).toHaveScreenshot({ maxDiffPixels: 600 });
    // await page.locator('#force-container circle').nth(1).click();
    // await page.locator('#force-container circle').first().click();

    // Test exports
    await page.locator('#format_select_id').selectOption('Kappa');
    await utils.testExports(page, "#export_snapshot_kappa", "snapshot_kappa", ["json", "kappa", "dot"],
      ['', '', '"#\\w{5,6}"']);
    await page.locator('#format_select_id').selectOption('Graph');
    await utils.testExports(page, "#export_snapshot_graph", "snapshot_graph", ["json", "kappa", "dot"], ['', '', '"#\\w{5,6}"']);

    await utils.testExports(page, "#export_snapshot_graph", "snapshot_graph", ["svg"],
      ['<svg class="svg.*'], browserName);
    if (browserName != "chromium") {
      await utils.testExports(page, "#export_snapshot_graph", "snapshot_graph", ["png"],
        undefined, browserName);
      // TODO: pngs doesn't match on CI's chromium. check if we can test them in some way
    }
    // note: dot and svg export have special change as there is variance on their outputs if ran in playwright through the cli or through --ui …
    // dot : don't check colors, svg: only check there is a svg header
    // TODO: more complete match for svg, where the difference seems to be in the sizes…
    // Graphics are different between firefox and chrome… and on chrome png is also different between --ui and cli uuuuuh

  });

  test('outputs', async ({ page }) => {
    await utils.open_app_with_model(page, abc_ka);
    await utils.setSeed(page, 1);

    // Generate two snapshots
    await utils.set_pause_if(page, '[T] > 30');
    await page.getByRole('button', { name: 'start' }).click();
    await utils.wait_for_sim_stop(page, { timeout: 10000 });
    const print_time = '$PRINT "time: ".[T] > "time.txt"';
    const print_ab = '$PRINT \'AB\' > "ab.txt"';
    await utils.apply_perturbation(page, print_time);
    await page.waitForTimeout(500);
    await utils.apply_perturbation(page, print_ab);
    await utils.set_pause_if(page, '[T] > 60');
    await page.getByRole('button', { name: 'continue' }).click();
    await utils.wait_for_sim_stop(page, { timeout: 10000 });
    await utils.apply_perturbation(page, print_time);
    await page.waitForTimeout(500);
    await utils.apply_perturbation(page, print_ab);

    // check log page
    await page.locator('#navlog').click();
    await expect.soft(page.locator('#log div')).toHaveText(
      ` + Building initial simulation conditions...	 -variable declarations	 -rules	 -interventions	 -observables	 -update_domain construction	 21 (sub)observables 37 navigation steps	 -initial conditionsUser-set seed used for simulation: 1%mod: [E] = 14599 do $PRINTF ("time: ".[T]) > "time.txt";%mod: [E] = 14599 do $PRINTF (AB) > "ab.txt";%mod: [E] = 28929 do $PRINTF ("time: ".[T]) > "time.txt";%mod: [E] = 28929 do $PRINTF (AB) > "ab.txt";`
    );

    // outputs page
    await page.locator('#navoutputs').click();
    const outputs_display = page.locator('.show > .navcontent-view > .panel-scroll');

    await page.locator('#output-select-id').selectOption('time.txt');
    await expect.soft(outputs_display).toHaveText("time: 30.0032078729time: 60.0017502316");
    await page.locator('.list-group-item').click();
    await page.locator('#output-select-id').selectOption('ab.txt');
    await expect.soft(outputs_display).toHaveText("394393");

    const downloadPromise = page.waitForEvent('download');
    await page.getByRole('button', { name: 'All outputs' }).click();
    const download = await downloadPromise;

    utils.compare_zip_files_list_with_ref(download, []);
  });

});

test.describe('stories', () => {

  async function setup_stories(page: Page) {
    await utils.open_app_with_model(page, causality_slide_10_ka, true);
    await utils.setSeed(page, 1);

    // Enable trace
    await page.getByRole('list').locator('a').nth(2).click();
    await page.locator('.col-md-offset-2 > label').first().click();
    await page.getByRole('button', { name: 'Set', exact: true }).click();
    // gather trace
    await page.getByRole('button', { name: 'start' }).click();
    await utils.wait_for_sim_stop(page, { timeout: 20000 });

    // go to stories tab
    await page.locator('#navstories').click();
    // uncheck weakly
    await page.getByRole('checkbox', { name: 'Weakly' }).uncheck();
  }

  async function expect_texts_story_logs(page: Page, text_info_log: string, text_computation_log: string) {
    await page.getByRole('tab', { name: 'story_info_log' }).click();
    await expect.soft(page.locator('#story_info_log')).toHaveText(text_info_log);
    await page.getByRole('tab', { name: 'stories_computation_log' }).click();
    await expect.soft(page.locator('#stories_computation_log')).toHaveText(text_computation_log);
    await page.getByRole('tab', { name: 'story_info_log' }).click();
  }

  async function computeStoriesAndWait(page: Page) {
    await page.getByRole('button', { name: 'Launch' }).click();
    // wait for icon in project to be busy `refresh`, the be back to checkmark
    // (the `post` to the worker takes some time, which during when the icon is not refreshed)
    // TODO: update this when icon refresh is fixed
    await utils.wait_for_project_ready_status(page, { timeout: 30000 }, true);
  }

  async function computeStoriesAndTest(page: Page, text_info_log: string, text_computation_log: string, expect_screenshot: boolean = true) {
    await computeStoriesAndWait(page);
    await expect_texts_story_logs(page, text_info_log, text_computation_log);
    if (expect_screenshot) {
      await expect.soft(page.getByRole('img')).toHaveScreenshot();
    }
  }


  test('Empty', async ({ page }) => {
    await setup_stories(page);
    // No screenshot test as no stories causes no image to locate
    await computeStoriesAndTest(page, "", `Starting Compression
Compression completed
0 stories
`, false);
  });

  test('Weakly', async ({ page }) => {
    await setup_stories(page);
    await page.getByRole('checkbox', { name: 'Weakly' }).check();
    await computeStoriesAndTest(page,
      `ids: 11, 19, 24, 29, 33, 36, 37, 39, 49, 52, 55
t=0.0975547254717, 0.153578551271, 0.171807609491, 0.195251616607,
  0.211330564253, 0.218683483843, 0.219103111568, 0.231032193424,
  0.275917880116, 0.287969294047, 0.299259752742
event=2825, 3877, 4260, 4765, 5067, 5218, 5225, 5477, 6336, 6579, 6766`,
      `Starting Compression
Start one weak compression
Start one weak compression
Start one weak compression
Compression completed
3 stories
`);
  });

  test('Strongly', async ({ page }) => {
    await setup_stories(page);
    await page.getByRole('checkbox', { name: 'Strongly' }).check();
    await computeStoriesAndTest(page,
      `ids: 11, 19, 24, 29, 33, 36, 37, 39, 49, 52, 55, 5, 8, 21, 27, 28, 30, 31,
     32, 41, 42, 46, 47, 50, 51, 56, 1, 2, 3, 4, 6, 7, 9, 10, 12, 13, 14, 15,
     16, 17, 18, 20, 22, 23, 25, 26, 34, 35, 38, 40, 43, 44, 45, 48, 53, 54
t=0.0975547254717, 0.153578551271, 0.171807609491, 0.195251616607,
  0.211330564253, 0.218683483843, 0.219103111568, 0.231032193424,
  0.275917880116, 0.287969294047, 0.299259752742, 0.0500738649842,
  0.0884797831425, 0.161217577071, 0.18332499113, 0.186583686508,
  0.195789013044, 0.198978506925, 0.205627414085, 0.250574592161,
  0.26335509208, 0.272985766182, 0.274937277942, 0.28313183861,
  0.28605881908, 0.299316030636, 0.0137256652424, 0.0424530222206,
  0.0449356024089, 0.0459328458692, 0.0657465273502, 0.0702375030214,
  0.0958494110054, 0.0975448431689, 0.101919958829, 0.1115605239,
  0.117581700446, 0.125390166878, 0.126609656237, 0.139072907093,
  0.150316974123, 0.154324059758, 0.163465828786, 0.167329986128,
  0.172870749643, 0.181525571054, 0.21396617172, 0.217633967976,
  0.228163976202, 0.236188125158, 0.265437118532, 0.267501895402,
  0.272427852987, 0.274945449678, 0.28820790984, 0.288299009288
event=2825, 3877, 4260, 4765, 5067, 5218, 5225, 5477, 6336, 6579, 6766, 1815,
      2638, 4029, 4505, 4581, 4775, 4822, 4950, 5834, 6102, 6272, 6316, 6491,
      6544, 6767, 1103, 1662, 1709, 1728, 2159, 2246, 2781, 2823, 2900, 3076,
      3184, 3329, 3347, 3577, 3793, 3890, 4083, 4170, 4282, 4459, 5118, 5200,
      5423, 5563, 6127, 6167, 6263, 6317, 6581, 6583`,
      `Starting Compression
Start one strong compression
Start one strong compression
Start one strong compression
Compression completed
1 stories
`);

  });

  test('Causal + select stories', async ({ page }) => {
    await setup_stories(page);
    await page.getByRole('checkbox', { name: 'Causal' }).check();
    const computation_log = `Starting Compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Start one causal compression
Compression completed
26 stories
`;

    await computeStoriesAndTest(page,
      `ids: 56 t=0.299316030636 event=6767`,
      computation_log);
    await page.getByRole('combobox').selectOption('0');
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await expect_texts_story_logs(page,
      `ids: 43, 26, 22, 20, 16, 15, 10, 9, 6, 4, 3, 2, 1
t=0.265437118532, 0.181525571054, 0.163465828786, 0.154324059758,
  0.126609656237, 0.125390166878, 0.0975448431689, 0.0958494110054,
  0.0657465273502, 0.0459328458692, 0.0449356024089, 0.0424530222206,
  0.0137256652424
event=6127, 4459, 4083, 3890, 3347, 3329, 2823, 2781, 2159, 1728, 1709, 1662,
      1103`,
      computation_log);
    await page.getByRole('combobox').selectOption('2');
    await expect.soft(page.getByRole('img')).toHaveScreenshot();
    await expect_texts_story_logs(page,
      `ids: 35, 14, 7 t=0.217633967976, 0.117581700446, 0.0702375030214
event=5200, 3184, 2246`,
      computation_log);
  });

  test('Weakly + Strongly', async ({ page }) => {
    await setup_stories(page);
    await page.getByRole('checkbox', { name: 'Weakly' }).check();
    await page.getByRole('checkbox', { name: 'Strongly' }).check();
    await computeStoriesAndTest(page,
      `ids: 11, 19, 24, 29, 33, 36, 37, 39, 49, 52, 55
t=0.0975547254717, 0.153578551271, 0.171807609491, 0.195251616607,
  0.211330564253, 0.218683483843, 0.219103111568, 0.231032193424,
  0.275917880116, 0.287969294047, 0.299259752742
event=2825, 3877, 4260, 4765, 5067, 5218, 5225, 5477, 6336, 6579, 6766`
      ,
      `Starting Compression
Start one weak compression
Start one weak compression
Start one weak compression
Start one strong compression
Start one strong compression
Start one strong compression
Compression completed
3 stories
`);
  });

  test('Trace download', async ({ page }) => {
    await setup_stories(page);
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

    // project tab is `a` in `list`, `list` contains "active" class info, `a` is clickable
    const locator_project_tabs_list = page.locator('#projects_tabs');
    const locator_project_tabs_text = locator_project_tabs_list.locator('a');
    async function change_for_project(n: number) {
      await locator_project_tabs_text.nth(n).click();
      // await page.waitForTimeout(2000);
      await expect(locator_project_tabs_list.locator('li').nth(n)).toHaveClass("active", { timeout: 10000 });
    }
    async function wait_for_project_nb(nb: number) {
      await utils.expect_locator_toHaveInnerHtml(page, locator_project_tabs_text.nth(nb), "New project", 5000);
    }

    // open new project
    const project_name = 'new_project'
    // click new project button on second tab at startup
    await locator_project_tabs_text.nth(1).click();
    // fill project name entry
    await page.getByPlaceholder('project new').click();
    await page.getByPlaceholder('project new').fill(project_name);
    await page.getByPlaceholder('project new').press('Enter');
    await wait_for_project_nb(2);
    // add new file "abc.ka" and fill it
    await page.getByRole('button', { name: 'File' }).click();
    await page.locator('#menu-editor-file-new-li').click();
    await page.getByRole('textbox', { name: 'file name' }).click();
    await page.getByRole('textbox', { name: 'file name' }).fill('abc.ka');
    await page.getByRole('textbox', { name: 'file name' }).press('Enter');
    await page.waitForTimeout(5000);
    await page.locator('.CodeMirror-scroll').click();
    await utils.input_in_editor_from_url(page, abc_ka);
    await utils.wait_for_file_load(page, { timeout: 10000 });

    // switch and check contact_map change
    const contact_map = page.locator('#map-container');
    await expect.soft(contact_map).toHaveScreenshot();
    await change_for_project(1);
    await expect.soft(contact_map).toHaveScreenshot();
    await change_for_project(0);
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
    const fileChooserPromise = page.waitForEvent('filechooser');
    await page.locator('#menu-editor-file-open-li').click();
    const fileChooser = await fileChooserPromise;
    await fileChooser.setFiles(downloaded_path);

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
    await utils.wait_for_file_load(page, { timeout: 10000 });
    const contact_map = page.locator('#map-container');

    const opts_screen = { maxDiffPixels: 150 }
    await expect.soft(contact_map).toHaveScreenshot(opts_screen);

    // TODO: fix this flaky test: sometimes the graph doesn't show, bug?
    // simulate and test screenshot
    // await utils.set_pause_if(page, '[T] > 30');
    // await page.getByRole('button', { name: 'start' }).click();
    // await utils.wait_for_sim_stop(page, { timeout: 20000 });
    // await page.locator('#navplot').click();
    // await expect.soft(page.getByRole('img')).toHaveScreenshot();
  });

});
