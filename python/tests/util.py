from __future__ import absolute_import, print_function, unicode_literals
from builtins import dict, str

import sys
import unittest
from os import path, walk

import kappy


def find_path(top_dir, fname, force_type=None):
    """Find the path a given file somewhere bellow the top_dir."""
    for dirpath, sub_dirs, file_contents in walk(top_dir):
        if fname in file_contents and not force_type == 'directory':
            return path.join(dirpath, fname)
        elif fname in sub_dirs and not force_type == 'file':
            return path.join(dirpath, fname)

    return None


MODELS_DIR = find_path('../..', 'models', force_type='directory')
assert MODELS_DIR is not None, "Could not find models folder."


class _KappaClientTest(unittest.TestCase):

    # Universal Tests =========================================================

    def test_file_crud(self):
        print("Getting the runtime object...")
        runtime = self.getRuntime()

        print("Creating the file...")
        file_id = runtime.make_unique_id("test_file")
        model_str = ''
        runtime.add_model_string(model_str, 0, file_id)

        print("Getting the file names")
        file_names = [entry.id for entry in runtime.file_info()]

        print("Running checks...")
        self.assertIn(file_id, file_names)
        self.assertEqual(runtime.file_get(file_id).get_content(), model_str)
        runtime.file_delete(file_id)
        try:
            runtime.file_delete(file_id)
            self.fail()
        except kappy.KappaError:
            pass

    def test_parse_multiple_files(self):
        runtime = self.getRuntime()
        file_1_id = runtime.make_unique_id("file1.ka")
        file_2_id = runtime.make_unique_id("file2.ka")
        test_dir = path.join(MODELS_DIR, "test_suite", "compiler",
                             "file_order")
        f1_path = path.join(test_dir, 'file1.ka')
        runtime.add_model_file(f1_path, 1, file_1_id)
        f2_path = path.join(test_dir, 'file2.ka')
        runtime.add_model_file(f2_path, 2, file_2_id)
        with self.assertRaises(kappy.KappaError):
            runtime.add_model_file(f2_path, 2, file_2_id)
        file_names = [entry.id for entry in runtime.file_info()]
        self.assertIn(file_1_id, file_names)
        self.assertIn(file_2_id, file_names)
        return

    def test_run_simulation(self):
        print("Getting runtime...")
        runtime = self.getRuntime()

        file_id = runtime.make_unique_id('abc-pert')
        print("Adding model file %s..." % file_id)
        fpath = path.join(MODELS_DIR, "abc-pert.ka")
        runtime.add_model_file(fpath, 0, file_id)

        print("Parse project...")
        runtime.project_parse()

        print("Start simulation...")
        pause_condition = "[T] > 10.0"
        runtime.set_default_sim_param(0.1, pause_condition)
        runtime.simulation_start()

        print("Waiting for simulation to stop...")
        simulation_info = runtime.wait_for_simulation_stop()

        print("Checking that no limit returns all entries...")
        last_status = runtime.simulation_plot()
        test_count = 101
        self.assertEqual(test_count, len(last_status['series']))

        print("Got simulation info at end:\n%s" % simulation_info)
        print("Doing other checks...")
        plot_limit_offset = 100
        test_time = 10.0
        test_count = 1
        limit = kappy.PlotLimit(plot_limit_offset)
        last_status = runtime.simulation_plot(limit)
        self.assertEqual(test_count, len(last_status['series']))
        self.assertEqual(test_time, last_status['series'][0][0])

        plot_limit_offset = 10
        plot_limit_points = 1
        test_time = 1.0
        test_count = 1
        limit = kappy.PlotLimit(plot_limit_offset, plot_limit_points)
        last_status = runtime.simulation_plot(limit)
        self.assertEqual(test_count, len(last_status['series']))
        self.assertEqual(test_time, last_status['series'][0][0])

        plot_limit_offset = 50
        test_time = 10.0
        test_count = 51
        limit = kappy.PlotLimit(plot_limit_offset)
        last_status = runtime.simulation_plot(limit)
        self.assertEqual(test_count, len(last_status['series']))
        self.assertEqual(test_time, last_status['series'][0][0])

        print("Continuing simulation...")
        runtime.simulation_continue("[T] > 35")

        print("Waiting for second simulation to end...")
        runtime.wait_for_simulation_stop()

        # test that no limit returns all entries
        last_status = runtime.simulation_plot()
        self.assertEqual(351, len(last_status['series']))
        return


def run_nose(fname):
    import nose
    fpath = path.abspath(fname)
    print("Running nose for package: %s" % fname)
    return nose.run(argv=[sys.argv[0], fpath] + sys.argv[1:])
