from __future__ import absolute_import, print_function, unicode_literals
from builtins import dict, str

import inspect

import kappy

from util import _KappaClientTest, run_nose


class StdClientTest(_KappaClientTest):
    """ Integration test for kappa client"""

    def getRuntime(self):
        return kappy.KappaStd()


def test_docs():
    kappa_std_doc = inspect.getdoc(kappy.KappaStd.file_create)
    kappa_abst_doc = inspect.getdoc(kappy.kappa_common.KappaApi.file_create)
    assert kappa_std_doc is not None and kappa_std_doc == kappa_abst_doc, \
        "Doc fixing failed: parent method doc not inheritted by KappaStd."


if __name__ == '__main__':
    run_nose(__file__)
