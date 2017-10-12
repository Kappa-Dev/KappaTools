from setuptools import setup

import distutils.cmd
import subprocess
import setuptools.command.build_py

class BuildAgentsCommand(distutils.cmd.Command):
    """Instruction to compile Kappa agents"""

    description = 'compile Kappa agents'
    user_options = [
        ('ocamlfind=', None, 'path to ocamlfind binary'),
    ]

    def initialize_options(self):
        self.ocamlfind = 'ocamlfind'

    def finalize_options(self):
        ()

    def run(self):
        subprocess.check_call(["make","agents"])
        distutils.dir_util.mkpath("python/kappy/bin")
        distutils.file_util.copy_file("bin/KaSimAgent", "python/kappy/bin/")

class MyBuildPyCommand(setuptools.command.build_py.build_py):
    """Compile Kappa agent in addition of standard build"""

    def run(self):
        self.run_command('build_agents')
        setuptools.command.build_py.build_py.run(self)


def readme():
    with open('python/README.rst') as f:
        return f.read()

setup(name='kappy',
      license='LGPLv3',
      version='4.0.0.dev3',
      description='Wrapper to interact with the Kappa tool suite',
      long_description=readme(),
      url='https://github.com/Kappa-Dev/KaSim.git',
      author='Kappa-Dev',
      author_email='kappa-dev@listes.sc.univ-paris-diderot.fr',
      classifiers=[
          'License :: OSI Approved :: GNU Lesser General Public License v3 (LGPLv3)',
          'Topic :: Scientific/Engineering :: Bio-Informatics'
      ],
      cmdclass={
          'build_agents': BuildAgentsCommand,
          'build_py': MyBuildPyCommand,
      },
      extras_require={
          'REST': ['urllib3'],
      },
      package_dir={'':'python'},
      include_package_data=True,
      package_data={
          '': ['README.rst'],
          'kappy':['bin/KaSimAgent'],
      },
      packages=['kappy'],
      zip_safe=False)
