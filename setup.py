from setuptools import setup

import distutils.cmd
import distutils.core
import subprocess
import setuptools.command.build_ext
import os.path

def should_build_agents():
    """
    Should we build agents? Currently this is decided based on
    checking for existence of a .ml file. Ideally, this should
    be made into a flag.
    """
    return os.path.isfile('agents/KaMoHa.ml')

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
        if should_build_agents():
            try:
                subprocess.check_call(["make","all","agents"])
            except subprocess.CalledProcessError:
                print("Failed to compile Kappa agents. Installing Python " \
                      "wrapper only.")

class MyBuildExtCommand(setuptools.command.build_ext.build_ext):
    """Compile Kappa agent in addition of standard build"""

    def run(self):
        self.my_outputs = []
        self.run_command('build_agents')
        bin_dir = os.path.join(self.build_lib, 'kappy/bin')
        distutils.dir_util.mkpath(bin_dir)
        if os.path.isfile('bin/KaSimAgent'):
            distutils.file_util.copy_file("bin/KaSimAgent", bin_dir)
            self.my_outputs.append(os.path.join(bin_dir, "KaSimAgent"))
        if os.path.isfile('bin/KaSaAgent'):
            distutils.file_util.copy_file("bin/KaSaAgent", bin_dir)
            self.my_outputs.append(os.path.join(bin_dir, "KaSaAgent"))
        if os.path.isfile('bin/KaMoHa'):
            distutils.file_util.copy_file("bin/KaMoHa", bin_dir)
            self.my_outputs.append(os.path.join(bin_dir, "KaMoHa"))
        setuptools.command.build_ext.build_ext.run(self)

    def get_outputs(self):
        outputs = setuptools.command.build_ext.build_ext.get_outputs(self)
        outputs.extend(self.my_outputs)
        return outputs

def readme():
    with open('python/README.rst') as f:
        return f.read()

setup(name='kappy',
      license='LGPLv3',
      version='4.0.94',
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
          'build_ext': MyBuildExtCommand,
      },
      install_requires=['requests', 'future', 'nose'],
      package_dir={'':'python'},
      include_package_data=True,
      data_files=[
          ('python', ['python/README.rst']),
      ],
      packages=['kappy'],
      zip_safe=False,
      # This distribution contains binaries not built with
      # distutils. So we must create a dummy Extension object so when
      # we create a binary file it knows to make it platform-specific.
      ext_modules=[distutils.core.Extension('kappy.dummy', sources = ['python/dummy.c'])],
)
