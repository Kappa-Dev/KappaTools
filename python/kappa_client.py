""" Web api client for the kappa programming language
"""

import sys
import getopt
import time
import uuid
import kappa_common
import kappa_std
import kappa_rest

def project_catalog_project_id (project_catalog):
    print(project_catalog)
    return(map((lambda entry: entry["project_id"]),project_catalog["project_list"]))

def file_catalog_file_id (file_catalog):
    return(map((lambda entry: entry["id"]),file_catalog["file_metadata_list"]))

def scratch():
    file_id = str(uuid.uuid1())
    project_id_1 = "1"
    project_id_2 = "2"
    project_id_3 = "3"
    kappaStd = kappa_std.KappaStd("../bin/KaSimAgent")
    print(kappaStd.info())
    print(kappaStd.project_create(project_id_1))
    # print(kappaStd.project_create(project_id_2))
    # print(kappaStd.info())
    print(kappaStd.project_info())
    # file = File(FileMetadata(file_id,0),
    #             "%agent: A(x,c) # Declaration of agent A ")
    # print(kappaStd.file_create(project_id_1,file))
    # print(kappaStd.file_info(project_id_1))
    # print(kappaStd.file_get(project_id_1,file_id))
    # print(kappaStd.file_delete(project_id_1,file_id))
    # print(kappaStd.file_info(project_id_1))

    # file_id = str(uuid.uuid1())
    # project_id_1 = str(uuid.uuid1())
    # print(kappaStd.project_create(project_id_1))
    # inputfile = "/home/mwm1/Work/KaSim/models/abc.ka"
    # with open(inputfile) as f:
    #     code = f.read()
    #     file = File(FileMetadata(file_id,0),code)
    #     print(kappaStd.file_create(project_id_1,file))
    #     simulation_parameter =
    #       SimulationParameter(1,project_id_1,simulation_pause_condition = "[E] = 10")
    #     print(kappaStd.simulation_start(project_id_1,simulation_parameter))
    #     print(kappaStd.simulation_info(project_id_1,project_id_1))
    # kappaStd.shutdown()


def main():
    # command line
    argv = sys.argv[1:]
    cmd = "kappa_client.py"

    # default arguments
    inputfile = None  # if missing input file just get version
    url = "http://localhost:8080"
    pause_condition = "[false]"
    plot_period = 0.1
    seed = None

    help_line = (cmd +
                 ' -k <kappafile> ' +
                 ' -u <url or path to stdsim> ' +
                 ' -t <max_time> ' +
                 ' -e <max_events> ' +
                 ' -pp <plot_period> ' +
                 ' -s <random_seed> ')
    try:
        opts, args = getopt.getopt(argv,
                                   "h:k:u:t:e:pp:s",
                                   ["help","kappafile=",
                                    "url=",
                                    "max_time=",
                                    "max_event=",
                                    "plot_period=",
                                    "seed="])
    except:
        print(help_line)

        sys.exit(2)

    for opt, arg in opts:
        if opt == '-h':
            print(help_line)
            sys.exit()
        elif opt in ("-k", "--kappafile"):
            inputfile = arg
        elif opt in ("-u", "--url"):
            url = arg
        elif opt in ("-t", "--max_time"):
            pause_condition = "[T]>"+arg+" || "+pause_condition
        elif opt in ("-e", "--max_events"):
            pause_condition = "[E]>"+arg+" || "+pause_condition
        elif opt in ("-pp", "--plot_period"):
            plot_period = float(arg)
        elif opt in ("-s", "--seed"):
            seed = int(arg)

    print('Input file is : {0} '.format(inputfile))
    print('Endpoint url : {0} '.format(url))
    print('Pause conditon : {0}'.format(pause_condition))
    print('Plot period : {0} '.format(plot_period))
    print('Random seed : {0} '.format(seed))

    try:
        project_id = "{0}-{1}".format(cmd,str(uuid.uuid1()))
        if url.startswith('http'):
            runtime = kappa_rest.KappaRest(url,project_id)
        else:
            runtime = kappa_std.KappaStd(url)
        print("project_id : {0}".format(project_id))
        if inputfile:
            with open(inputfile) as f:
                code = f.read()
                file_content = str(code)
                file_metadata = kappa_common.FileMetadata(inputfile,0)
                file_object = kappa_common.File(file_metadata,file_content)
                runtime.file_create(file_object)
                runtime.project_parse()
                simulation_id = str(uuid.uuid1())
                print("simulation_id : {0}".format(simulation_id))

                end_time = 10.0
                simulation_parameter = kappa_common.SimulationParameter(plot_period,
                                                                        simulation_id,
                                                                        pause_condition,
                                                                        seed)
                runtime.simulation_start(simulation_parameter)

                simulation_info = runtime.simulation_info()

                while simulation_info["simulation_info_progress"]["simulation_progress_is_running"] :
                    time.sleep(1)

                    percentage = ""
                    time_percentage = simulation_info["simulation_info_progress"]["simulation_progress_time_percentage"]
                    event_percentage = simulation_info["simulation_info_progress"]["simulation_progress_event_percentage"]

                    if time_percentage or time_percentage == 0 :
                        percentage = time_percentage
                    if event_percentage or event_percentage == 0 :
                        percentage = event_percentage

                    sys.stdout.write("..{0}.. ".format(percentage))
                    sys.stdout.flush()
                    simulation_info = runtime.simulation_info()

                print("")
                print("info")
                print(simulation_info)
                plot_detail = runtime.simulation_detail_plot()
                print("plot")
                print(plot_detail)
        else:
            print(runtime.info())
    except kappa_common.KappaError as exception:
        print(exception.errors)
    return None
    None

if __name__ == "__main__":
    main()
