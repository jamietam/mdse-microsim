import files;
import string;
import sys;
import io;
import stats;
import python;
import math;
import location;
import assert;
import julia;

import EQPy;

string emews_root = getenv("EMEWS_PROJECT_ROOT");
string turbine_output = getenv("TURBINE_OUTPUT");

string resident_work_ranks = getenv("RESIDENT_WORK_RANKS");
string r_ranks[] = split(resident_work_ranks,",");

string ga_cfg_file = argv("ga_cfg");
int ni = string2int(argv("num_p"));
string gender = argv("gender");
int end_year = string2int(argv("end_year"));
string r_file = argv("r_file");

string run_model_template = """
from launch import run
import os

param_line = '%s'
gender = "%s"
end_year = "%s"
ni = %d
result_file = "%s"
run_id = "%s"
r_file = "%s"

run(param_line, gender, end_year, ni, result_file, run_id, r_file)

with open(result_file) as fin:
    gof = fin.readline().strip()
os.unlink(result_file)
""";

(string result) obj(string param_line, int iter, int run) {
    string run_id = "%d_%d" % (iter, run);
    result_f = "%s/tmp/%i_%i_result.txt" % (turbine_output, iter, run);

    string code = run_model_template % (param_line, gender, end_year, ni, result_f, run_id, r_file);
    result = python_persist(code, "gof");
}


(void v) loop (location ME) {
    for (boolean b = true, int i = 1;
       b;
       b=c, i = i + 1)
  {
    // gets the model parameters from the python algorithm
    string params = EQPy_get(ME);
    // printf("Received params");
    boolean c;
    if (params == "DONE") {
        printf("DONE");
        v = propagate() =>
        c = false;
    } else if (params == "EQPY_ABORT") {
        printf("EQPy Aborted");
        string why = EQPy_get(ME);
        printf("%s", why) =>
        v = propagate() =>
        c = false;
    } else {
        string param_array[] = split(params, ";");
        string results[];
        foreach p, j in param_array {
            results[j] = obj(p, i, j);
        }

        string res = join(results, ";");
        EQPy_put(ME, res) =>
        c = true;
    }
  }
  // v = propagate();
}

(void o) start (int ME_rank) {
    location deap_loc = locationFromRank(ME_rank);
    EQPy_init_package(deap_loc, "ga") =>
    EQPy_get(deap_loc) =>
    EQPy_put(deap_loc, ga_cfg_file) =>
    loop(deap_loc) => {
        EQPy_stop(deap_loc);
        o = propagate();
    }
}

app(void o) rm(string fname) {
    "rm" fname;
}

// deletes the specified directory
app (void o) rm_dir(string dirname) {
  "rm" "-rf" dirname;
}

// deletes the specified directory
app (void o) rm_dirs(file dirnames[]) {
  "rm" "-rf" dirnames;
}

// call this to create any required directories
app (void o) make_dir(string dirname) {
  "mkdir" "-p" dirname;
}

// anything that need to be done prior to a model runs
// (e.g. file creation) can be done here
//app (void o) run_prerequisites() {
//
//}


main() {
  int rank = string2int(r_ranks[0]);
  start(rank);
}
