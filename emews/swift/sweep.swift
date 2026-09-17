import io;
import sys;
import files;
import string;
import emews;

string emews_root = getenv("EMEWS_PROJECT_ROOT");
string turbine_output = getenv("TURBINE_OUTPUT");

file upf = input(argv("f"));

int ni = string2int(argv("num_p"));
string gender = argv("gender");
int end_year = string2int(argv("end_year"));
string r_file = argv("r_file");
int policy_year = string2int(argv("policyyear"));
int dyear = string2int(argv("dyear"));
float dc = string2float(argv("dc"));


string run_model_template = """
from launch import run_sweep
import os

param_line = '%s'
gender = "%s"
end_year = "%s"
ni = %d
result_file = "%s"
run_id = "%s"
r_file = "%s"
policy_year = %d
dyear = %d
dc = %f

run_sweep(param_line, gender, end_year, ni, result_file, run_id, r_file, policy_year, dyear, dc)
""";


(void o) obj(string param_line, int instance) {
    result_f = "%s/results/%i_result.RData" % (turbine_output, instance);

    string code = run_model_template % (param_line, gender, end_year, ni, result_f, instance, r_file,
                                        policy_year, dyear, dc);
    python_persist(code, "str(1)") =>
    o = propagate();
}

// call this to create any required directories
app (void o) make_dir(string dirname) {
    "mkdir" "-p" dirname;
}

// Anything that needs to be done prior to a model
// run (e.g. file creation) should be done within this
// function.
// app (void o) run_prerequisites() {
//
// }

// Iterate over each line in the upf file, passing each line 
// to the model script to run
main() {
    // run_prerequisites() => {
    string upf_lines[] = file_lines(upf);
    foreach s, i in upf_lines {
        obj(s, i + 1);
    }
    // }
}
