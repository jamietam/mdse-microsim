import os
import subprocess
import datetime


def run(param_line: str, gender: str, end_year: int, ni: int, result_file: str,
        run_id: str):

    turbine_output = os.environ["TURBINE_OUTPUT"]
    emews_root = os.environ["EMEWS_PROJECT_ROOT"]
    mdse_code_dir = os.path.join(emews_root, "../")
    script = os.path.join(turbine_output, "run_mdse_microsim.sh")

    out_txt = os.path.join(turbine_output, "tmp", f"{run_id}_out.txt")
    err_txt = os.path.join(turbine_output, "tmp", f"{run_id}_err.txt")

    env = os.environ.copy()
    env["MDSE_NUM_CORES"] = os.environ["PROCS_PER_RUN"]
    cmd = [script, gender, str(end_year), str(ni),
           result_file, param_line, mdse_code_dir ]
    
    try:
        start_t = datetime.datetime.now()
        proc = subprocess.run(cmd, cwd=turbine_output, env=env, capture_output=True, text=True)
        proc.check_returncode()
        end_t = datetime.datetime.now()
        # with open(out_txt, 'w') as fout:
        #     fout.write(proc.stdout)
        #     fout.write(f"runtime: {(end_t - start_t).total_seconds()}")
        # with open(err_txt, 'w') as fout:
        #     fout.write(proc.stderr)
    except subprocess.CalledProcessError as e:
        print(f"Error: see {out_txt} and {err_txt}", flush=True)
        with open(out_txt, 'w') as fout:
            fout.write(e.stdout)
        with open(err_txt, 'w') as fout:
            fout.write(e.stderr)
    except OSError as e:
        print(f"OSError: {e}", flush=True)
