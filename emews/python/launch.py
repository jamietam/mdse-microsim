import os
import subprocess
import datetime
from pathlib import Path
import stat


def run(param_line: str, gender: str, end_year: int, ni: int, result_file: str,
        run_id: str, r_file: str):

    turbine_output = os.environ["TURBINE_OUTPUT"]
    emews_root = os.environ["EMEWS_PROJECT_ROOT"]
    mdse_code_dir = os.path.join(emews_root, "../")
    script = os.path.join(turbine_output, "run_mdse_microsim.sh")

    out_txt = os.path.join(turbine_output, "tmp", f"{run_id}_out.txt")
    err_txt = os.path.join(turbine_output, "tmp", f"{run_id}_err.txt")

    env = os.environ.copy()
    env["MDSE_NUM_CORES"] = os.environ["PROCS_PER_RUN"]
    cmd = [script, gender, str(end_year), str(ni),
           result_file, param_line, mdse_code_dir, r_file]
    
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


def run_sweep(param_line: str, gender: str, end_year: int, ni: int, result_file: str,
              run_id: str, r_file: str, policy_year: int, dyear: int, dc):

    turbine_output = os.environ["TURBINE_OUTPUT"]
    emews_root = os.environ["EMEWS_PROJECT_ROOT"]
    mdse_code_dir = os.path.join(emews_root, "../")
    script = Path(turbine_output, "run_mdse_microsim_sweep.sh")
    current_mode = script.stat().st_mode
    new_mode = current_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH
    script.chmod(new_mode)


    out_txt = os.path.join(turbine_output, "tmp", f"{run_id}_out.txt")
    err_txt = os.path.join(turbine_output, "tmp", f"{run_id}_err.txt")

    env = os.environ.copy()
    env["MDSE_NUM_CORES"] = os.environ["PROCS_PER_RUN"]
    cmd = [script, gender, str(end_year), str(ni),
           result_file, param_line, mdse_code_dir, r_file,
           str(policy_year), str(dyear), str(dc)]
    
    try:
        start_t = datetime.datetime.now()
        proc = subprocess.run(cmd, cwd=turbine_output, env=env, capture_output=True, text=True)
        proc.check_returncode()
        end_t = datetime.datetime.now()
        with open(out_txt, 'w') as fout:
            fout.write(proc.stdout)
            fout.write(f"runtime: {(end_t - start_t).total_seconds()}")
        with open(err_txt, 'w') as fout:
            fout.write(proc.stderr)
    except subprocess.CalledProcessError as e:
        print(f"Error: see {out_txt} and {err_txt}", flush=True)
        with open(out_txt, 'w') as fout:
            fout.write(e.stdout)
        with open(err_txt, 'w') as fout:
            fout.write(e.stderr)
    except OSError as e:
        print(f"OSError: {e}", flush=True)
