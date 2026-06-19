import subprocess
import time
from pathlib import Path
import os
import csv

# Configuration
TEST_DIR = "../experiment/experiment_cases"
ALLOWED_FOLDERS = {"array", "for", "nested_functions", "pairs", "exception", "large_programs"}
CONFIGS = [
    ("arm32", False, "ARM32 No Peephole"),
    ("arm32", True,  "ARM32 Peephole"),
    ("aarch64", False, "AArch64 No Peephole"),
    ("aarch64", True,  "AArch64 Peephole"),
]
MAX_TESTS = 50 # limit the maximum number of tests a folder can contain
ITERATION_NUM = 30 # number of iterations to run each binary file
BASE_CMD = "scala .. --"  # base command to generate .s

BIN_DIR = "/tmp/wacc_bins"
os.makedirs(BIN_DIR, exist_ok=True)

# Scan for .wacc files
def scan_wacc_files(base_dir):
    files = {folder: [] for folder in ALLOWED_FOLDERS}
    for root, dirs, filenames in os.walk(base_dir):
        # Only recurse into allowed folders
        dirs[:] = [d for d in dirs if d in ALLOWED_FOLDERS]

        for f in filenames:
            if not f.endswith(".wacc"):
                continue

            folder_name = Path(root).name
            if folder_name not in ALLOWED_FOLDERS:
                continue

            # Limit files per folder
            if len(files[folder_name]) < MAX_TESTS:
                files[folder_name].append(Path(root) / f)

    return files

# Compile .s to AArch64 binary
def compile_to_aarch64(asm_file, bin_name):
    bin_file = Path(BIN_DIR) / bin_name
    subprocess.run([
        "aarch64-linux-gnu-gcc",
        "-o", str(bin_file),
        "-z", "noexecstack",
        "-march=armv8-a",
        str(asm_file)
    ], check=True)
    return bin_file

# Compile .s to ARM32 binary
def compile_to_arm32(asm_file, bin_name):
    bin_file = Path(BIN_DIR) / bin_name
    subprocess.run([
        "arm-linux-gnueabi-gcc",
        "-o", str(bin_file),
        "-z", "noexecstack",
        "-march=armv6",
        str(asm_file)
    ], check=True)
    return bin_file

# Run binary in QEMU and measure the execution time
def run_and_measure(iterations, bin_file, arch):
    times = []
    for i in range(iterations):
        start = time.perf_counter_ns()

        if arch == "aarch64":
            subprocess.run([
                "qemu-aarch64",
                "-L", "/usr/aarch64-linux-gnu/",
                str(bin_file)
            ], check=True)
        else:
            subprocess.run([
                "qemu-arm",
                "-L", "/usr/arm-linux-gnueabi/",
                str(bin_file)
            ], check=True)

        end = time.perf_counter_ns()
        times.append((end - start) / 1e6)  

    return sum(times) / len(times)

# calculate the size of binary file, in bytes
def size_of_binary_file(bin_file, arch):
    if arch == "aarch64":
        output = subprocess.check_output(["aarch64-linux-gnu-size", str(bin_file)]).decode()
    else:
        output = subprocess.check_output(["arm-linux-gnueabi-size", str(bin_file)]).decode()
    
    lines = output.strip().split("\n")
    if len(lines) < 2:
        return 0
    total_bytes = int(lines[1].split()[3])
    return total_bytes

# Helper function to store the experimental results into .csv file
def store_result_csv(results, sizes):
    path = "../experiment"
    csv_file = Path(path) / "experiment_results.csv"
    with open(csv_file, "w", newline="") as f:
        writer = csv.writer(f)
        # Write header
        writer.writerow([
            "Config", "Folder", "AvgExecutionTime (ms)", "Runs (test case)", "AvgBinarySize (Byte)"
        ])

        # Write per-folder results
        for config, tests in results.items():
            for folder_class, (avg_time, runs) in tests.items():
                # Get size (in bytes)
                avg_size = sizes.get(config, 0)
                writer.writerow([config, folder_class, f"{avg_time:.3f}", runs, int(avg_size)])  

# Helper function to store the results for each program in the large_programs folder
def store_large_programs_csv(large_programs_data):
    path = "../experiment"
    csv_file = Path(path) / "large_programs_results.csv"

    with open(csv_file, "w", newline="") as f:
        writer = csv.writer(f)

        writer.writerow(["BinaryName", "ExecutionTime(ms)"])

        for bin_name, elapsed in large_programs_data.items():
            writer.writerow([bin_name, f"{elapsed:.3f}"])                

# ==========================================
# Main experiment
# ==========================================
def main():
    files_by_folder = scan_wacc_files(TEST_DIR)
    if not files_by_folder:
        print("No .wacc files found")
        return
    
    results = {}
    sizes = {}
    large_programs_data = {}
    for arch, peephole, config_name in CONFIGS:
        print(f"=== Running tests: {config_name} ===")
        results[config_name] = {}

        for folder_class, file_list in files_by_folder.items():
            total_time = 0
            runs = 0
            print(f"\n--- Folder: {folder_class} ---")

            total_size = 0
            for wacc_file in file_list:
                # 1. Generate .s assembly file
                cmd = [*BASE_CMD.split(), str(wacc_file)]

                cmd.extend(["--architecture", arch])

                if peephole:
                    cmd.append("--peephole-optim")
                else:
                    cmd.append("--no-peephole")

                try:
                    subprocess.run(cmd, check=True)
                except subprocess.CalledProcessError:
                    print(f"Failed to generate .s for {wacc_file}")
                    continue

                # 2. Locate .s file
                asm_file = Path(f"./{wacc_file.stem}.s")
                if not asm_file.exists():
                    print(f".s file not found for {wacc_file}")
                    continue

                # 3. Compile to binary
                bin_name = f"{wacc_file.stem}_{config_name.replace(' ', '')}"
                try:
                    if arch == "aarch64":
                        bin_file = compile_to_aarch64(asm_file, bin_name)
                    else:
                        bin_file = compile_to_arm32(asm_file, bin_name)
                    total_size += size_of_binary_file(bin_file, arch)    
                except subprocess.CalledProcessError:
                    print(f"Compilation failed for {asm_file}")
                    continue

                # 4. Run in QEMU and measure
                try:
                    elapsed = run_and_measure(ITERATION_NUM, bin_file, arch)
                    if folder_class == "large_programs":
                        large_programs_data[bin_name] = elapsed
                except subprocess.CalledProcessError:
                    print(f"Execution failed for {bin_file}")
                    continue

                print(f"{wacc_file}: {elapsed:.3f} ms")
                total_time += elapsed
                runs += 1
    
            if runs > 0:
                avg = total_time / runs
                results[config_name][folder_class] = (avg, runs)
            else:
                print(f"No successful runs for folder: {folder_class}\n")

            sizes[config_name] = total_size / len(file_list)    

    # Print the avaraged runtime of the binary files in each selected folder
    for config, tests in results.items():
        print("#################################################################")
        for folder_class, (avg, runs) in tests.items():
            print(
                f"{config}: average execution time of {folder_class} is "
                f"{avg:.3f} ms over {runs} runs\n"
            )        

    for config, avg in large_programs_data.items():
        print("#################################################################")
        print(
            f"{config}: average execution time is "
            f"{avg:.3f} ms"
        )   

    # Print the size of generated binary file
    for config, size in sizes.items():
        print(f"{config}: binary file size is {size:.3f} bytes")    

    # Store result into .csv file
    store_result_csv(results, sizes)   
    store_large_programs_csv(large_programs_data)     

if __name__ == "__main__":
    main()
