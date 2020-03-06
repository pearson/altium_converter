import subprocess
import os
import shutil
import gen_kicad_lib

mdb_dir = "mdbs"
out_dir = "output"

def process_mdb(mdb_path):
    result = subprocess.run(["tools/mdb-tables.exe", "-1", mdb_path], capture_output=True)
    tables = list(filter(None, result.stdout.decode('utf-8').split("\r\n")))
    csv_dir = mdb_path.replace('.mdb', '')
    lib_name = os.path.basename(mdb_path).replace('.mdb', '')
    lib_dir = os.path.join(out_dir, lib_name)
    
    if not os.path.exists(lib_dir):
        os.makedirs(lib_dir, True)
    
    if not os.path.exists(csv_dir):
        os.makedirs(csv_dir, True)
    
    for t in tables:
        csv_path = os.path.join(csv_dir, t + '.csv')
        
        with open(csv_path, 'w') as csv_file:
            subprocess.run(["tools/mdb-export.exe", mdb_path, t], stdout=csv_file)
        
        try:
            lib_path = csv_path.replace('.csv', '.lib')
            gen_kicad_lib.generate(csv_path, lib_path, lib_name, t)
            shutil.move(lib_path, lib_dir)
        except:
            print('!!! Could not generate libraries for {0}'.format(csv_path))


try:
    # remove the previous conversion result
    shutil.rmtree(out_dir)
except:
    pass

for mdb_file in [f for f in os.listdir(mdb_dir) if f.endswith('.mdb')]:
    print("Processing {0}...".format(mdb_file))
    process_mdb(os.path.join(mdb_dir, mdb_file))