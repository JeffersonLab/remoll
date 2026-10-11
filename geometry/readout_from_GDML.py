#run instruction python readout_from_GDML.py DetectorArray.gdml rotations.txt 
#need to run this file in direcotry geometry

#!/usr/bin/env python3
import sys
from pathlib import Path
import xml.etree.ElementTree as ET

# Ring → extra number mapping
EXTRA_height_MAP = {"R1": 5, "R2": 30, "R3": 30, "R4": 60, "R5": 70, "R6": 50}
EXTRA_thickness_MAP = {"R1": 10, "R2": 10, "R3": 10, "R4": 10, "R5": 8.5, "R6": 10}
def detect_ring(name: str):
    for key in EXTRA_height_MAP:
        if key in name:
            return key
    return None

def get_detno(included_file: Path):
    try:
        tree = ET.parse(included_file)
        root = tree.getroot()
        node = root.find(".//auxiliary[@auxtype='DetNo']")
        if node is not None:
            return node.attrib.get("auxvalue")
    except Exception as e:
        print(f"[WARN] could not read {included_file}: {e}", file=sys.stderr)
    return None

def parse_main_gdml(gdml_path: Path, outfile: Path):
    tree = ET.parse(gdml_path)
    root = tree.getroot()

    with open(outfile, "w") as f:
        for phys in root.findall(".//physvol"):
            file_node = phys.find("file")
            rot_node  = phys.find("rotation")
            if file_node is None or rot_node is None:
                continue

            # rotation info
            name = rot_node.attrib.get("name", "")
            x = rot_node.attrib.get("x")
            y = rot_node.attrib.get("y")
            z = rot_node.attrib.get("z")
            if x is None or z is None or y is None:
                continue

            # ring → extra
            ring = detect_ring(name)
            extra_height = EXTRA_height_MAP.get(ring, "")
            extra_thick = EXTRA_thickness_MAP.get(ring, "")

            # included file → DetNo
            detno = None
            rel = file_node.attrib.get("name")
            if rel:
                inc_path = Path(rel)
                if inc_path.exists():
                    detno = get_detno(inc_path)

            # build output line
            id_value = detno if detno else name
            line = f"{{{id_value}, {{{x}, {y}, {z}"
            if extra_height != "" and extra_thick != "":
                line += f",{extra_height} ,{extra_thick} }}}}"
            f.write(line + "\n")

    print(f"[OK] Results written to {outfile}")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python parse_gdml_rotations.py <input.gdml> <output.txt>")
        sys.exit(1)

    parse_main_gdml(Path(sys.argv[1]), Path(sys.argv[2]))