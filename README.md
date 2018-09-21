## Altium to KiCad library converter
(c) 2017-2018 CERN

Scripts to convert Altium schematic and footprint libraries to KiCad format.
The scripts are written in the Altium scripting language (DelphiScript) and as
such, they require Altium. They have been tested with Altium 14.3, but most
likely work with other versions too.

### Configuration
The scripts have several options that might be changed by modifying *config.pas* file. Most of default values will work fine for you, but you should at least verify the layer mapping for footprint converter (especially mechanical layers) and modify *MODEL_PATH* if you want to have STEP files associated with footprints.

### Usage
Description below is for Altium 14.3, other versions may slightly differ. Each action will generate log files that will contain information about conversion issues. It is **highly recommneded** that you check the logs!
 - Run Altium
 - Select menu *DXP* -> *Run Script...*
 - In the dialog select *Browse* and pick *converters.PrjScr*
 - You should see a list of scripts, where *pcblib_converter.pas* and *schlib_converter.pas* have several subitems

**Symbol libraries (*.PcbLib)** (schlib_converter.pas)
 - BatchTemplateConversion: Loads a text file (*.txt) with a list of .SchLib files to generate symbol templates used in database library conversion. The file names should be separated with a newline character.
 - ConvertLibrary: Converts a single .SchLib file to a KiCad library. It will either convert the opened and active library or ask you to pick a file to convert.
 - GenerateTemplates: Generates symbol template files for database library conversion.

**Footprint libraries (*.SchLib)** (pcblib_converter.pas)
 - BatchConversion: Loads a text file (*.txt) with a list of .PcbLib files for conversion. The file names should be separated with a newline character.
 - ConvertLibrary: Converts a single .PcbLib file to a KiCad library. It will either convert the opened and active library or ask you to pick a file to convert.

**Integrated libraries (*.IntLib)**
Not supported yet.

**Database libraries (*.DbLib)**
At the moment only MS Access based libraries are supported. Database libraries store only symbol field values and link to a graphical representation in a .SchLib file.
Prerequisites: Python 3, [mdbtools](http://mdbtools.sourceforge.net/)
- Generate symbol templates using schlib_converter.pas
- Move the generated symbol templates to *dblib-converter/templates* directory
- Move *.mdb files to *dblib-converter*
- Run *process_mdb.sh <database.mdb>* on each file to convert

Note that *gen_kicad_lib.py* is adapted to a particular database layout. You may need to modify it to suit your needs.

### Limitations
Features not supported by the schematic **symbol converter**:
- Colors
- Fonts (size and attributes are imported)
- Dashed/dotted lines (converted as plain lines)
- Rounded rectangles with different X & Y corner radius
- Filled rounded rectangles (imported as stroked)
- Pictures
- IEEE symbols
- Components that have more than 2 graphical representations
- Several pin attributes that do not have a corresponding type in KiCad (e.g. schmitt, high current, right-left signal flow)
- Power pins might not always be correctly recognized (input or output)

Features not supported by the **footprint converter**:
- Certain pad shapes (octagonal are optionally approximated with round rectangle; no support for arcs, rotated rectangular shape pads) [might be improved thanks to custom shape pad support in KiCad]
- Copper arcs/circles
- Pads/vias with full stack-up definition (pads with shapes dependent on the layer)
- SMD pads on inner layers
- Square holes
- Rotated holes
- Vias
- Copper tracks
- Mirrored text
- TrueType fonts
- Multiline text
- Rotated fills (polygons)
