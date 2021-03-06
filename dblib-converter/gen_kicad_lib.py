#!/usr/bin/python3

# Script to create KiCad symbol libraries using symbol templates
# and .csv files to fill out the template fields.

# Copyright (C) 2017-2020 CERN
# author: Maciej Suminski <maciej.suminski@cern.ch>

import sys
import os
import csv
import string

# Configuration
# text size in symbols [mils]
fieldTextSize = 50
# vertical space between consecutive fields
fieldTextInterline = 1.5
# license text added in 'License' field
licenseText = """This work is licensed under the Creative Commons CC-BY-SA 4.0 License. To the extent that circuit schematics that use Licensed Material can be considered to be ‘Adapted Material’, then the copyright holder waives article 3.b of the license with respect to these schematics."""
# filter fields containing addresses accessible only in the internal network
filterURL = False


class MultiwordTemplate(string.Template):
    idpattern = '[_a-zA-Z][_a-zA-Z0-9 ]*'

class SymbolTemplate:
    def __init__(self, file_name, fields):
        # get the template contents
        self._template = []
        fhandle = open(file_name, 'r', encoding='utf-8')
        dsFieldLine = ''
        fieldNr = 0

        for line in fhandle:
            # process fields
            if line[0] == 'F':
                # use the datasheet URL field as the reference position
                # for other fields
                if line[1] == '3':
                    dsFieldLine = line

                # select the next field number
                fieldNr = fieldNr + 1
            elif line[0:4] == 'DRAW':   # no more field lines
                try:
                    # TODO this method will not correctly if there are spaces
                    # in ${Field Value}. Use regex instead?
                    # extract the position of the datasheet line
                    lineParts = dsFieldLine.split(' ')
                    posX = int(lineParts[2])
                    posY = int(lineParts[3]) - int(fieldTextSize * fieldTextInterline)

                    # add fields from .csv file
                    for field in fields:
                        field = field.strip()

                        # reserved fields which already included by default
                        if field in ('Reference', 'Value', 'Footprint', 'Datasheet'):
                            continue

                        # skip fields containing internal URLs, if requested
                        if filterURL and field in ('HelpURL', 'ComponentLink1URL', 'ComponentLink2URL', 'ComponentLink3URL'):
                            continue

                        fieldLine = 'F%d "${%s}" %d %d %d H I L CNN "%s"\n' \
                                    % (fieldNr, field, posX, posY, fieldTextSize, field)
                        self._template.append(MultiwordTemplate(fieldLine))

                        posY = posY - int(fieldTextSize * fieldTextInterline)
                        fieldNr = fieldNr + 1

                except e: # just process as many fields as possible
                    print(e)
                    pass

            # filter out header and comments
            if not line.startswith('#') and not line.startswith('EESchema-LIBRARY Version'):
                self._template.append(MultiwordTemplate(line))

        fhandle.close()

    def generate(self, field_dict):
        templ_inst = []

        for line in self._template:
            # empty fields are not allowed in KiCad, so replace them with '~'
            l = line.safe_substitute(field_dict).replace('""', '"~"')
            templ_inst.append(l)

        return templ_inst


def generate(filename_in, filename_out, db_table, lib_name):
    if not os.path.isfile(filename_in):
        sys.exit('Cannot open file %s' % filename_in)

    print('Converting %s' % filename_in)

    # map to translate schematic symbol names to templates
    # (replace characters that are invalid for filenames)
    filename_trans = str.maketrans('<>:\"\\/|?*', '_________')
    part_name_trans = str.maketrans(' /', '__')

    # map of loaded templates
    symbol_templates = {}


    # process files
    file_out = open(filename_out, 'w', encoding='utf-8')
    file_out.write('EESchema-LIBRARY Version 2.4\n')
    file_out.write('#encoding utf-8\n')

    with open(filename_in, 'r', encoding='utf-8') as file_in:
        cr = csv.DictReader(file_in, delimiter=',', quotechar='"')
        fields = None

        # try to obtain list of fields that will be added to the template
        try:
            f = open(filename_in + '.fields', 'r', encoding='utf-8')
            fields = f.readlines()
            f.close
        except:
            pass

        for part in cr:
            # escape quotes
            for key in part:
                # some .csv entries give 'Pin Count' as float values,
                # so convert them to int
                try:
                    if key == 'Pin Count':
                        part[key] = str(int(float(part[key])))
                except:
                    pass

                # try to evaluate '=FieldName' values
                try:
                    if part[key][0] == '=':
                        part[key] = part[part[key][1:]]
                except:
                    pass

                # KiCad does not allow empty fields
                if part[key] == '':
                    part[key] = ' '

                # escape backslashes and quotes
                else:
                    part[key] = part[key].replace('\\', '\\\\')
                    part[key] = part[key].replace('"', '\\"')

                # remove new-line characters
                part[key] = ' '.join(part[key].splitlines())

            # extract and store the footprint library name
            fp_lib = part['Footprint Path'].replace('.PcbLib', '')

            # strip path and extension from the template name
            slash = fp_lib.rfind('\\')
            if slash >= 0:
                fp_lib = fp_lib[slash+1:]

            # part number cannot contain spaces
            part['Part Number'] = part['Part Number'].translate(part_name_trans)

            # some additional fields
            part['Database Table Name'] = db_table
            part['Library Name'] = lib_name
            part['Footprint Library'] = fp_lib
            part['License'] = licenseText

            if filterURL:
                part['HelpURL'] = ' '

            # pick the corresponding template
            template_name = part['Library Ref'].translate(filename_trans)
            template_path = './templates/' + part['Library Path'].split('\\\\')[1].replace('.SchLib', '/')

            try:
                # either load a template or use the existing one
                if template_name in symbol_templates:
                    template = symbol_templates[template_name]
                else:
                    template = SymbolTemplate(template_path + template_name + '.lib',
                                        part.keys() if fields == None else fields)
                    symbol_templates[template_name] = template

                # generate a part instance
                for l in template.generate(part):
                    file_out.write(l)
            except:
                print('Skipping %s: \'%s%s.lib\' template is not available'
                        % (part['Part Number'], template_path, template_name))

            # print(part)

    file_out.write('#\n')
    file_out.write('#End Library')
    file_out.close()


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print('CSV to KiCad symbol library converter')
        print('usage: %s <library.csv> [output.lib]' % sys.argv[0])
        sys.exit()

    filename_in = sys.argv[1]
    filename_out = sys.argv[2] if len(sys.argv) >= 3 else filename_in.replace('.csv', '.lib')
    
    # TODO crappy way of extracting the database and library name
    path_in = filename_in.split('/')
    lib_name = path_in[-2] + '.DbLib'
    db_table = path_in[-1].replace('.csv', '')

    generate(filename_in, filename_out, db_table, lib_name)
