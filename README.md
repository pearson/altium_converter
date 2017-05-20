TODO:
- 3D model association (footprints)

Limitations:

Schematic symbol conversion:
- Colors
- Fonts (size and attributes are imported)
- Dashed/dotted lines (converted as plain lines)
- Rounded rectangles with different X & Y corner radius
- Filled rounded rectangles (imported as stroked)
- Bitmap pictures
- IEEE symbols
- Components that have more than 2 graphical representations
- A few pin attributes that do not have a corresponding type in KiCad (e.g. schmitt, high current, right-left signal flow)
- Power pins might not be always correctly recognized (whether it is input or output)
