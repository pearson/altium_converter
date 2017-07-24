{*
 * Altium to KiCad footprint library converter script
 *
 * Copyright (C) 2017 CERN
 * @author Maciej Suminski <maciej.suminski@cern.ch>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, you may find one here:
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * or you may search the http://www.gnu.org website for the version 2 license,
 * or you may write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 *}

uses StrUtils;

var
  // converted footprint name, used for logging
  footprint : String;

  // current output file
  outFile   : TextFile;

  // footprint origin
  fpX, fpY  : Integer;

  // default reference and value size in Altium
  textSizeAltium : Integer;

  // footprint relief settings
  reliefGap : Integer;
  reliefWidth : Integer;
  reliefStyle : Integer;


procedure throw(aMessage : TDynamicString);
begin
    log(aMessage);
    raise; // no, you cannot create exception objects in DelphiScript..
end;


function pcbXYToStr(aX : TCoord, aY : TCoord) : TDynamicString;
begin
    // inverted Y-axis
    result := XYToStr(aX - fpX, fpY - aY);
end;


function fontSize(aFontID : TFontID) : Integer;
begin
     result := PCBServer.FontManager.Size(aFontID) * 5
end;


function shapeToStr(aItem : IPCB_Primitive) : TDynamicString;
begin
    case aItem.TopShape of
        eNoShape: throw(footprint + ': invalid shape (eNoShape)');

        // KiCad and Altium have different definitions for rounded rectangle
        // corner percentage, hence division by 2
        eRoundedRectangular,    // TODO is it ok?
        eRoundRectShape:        result := 'roundrect (roundrect_rratio 0.'
            + Format('%.2d', [aItem.CRPercentage[aItem.Layer] div 2]) + ')';

        eRectangular:  result := 'rect';

        eOctagonal:
            if OCTAGON_TO_ROUNDRECT then
            begin
                log(footprint + ': octagonal shape approximated with a round rectangle');
                result := 'roundrect';
            end
            else
                throw(footprint + ': ERROR: octagonal shapes are disabled');

        eRounded,
        eCircleShape: result := 'circle';

        eArcShape:
            throw(footprint + ': ERROR: arc shape is not supported');

        eTerminator:
            throw(footprint + ': ERROR: terminator shape is not supported');

        eRotatedRectShape:
            throw(footprint + ': ERROR: rotated rectangular shape is not supported');
    end;
end;


function padTypeToStr(aPad : IPCB_Pad) : TDynamicString;
begin
    // thru_hole, smd, connect, np_thru_hole
    if aPad.IsSurfaceMount then
    begin
        result := 'smd';
    end
    else if aPad.Plated then
    begin
        result := 'thru_hole';
    end
    else
    begin
        result := 'np_thru_hole';
    end;

    // TODO check for paste layer and select 'connect' if not present?
end;


function isCopperLayer(aLayer : TLayer) : Boolean;
begin
    // none of the below works..
    //result := (aLayer >= eTopLayer) and (aLayer <= eBottomLayer);
    //result := (aLayer in SignalLayers);
    {if aLayer in SignalLayers then
        result := true
    else
        result := false;}

    case aLayer of
        eTopLayer,
        eMidLayer1,
        eMidLayer2,
        eMidLayer3,
        eMidLayer4,
        eMidLayer5,
        eMidLayer6,
        eMidLayer7,
        eMidLayer8,
        eMidLayer9,
        eMidLayer10,
        eMidLayer11,
        eMidLayer12,
        eMidLayer13,
        eMidLayer14,
        eMidLayer15,
        eMidLayer16,
        eMidLayer17,
        eMidLayer18,
        eMidLayer19,
        eMidLayer20,
        eMidLayer21,
        eMidLayer22,
        eMidLayer23,
        eMidLayer24,
        eMidLayer25,
        eMidLayer26,
        eMidLayer27,
        eMidLayer28,
        eMidLayer29,
        eMidLayer30,
        eBottomLayer:       result := true;
        else                result := false;
    end;
end;


function layerToStr(aLayer : TLayer) : TPCB_String;
begin
    case aLayer of
        // these layers have direct counterparts
        eTopLayer:           result := 'F.Cu';
        eMidLayer1:          result := 'In1.Cu';
        eMidLayer2:          result := 'In2.Cu';
        eMidLayer3:          result := 'In3.Cu';
        eMidLayer4:          result := 'In4.Cu';
        eMidLayer5:          result := 'In5.Cu';
        eMidLayer6:          result := 'In6.Cu';
        eMidLayer7:          result := 'In7.Cu';
        eMidLayer8:          result := 'In8.Cu';
        eMidLayer9:          result := 'In9.Cu';
        eMidLayer10:         result := 'In10.Cu';
        eMidLayer11:         result := 'In11.Cu';
        eMidLayer12:         result := 'In12.Cu';
        eMidLayer13:         result := 'In13.Cu';
        eMidLayer14:         result := 'In14.Cu';
        eMidLayer15:         result := 'In15.Cu';
        eMidLayer16:         result := 'In16.Cu';
        eMidLayer17:         result := 'In17.Cu';
        eMidLayer18:         result := 'In18.Cu';
        eMidLayer19:         result := 'In19.Cu';
        eMidLayer20:         result := 'In20.Cu';
        eMidLayer21:         result := 'In21.Cu';
        eMidLayer22:         result := 'In22.Cu';
        eMidLayer23:         result := 'In23.Cu';
        eMidLayer24:         result := 'In24.Cu';
        eMidLayer25:         result := 'In25.Cu';
        eMidLayer26:         result := 'In26.Cu';
        eMidLayer27:         result := 'In27.Cu';
        eMidLayer28:         result := 'In28.Cu';
        eMidLayer29:         result := 'In29.Cu';
        eMidLayer30:         result := 'In30.Cu';
        eBottomLayer:        result := 'B.Cu';
        eTopOverlay:         result := 'F.SilkS';
        eBottomOverlay:      result := 'B.SilkS';
        eTopPaste:           result := 'F.Paste';
        eBottomPaste:        result := 'B.Paste';
        eTopSolder:          result := 'F.Mask';
        eBottomSolder:       result := 'B.Mask';
        else                 result := layerMapping(aLayer);
    end;

    if (result = '') then
    begin
        if ABORT_ON_UNKNOWN_LAYER then
            throw(footprint + ': ERROR: unmapped layer ' + cLayerStrings[aLayer])
        else
            log(footprint + ': unmapped layer ' + cLayerStrings[aLayer]);
    end;
end;


function find3DModel(aFootprint : TDynamicString) : TDynamicString;
var
    modelFile: TString;
    // does not work, do not ask me..
    //extensions : array[0..1] of TDynamicString;
begin
    //extensions[0] := '.stp';
    //extensions[1] := '.step';

    modelFile := MODEL_PATH + '\' + aFootprint;

    if FileExists(modelFile + '.stp') then
        result := ifElse(USE_FULL_MODEL_PATH, modelFile, aFootprint) + '.stp'
    else if FileExists(modelFile + '.step') then
        result := ifElse(USE_FULL_MODEL_PATH, modelFile, aFootprint) + '.step'
    else
        result := '';
end;


procedure processArc(aArc : IPCB_Arc);
var
    endPt : TLocation;
    angle : TAngle;
    isCircle : Boolean;
begin
    // handles both arcs and circles (special kind of arc)
    // (fp_arc (start 6.25 5.3) (end -6.25 5.3) [(angle 100)] (layer F.CrtYd) (width 0.05))

    if isCopperLayer(aArc) then
        throw(footprint + ': ERROR: copper arcs are not supported');

    isCircle := (aArc.StartAngle = 0) and (aArc.EndAngle = 360);
    endPt := TLocation;

    if aArc.StartAngle > aArc.EndAngle then
    begin
        endPt.x := aArc.EndX;
        endPt.y := aArc.EndY;
        angle := aArc.EndAngle + (360 - aArc.StartAngle);
    end
    else
    begin
        endPt.x := aArc.StartX;
        endPt.y := aArc.StartY;
        angle := aArc.StartAngle - aArc.EndAngle;
    end;

    Write(outFile, ifElse(isCircle, '(fp_circle ', '(fp_arc ')
       + ifElse(isCircle, '(center ', '(start ') + pcbXYToStr(aArc.XCenter, aArc.YCenter) + ') '
       + '(end ' + pcbXYToStr(endPt.x, endPt.y) + ') ');

    if not isCircle then
        Write(outFile, '(angle ' + IntToStr(angle) + ') ');

    WriteLn(outFile, '(layer ' + layerToStr(aArc.Layer) + ') '
       + '(width ' + sizeToStr(aArc.LineWidth) + '))');
end;


procedure processPad(aPad : IPCB_Pad);
var
    width, height, pasteMargin, solderMargin : TCoord;
begin
    // (pad 1 smd rect (at -4 0 180) (size 4 2.5) (layers F.Cu F.Paste F.Mask))

    if aPad.Mode <> ePadMode_Simple then
    begin
        // there is still a chance if all layers have the same shape and size
        // (basically it is a simple stack)
        if not((aPad.Mode = ePadMode_LocalStack) and
          (aPad.TopShape = aPad.MidShape) and (aPad.MidShape = aPad.BotShape) and
          (aPad.TopXSize = aPad.MidXSize) and (aPad.MidXSize = aPad.BotXSize) and
          (aPad.TopYSize = aPad.MidYSize) and (aPad.MidYSize = aPad.BotYSize)) then
            throw(footprint + ': ERROR: only simple pads are supported: ' + aPad.Name);
    end;

    if Length(aPad.Name) > 4 then
    begin
        if TRUNCATE_PAD_NAMES then
            log(footprint + ': pad name truncated from ' + aPad.Name
                + ' to ' + Copy(aPad.Name, 1, 4))
        else
            throw(footprint + ': ERROR: too long pad name: ' + aPad.Name);
    end;

    if (aPad.HoleSize > aPad.TopXSize) or (aPad.HoleSize > aPad.TopYSize) then
    begin
        // TODO we could handle cases when paste margin & solder margin are
        // completely covered by the hole, but it will be an ugly if
        // keep in mind that the margins depend on the pad shape, not on the
        // hole shape, so each hole-pad shape combination needs to be tested
        if aPad.TopXSize <> aPad.TopYSize then
            throw(footprint + ': ERROR: pads with different height and width and hole smaller than the pad size are not supported');

        // in Altium holes bigger than pads are allowed, in KiCad it is not possible
        // resize pads to the hole size if they are smaller
        width := Max(aPad.HoleSize, aPad.TopXSize);
        height := Max(aPad.HoleSize, aPad.TopYSize);

        // Altium computes solder and paste clearance basing on the pad size,
        // but if the pad size is smaller than the hole - it has to be adjusted
        pasteMargin := Max(0, aPad.PasteMaskExpansion - (aPad.HoleSize - aPad.TopXSize) div 2);
        solderMargin := Max(0, aPad.SolderMaskExpansion - (aPad.HoleSize - aPad.TopXSize) div 2);
    end
    else
    begin  // normal case (hole smaller than the pad size)
        width := aPad.TopXSize;
        height := aPad.TopYSize;
        pasteMargin := aPad.PasteMaskExpansion;
        solderMargin := aPad.SolderMaskExpansion;
    end;

    // pad name in KiCad is limited to 4 chars, spaces are allowed
    Write(outFile, '(pad ' + Copy(aPad.Name, 1, 4)
        + ' ' + padTypeToStr(aPad) + ' ' + shapeToStr(aPad)
        + ' (at ' + pcbXYToStr(aPad.X, aPad.Y)
        + ifElse(aPad.Rotation <> 0, ' ' + IntToStr(aPad.Rotation), '') + ') '
        + '(size ' + XYToStr(width, height) + ') ');

    if aPad.IsSurfaceMount then
    begin
        case aPad.Layer of
            eTopLayer:    Write(outFile, '(layers F.Cu F.Paste F.Mask)');
            eBottomLayer: Write(outFile, '(layers B.Cu B.Paste B.Mask)');
            else throw(footprint + ': ERROR: invalid layer '
                    + cLayerStrings[aPad.Layer] + ' for SMD pad ' + aPad.Name);
        end;
    end
    else
    begin
        // TODO could be easily handled for slot holes and angles 90, 180, 270
        if (aPad.HoleRotation <> 0) and (aPad.HoleType <> eRoundHole) then
            throw(footprint + ': ERROR: rotated holes are not supported');

        Write(outFile, '(drill ');

        case aPad.HoleType of
            eRoundHole:
                Write(outFile, sizeToStr(aPad.HoleSize));

            eSquareHole:
                throw(footprint + ': ERROR: square holes are not supported');

            eSlotHole:
                Write(outFile, 'oval ' + sizeToStr(aPad.HoleWidth) + ' ' + sizeToStr(aPad.HoleSize));
        end;

        // drill offset
        if (aPad.XPadOffset[aPad.Layer] <> 0) or (aPad.YPadOffset[aPad.Layer] <> 0) then
           Write(outFile, ' (offset ' + XYToStr(aPad.XPadOffset[aPad.Layer], -aPad.YPadOffset[aPad.Layer]) + ')');

        Write(outFile, ')');

        Write(outFile, ' (layers *.Cu *.Mask)');
    end;

    Write(outFile, ' (solder_mask_margin ' + sizeToStr(solderMargin) + ')');
    Write(outFile, ' (solder_paste_margin ' + sizeToStr(pasteMargin) + ')');

    WriteLn(outFile, ')');

    // INFO clearance, zone_connect, thermal_gap, thermal_width seem to be
    // correct only for pads placed on a board, where design rules are set
end;



procedure processTrack(aTrack : IPCB_Track);
begin
    // graphical line
    // (fp_line (start 6.25 5.3) (end -6.25 5.3) (layer F.CrtYd) (width 0.05))

    if isCopperLayer(aTrack) then
        throw(footprint + ': ERROR: copper tracks are not supported');

    WriteLn(outFile, '(fp_line '
       + '(start ' + pcbXYToStr(aTrack.X1, aTrack.Y1) + ') '
       + '(end ' + pcbXYToStr(aTrack.X2, aTrack.Y2) + ') '
       + '(layer ' + layerToStr(aTrack.Layer) + ') '
       + '(width ' + sizeToStr(aTrack.Width) + '))');
end;


// procedure to convert texts into lines, so the silkscreen texts are exactly the same
// TODO: ConvertToStrokeArray is documented but unavailable
{procedure strokeText(aText : IPCB_Text);
var
    count, i : Integer;
    strokes : TStrokeArray;
    layer, width : TDynamicString;
begin
    aText.ConvertToStrokeArray(count, strokes);
    aText.Get

    layer := layerToStr(aText.Layer);
    width := sizeToStr(aText.Width);

    for i := 0 to count - 1 do
    begin
        WriteLn(outFile, '(fp_line '
           + '(start ' + pcbXYToStr(strokes[i].X1, strokes[i].Y1) + ') '
           + '(end ' + pcbXYToStr(strokes[i].X2, strokes[i].Y2) + ') '
           + '(layer ' + layer + ') ' + '(width ' + width + '))');
    end;
end;}


procedure processText(aText : IPCB_Text);
var
    pos : TLocation;
    rotation : TAngle;
begin
    // (fp_text reference R1 (at 0 0.127) (layer F.SilkS) hide
    //     (effects (font (size 1.397 1.27) (thickness 0.2032)))
    // )

    {if STROKE_SILK_TEXT and ((aText.Layer = eTopOverlay) or (aText.Layer = eBottomOverlay)) then
    begin
        strokeText(aText);
        Exit;
    end;}

    if aText.MirrorFlag then
        throw(footprint + ': mirrored text is not supported');

    if aText.UseTTFonts then
    begin
        log(footprint + ': true type fonts are not supported');

        if ALLOW_TTF then
            log(footprint + ': true type fonts are drawn using the stroke font')
        else
            throw(footprint + ': ERROR: true type fonts disabled');
    end;

    if isMultiline(aText.Text) then
        throw(footprint + ': ERROR: multiline texts are not supported: ' + aText.Text);

    rotation := aText.Rotation;

    if rotation >= 270 then
        Dec(rotation, 360);

    if rotation <= -270 then
        Inc(rotation, 360);

    if (rotation > 90) or (rotation < -90) then
        throw(footprint + ': ERROR: text rotation has to be in range [-90,90]: ' + aText.Text);

    // Altium uses left bottom corner as a reference,
    // while in KiCad it is the text centre
    pos := TLocation;
    pos.x := aText.BoundingRectangle.left + (aText.BoundingRectangle.right - aText.BoundingRectangle.left) / 2;
    pos.y := aText.BoundingRectangle.bottom + (aText.BoundingRectangle.top - aText.BoundingRectangle.bottom) / 2;

    WriteLn(outFile, '(fp_text user "' + escapeQuotes(aText.Text) + '" (at '
         + pcbXYToStr(pos.x, pos.y)
         + ifElse(rotation <> 0, ' ' + IntToStr(rotation), '') + ')'
         + ' (layer ' + layerToStr(aText.Layer)
         + ifElse(aText.IsHidden, ' hide', '') + ')');
    WriteLn(outFile, '    (effects (font (size ' + sizeToStr(aText.Size)
         + ' ' + sizeToStr(aText.Size) + ')'
         + ' (thickness ' + sizeToStr(aText.Width) + ')))');
    WriteLn(outFile, ')');
end;


procedure processObject(aObject : IPCB_Primitive);
begin
    case aObject.ObjectId of
        eNoObject:              throw(footprint + ': ERROR: contains an invalid object (eNoObject)');
        eArcObject:             processArc(aObject);
        ePadObject:             processPad(aObject);
        eViaObject:             throw(footprint + ': ERROR: vias are not supported');
        eTrackObject:           processTrack(aObject);
        eTextObject:            processText(aObject);
        eFillObject:            throw(footprint + ': ERROR: fills are not supported');
        eConnectionObject:      throw(footprint + ': ERROR: connections are not supported');
        eNetObject:             throw(footprint + ': ERROR: nets are not supported');
        eComponentObject:       throw(footprint + ': ERROR: components are not supported');
        ePolyObject:            throw(footprint + ': ERROR: polys are not supported');
        eDimensionObject:       throw(footprint + ': ERROR: dimensions are not supported');
        eCoordinateObject:      throw(footprint + ': ERROR: coordinates are not supported');
        eClassObject:           throw(footprint + ': ERROR: classes are not supported');
        eRuleObject:            throw(footprint + ': ERROR: rules are not supported');
        eFromToObject:          throw(footprint + ': ERROR: fromtos are not supported');
        eViolationObject:       throw(footprint + ': ERROR: violations are not supported');
        eEmbeddedObject:        throw(footprint + ': ERROR: embedded objects are not supported');
        eTraceObject:           throw(footprint + ': ERROR: traces are not supported');
        eSpareViaObject:        throw(footprint + ': ERROR: spare vias are not supported');
        eBoardObject:           throw(footprint + ': ERROR: boards are not supported');
        eBoardOutlineObject:    throw(footprint + ': ERROR: board outlines are not supported');
    end;
end;


function processFootprint(aFootprint : IPCB_LibComponent) : Boolean;
var
    objIterator : IPCB_GroupIterator;
    pcbObj      : IPCB_Primitive;
    bbox        : TCoordRect;
    model       : TDynamicString;
begin
    footprint := aFootprint.Name;
    fpX := aFootprint.X;
    fpY := aFootprint.Y;
    result := true;     // assume it is ok

    // footprint bbox computed while processing children items
    // (IPCB_LibComponent::BoundingRectangle() does not work)
    bbox := TCoordRect;

    objIterator := aFootprint.GroupIterator_Create();

    WriteLn(outFile, '(module "' + escapeQuotes(fixSpaces(footprint)) + '" (layer F.Cu) (tedit 0)');
    WriteLn(outFile, '(descr "' + escapeQuotes(aFootprint.Description) + '")');

    // TODO smd/virtual attributes, tags
    //WriteLn(outFile, '(attr smd)');
    //WriteLn(outFile, '(tags xxx)');

    // TODO is it ever set?
    if aFootprint.SolderMaskExpansion <> 0 then
        WriteLn(outFile, '(solder_mask_margin '
            + sizeToStr(aFootprint.SolderMaskExpansion) + ')');

    // TODO is it ever set?
    if aFootprint.PasteMaskExpansion <> 0 then
        WriteLn(outFile, '(solder_paste_margin '
            + sizeToStr(aFootprint.PasteMaskExpansion) + ')');

    // INFO documented property, but it does not work
    {if aFootprint.DefaultPCB3DModel <> '' then
        WriteLn(outFile, '(model ' + aFootprint.DefaultPCB3DModel + ')');}
    model := find3DModel(footprint);

    if model <> '' then
        WriteLn(outFile, '(model "' + escapeQuotes(model) + '")');
        // TODO at, scale, rotate

    // INFO there are thermal relief settings, but they does not seem valid in
    // libraries (only on a board with design rules set)
    // WriteLn(outFile, '(clearance dim)');
    // WriteLn(outFile, '(zone_connect val');
    // WriteLn(outFile, '(thermal_width val)');
    // WriteLn(outFile, '(thermal_gap val)');

    bbox.bottom := 2147483647;  //High(Integer);
    bbox.top    := -2147483648; //Low(Integer);

    try
        try
            pcbObj := objIterator.FirstPCBObject;

            while pcbObj <> nil do
            begin
                processObject(pcbObj);
                //bbox.left   := Min(bbox.left, pcbObj.BoundingRectangle.left);
                //bbox.right  := Max(bbox.right, pcbObj.BoundingRectangle.right);
                bbox.bottom := Min(bbox.bottom, pcbObj.BoundingRectangle.bottom);
                bbox.top    := Max(bbox.top, pcbObj.BoundingRectangle.top);
                pcbObj := objIterator.NextPCBObject();
            end;
        except
            result := false;
        end;

    finally
        aFootprint.GroupIterator_Destroy(objIterator);
    end;

    if result = true then
    begin
        // reference and value
        WriteLn(outFile, '(fp_text reference REF** (at '
             + pcbXYToStr(fpX, bbox.top + textSizeAltium) + ') (layer F.SilkS)');
        WriteLn(outFile, '(effects (font (size 1 1) (thickness 0.15)))');
        WriteLn(outFile, ')');

        WriteLn(outFile, '(fp_text value "' + escapeQuotes(footprint) + '" (at '
             + pcbXYToStr(fpX, bbox.bottom - textSizeAltium) + ') (layer F.Fab)');
        WriteLn(outFile, '(effects (font (size 1 1) (thickness 0.15)))');
        WriteLn(outFile, ')');

        WriteLn(outFile, ')');
    end;

    footprint := '';
end;


procedure processLibrary(aShowMsg : Boolean);
var
  footprint     : IPCB_LibComponent;
  pcbLib        : IPCB_Library;
  fpIterator    : IPCB_LibraryIterator;
  compReader    : ILibCompInfoReader;
  compNumber    : Integer;

  libName       : TDynamicString;
  libPath       : TString;
  libOutPath    : TString;
  fileOutPath   : TString;
  valid         : Boolean;

  logDocument   : IServer_Document;
  logPath       : TDynamicString;
begin
    pcbLib := PCBServer.GetCurrentPCBLibrary;

    if pcbLib = nil then
        Exit;

    // TODO set compNumber correctly, otherwise the progress bar does not work
    // it seems impossible for footprint converter at the moment
    {try}
        {compReader := PCBServer.CreateLibCompInfoReader(pcbLib.DocumentName);}
        {compReader.ReadAllComponentInfo();}
        {compNumber := compReader.NumComponentInfos();}
    {finally}
        {PCBServer.DestroyCompInfoReader(compReader);}
    {end;}

    // Set encoding to UTF-8
    // https://msdn.microsoft.com/en-us/library/windows/desktop/dd317756(v=vs.85).aspx
    SetMultiByteConversionCodePage(65001);

    libName := pcbLib.Board.FileName;
    libName := StringReplace(ExtractFileName(libName), '.PCBLib', '', -1);
    libPath := ExtractFileDir(pcbLib.Board.FileName);
    logList := TStringList.Create();

    if libName = '' then
    begin
        ShowMessage('Empty library name, aborting');
        Exit;
    end;

    log('Converting ' + pcbLib.Board.FileName);

    // Create a directory to store the output
    libOutPath := libPath + '\' + fixFileName(libName) + '.pretty\';

    if DirectoryExists(libOutPath) then
        RemoveDir(libOutPath);

    ForceDirectories(libOutPath);

    // Iterate through footprints in the library
    fpIterator := pcbLib.LibraryIterator_Create;
    fpIterator.SetState_FilterAll;
    {fpIterator.AddFilter_ObjectSet(MkSet(ePCBComponent));}
    ProgressInit('Converting library ' + pcbLib.Board.FileName, compNumber);

    footprint := fpIterator.FirstPCBObject;

    while footprint <> nil do
    begin
        // Create file for the converted footprint
        try
            fileOutPath := libOutPath + fixFileName(footprint.Name) + '.kicad_mod';
            AssignFile(outFile, fileOutPath);
            Rewrite(outFile);
            valid := processFootprint(footprint);
        finally
            CloseFile(outFile);
        end;

        // Do not save invalid footprints
        if not valid then
            DeleteFile(fileOutPath);

        ProgressUpdate(1);
        footprint := fpIterator.NextPCBObject;
    end;

    pcbLib.LibraryIterator_Destroy(fpIterator);

    log('Converted');
    logPath := libPath + '\' + fixFileName(libName) + '.txt';
    logList.SaveToFile(logPath);
    logList.Free();

    if SHOW_LOG then
    begin
        logDocument := Client.OpenDocument('Text', logPath);

        if logDocument <> nil then
            Client.ShowDocument(logDocument);
    end;

    ProgressFinish(0);

    if aShowMsg then
        ShowMessage('Saved in ' + libOutPath);
end;


procedure init(aDummy : Integer);
begin
    // Altium internally uses nanoinches, KiCad PCB format uses millimeters,
    // even though internally pcbnew uses nanometers
    //setScale(254, 100000000, 3, false);
    // keep ratio, with decreased numerator it is less likely to overflow
    setScale(127, 50000000, 3, false);

    textSizeAltium := scaleToAltium(1);
end;


procedure ConvertLibrary;
var
    fileOpenDialog : TFileOpenDialog;
    i : Integer;
    doc : IServerDocument;
    multipleFiles : Boolean;
begin
    if PCBServer = nil then
        exit;

    doc := nil;

    if Client.CurrentView <> nil then
        doc := Client.CurrentView.OwnerDocument;

    init(0);

    if (doc <> nil) and (UpperCase(doc.Kind) = 'PCBLIB') then
    begin
        // Process only current library
        processLibrary(true);
    end
    else
    begin
        // Display a file open dialog and pick a library to be converted
        fileOpenDialog := TFileOpenDialog.Create(nil);
        fileOpenDialog.Title := 'Select footprint libraries';
        // TODO it does not work :(
        Include(fileOpenDialog.Options, fdoAllowMultiSelect);
        Include(fileOpenDialog.Options, fdoFileMustExist);

        with fileOpenDialog.FileTypes.Add do
        begin
            DisplayName := 'Footprint libraries (*.PcbLib)';
            FileMask := '*.PcbLib';
        end;

        if fileOpenDialog.Execute() then
        begin
            multipleFiles := fileOpenDialog.Files.Count > 1;

            for i := 0 to fileOpenDialog.Files.Count - 1 do
            begin
                doc := Client.OpenDocument('PcbLib', fileOpenDialog.Files[i]);

                if doc <> nil then
                begin
                    Client.ShowDocument(doc);
                    processLibrary(not multipleFiles);
                    Client.CloseDocument(doc);
                end;
            end;
        end;

        fileOpenDialog.Free();
    end;

    if multipleFiles then
         ShowMessage('Finished!');
end;


procedure BatchConversion;
var
    fileOpenDialog : TFileOpenDialog;
    i : Integer;
    doc : IServerDocument;
    listFile : TextFile;
    listFileName : TDynamicString;
    buf : String;
begin
    init(0);

    // Display a file open dialog and pick a library to be converted
    fileOpenDialog := TFileOpenDialog.Create(nil);
    fileOpenDialog.Title := 'Select library list';
    // TODO it does not work :(
    Include(fileOpenDialog.Options, fdoFileMustExist);

    with fileOpenDialog.FileTypes.Add do
    begin
        DisplayName := 'Library list (*.txt)';
        FileMask := '*.txt';
    end;

    if fileOpenDialog.Execute() then
    begin
        listFileName := fileOpenDialog.Files[0];

        AssignFile(listFile, listFileName);
        Reset(listFile);

        while not Eof(listFile) do
        begin
            ReadLn(listFile, buf);

            if buf[0] = '#' then  // comments
                continue;

            if FileExists(buf) then
                doc := Client.OpenDocument('PcbLib', buf)
            else if FileExists(ExtractFilePath(listFileName) + buf) then
                doc := Client.OpenDocument('PcbLib', ExtractFilePath(listFileName) + buf)
            else
                continue;

            if doc <> nil then
            begin
                Client.ShowDocument(doc);
                processLibrary(false);
                Client.CloseDocument(doc);
            end;
        end;

        CloseFile(listFile);
    end;

    fileOpenDialog.Free();
    ShowMessage('Finished!');
end;
