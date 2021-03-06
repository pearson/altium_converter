{*
 * Altium to KiCad footprint library converter script
 *
 * Copyright (C) 2017-2020 CERN
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
  footprintObj : IPCB_LibComponent;

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

  // pad counters to determine whether a component is SMD or THT (heuristics)
  smdPadCount : Integer;
  thtPadCount : Integer;

  // current footprint courtyard graphics
  courtyard : array [0..255] of IPCB_Primitive;
  courtyardIdx : Integer;


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
        eCircleShape:
            result := ifElse(aItem.TopXSize = aItem.TopYSize, 'circle', 'roundrect (roundrect_rratio 0.5)');

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


function createTCoordPoint(x, y : Integer) : TCoordPoint;
begin
    result := TCoordPoint;
    result.x := x;
    result.y := y;
end;


function countCommonPts(aObjA, aObjB : IPCB_Primitive) : Integer;
var
    // I could not get it to work with static arrays..
    ptsA0, ptsA1, ptsB0, ptsB1 : TCoordPoint;
begin
    result := 0;

    case aObjA.ObjectId of
        eArcObject:
        begin
            ptsA0 := createTCoordPoint(aObjA.StartX, aObjA.StartY);
            ptsA1 := createTCoordPoint(aObjA.EndX, aObjA.EndY);
        end;

        eTrackObject:
        begin
            ptsA0 := createTCoordPoint(aObjA.X1, aObjA.Y1);
            ptsA1 := createTCoordPoint(aObjA.X2, aObjA.Y2);
        end;

        else
        begin
            log(footprint + ': WARNING: unhandled courtyard object type');
            exit;
        end;
    end;

    case aObjB.ObjectId of
        eArcObject:
        begin
            ptsB0 := createTCoordPoint(aObjB.StartX, aObjB.StartY);
            ptsB1 := createTCoordPoint(aObjB.EndX, aObjB.EndY);
        end;

        eTrackObject:
        begin
            ptsB0 := createTCoordPoint(aObjB.X1, aObjB.Y1);
            ptsB1 := createTCoordPoint(aObjB.X2, aObjB.Y2);
        end;

        else
        begin
            log(footprint + ': WARNING: unhandled courtyard object type');
            exit;
        end;
    end;

    if (ptsA0.x = ptsB0.x) and (ptsA0.y = ptsB0.y) then Inc(result);
    if (ptsA1.x = ptsB0.x) and (ptsA1.y = ptsB0.y) then Inc(result);
    if (ptsA1.x = ptsB1.x) and (ptsA1.y = ptsB1.y) then Inc(result);
    if (ptsA0.x = ptsB1.x) and (ptsA0.y = ptsB1.y) then Inc(result);
end;


function processCourtyard(aObject : IPCB_Primitive) : Boolean;
begin
    // TODO handle B.CrtYd?
    result := ((aObject.ObjectId = eTrackObject) or (aObject.ObjectId = eArcObject)) and (layerToStr(aObject.Layer) = 'F.CrtYd');

    if result = true then
    begin
        courtyard[courtyardIdx] := aObject;
        Inc(courtyardIdx);
    end;
end;


procedure processArc(aArc : IPCB_Arc);
var
    endPt : TLocation;
    angle : TAngle;
    isCircle : Boolean;
begin
    // handles both arcs and circles (special kind of arc)
    // (fp_arc (start 6.25 5.3) (end -6.25 5.3) [(angle 100)] (layer F.CrtYd) (width 0.05))

    if isCopperLayer(aArc.Layer) then
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
    padCache : TPadCache;
begin
    // (pad 1 smd rect (at -4 0 180) (size 4 2.5) (layers F.Cu F.Paste F.Mask))

    // determine the pad dimensions, depending on the pad stack and configuration
    if aPad.Mode = ePadMode_Simple then
    begin
        // simple pads
        width  := aPad.TopXSize;
        height := aPad.TopYSize;
    end

    else if aPad.Mode = ePadMode_LocalStack then
    begin
        // local pad stack defines top, bottom and inner layer shape and size
        if not ((aPad.TopShape = aPad.MidShape) and (aPad.MidShape = aPad.BotShape)) then
            throw(footprint + ': ERROR: complex pad stack (different pad shaps on each layer): ' + aPad.Name);

        // there is an option that in case of complex pad stack
        // will use the largest pad size
        if LARGEST_COMPLEX_PAD then
        begin
            width  := Max(aPad.TopXSize, Max(aPad.MidXSize, aPad.BotXSize));
            height := Max(aPad.TopYSize, Max(aPad.MidYSize, aPad.BotYSize));
        end

        // there is still a chance if all layers have the same shape and size
        // (basically it is a simple stack)
        else if ((aPad.TopXSize = aPad.MidXSize) and (aPad.MidXSize = aPad.BotXSize) and
          (aPad.TopYSize = aPad.MidYSize) and (aPad.MidYSize = aPad.BotYSize)) then
        begin
            width  := aPad.TopXSize;    // equal to MidXSize and BotXSize
            height := aPad.TopYSize;    // equal to MidYSize and BotYSize
        end

        else
            throw(footprint + ': ERROR: only simple pads are supported: ' + aPad.Name);
    end
    else
        throw(footprint + ': ERROR: external pad stack is not supported: ' + aPad.Name);

    padCache := aPad.GetState_Cache();
    pasteMargin := padCache.PasteMaskExpansion;
    solderMargin := padCache.SolderMaskExpansion;

    // Altium pad-to-pin mapping is case-insensitive, but KiCad is case-sensitive,
    // so convert all pad names to upper case
    Write(outFile, '(pad "' + UpperCase(aPad.Name)
        + '" ' + padTypeToStr(aPad) + ' ' + shapeToStr(aPad)
        + ' (at ' + pcbXYToStr(aPad.X, aPad.Y)
        + ifElse(aPad.Rotation <> 0, ' ' + IntToStr(aPad.Rotation), '') + ') '
        + '(size ' + XYToStr(width, height) + ') ');

    if aPad.IsSurfaceMount then
    begin
        Inc(smdPadCount);

        case aPad.Layer of
            eTopLayer:    Write(outFile, '(layers F.Cu F.Paste F.Mask)');
            eBottomLayer: Write(outFile, '(layers B.Cu B.Paste B.Mask)');
            else throw(footprint + ': ERROR: invalid layer '
                    + cLayerStrings[aPad.Layer] + ' for SMD pad ' + aPad.Name);
        end;
    end
    else
    begin
        Inc(thtPadCount);

        Write(outFile, '(drill ');

        case aPad.HoleType of
            eRoundHole:
                Write(outFile, sizeToStr(aPad.HoleSize));

            eSquareHole:
                throw(footprint + ': ERROR: square holes are not supported');

            eSlotHole:
                // KiCad does not supported rotated holes, but 90 and 270 degree

                // rotation might be handled by swapping width and height

                if ((aPad.HoleRotation = 0) or (aPad.HoleRotation = 180)) then
                    Write(outFile, 'oval ' + sizeToStr(aPad.HoleWidth) + ' ' + sizeToStr(aPad.HoleSize))
                else if ((aPad.HoleRotation = 90) or (aPad.HoleRotation = 270)) then
                    Write(outFile, 'oval ' + sizeToStr(aPad.HoleSize) + ' ' + sizeToStr(aPad.HoleWidth))
                else
                    throw(footprint + ': ERROR: rotated holes are not supported');
        end;

        // drill offset
        if (aPad.XPadOffset[aPad.Layer] <> 0) or (aPad.YPadOffset[aPad.Layer] <> 0) then
           Write(outFile, ' (offset ' + XYToStr(aPad.XPadOffset[aPad.Layer], -aPad.YPadOffset[aPad.Layer]) + ')');

        Write(outFile, ')');

        Write(outFile, ' (layers *.Cu *.Mask)');
    end;

    if padCache.SolderMaskExpansionValid = eCacheManual then
       Write(outFile, ' (solder_mask_margin ' + sizeToStr(solderMargin) + ')');

    if padCache.PasteMaskExpansionValid = eCacheManual then
       Write(outFile, ' (solder_paste_margin ' + sizeToStr(pasteMargin) + ')');

    WriteLn(outFile, ')');

    // INFO clearance, zone_connect, thermal_gap, thermal_width seem to be
    // correct only for pads placed on a board, where design rules are set
end;


procedure processVia(aVia : IPCB_Via);
var
    pad, candidate : IPCB_Pad;
    padIterator : IPCB_GroupIterator;
begin
    // stand-alone vias are not supported, but vias inside pads can be
    // emulated by additional through hole pads, and this is the most frequent
    // use case

    if aVia.Mode <> ePadMode_Simple then
        throw(footprint + ': ERROR: no support for complex stack vias');

    // iterate through all pads to be sure via is placed inside (or intersects)
    // only a single pad
    padIterator := footprintObj.GroupIterator_Create();
    padIterator.AddFilter_ObjectSet(MkSet(ePadObject));

    pad := padIterator.FirstPCBObject;
    candidate := nil;

    while pad <> nil do
    begin
        if intersects(pad.BoundingRectangle, aVia.BoundingRectangle) then
        begin
            if candidate = nil then
                candidate := pad
            else if candidate.Name <> pad.Name then
                throw(footprint + ': ERROR: no support for vias connecting pads with different names');
        end;

        pad := padIterator.NextPCBObject();
    end;

    footprintObj.GroupIterator_Destroy(padIterator);

    if candidate = nil then
        throw(footprint + ': ERROR: no support for standalone vias');

    // convert via to a through-hole pad
    WriteLn(outFile, '(pad ' + Copy(candidate.Name, 1, 4)
        + ' thru_hole circle (at ' + pcbXYToStr(aVia.X, aVia.Y) + ') '
        + '(size ' + XYToStr(aVia.Size, aVia.Size) + ') '
        + '(drill ' + sizeToStr(aVia.HoleSize) + ') '
        + '(layers *.Cu *.Mask))');
end;


procedure processTrack(aTrack : IPCB_Track);
begin
    // graphical line
    // (fp_line (start 6.25 5.3) (end -6.25 5.3) (layer F.CrtYd) (width 0.05))

    if isCopperLayer(aTrack.Layer) then
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

        if CONVERT_TTF then
            log(footprint + ': true type fonts are drawn using the stroke font')
        else
            throw(footprint + ': ERROR: true type fonts disabled');
    end;

    if isMultiline(aText.Text) then
        throw(footprint + ': ERROR: multiline texts are not supported: ' + aText.Text);

    rotation := aText.Rotation;

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


procedure processFill(aFill : IPCB_Fill);
begin
    if aFill.Rotation <> 0 then
        throw(footprint + ': ERROR: rotated fills are not supported');

    WriteLn(outFile, '(fp_poly (pts' +
         + ' (xy ' + pcbXYToStr(aFill.X1Location, aFill.Y1Location) + ')'
         + ' (xy ' + pcbXYToStr(aFill.X2Location, aFill.Y2Location) + '))'
         + ' (layer ' + layerToStr(aFill.Layer) + ')'
         + ' (width 0))');
end;


procedure processObject(aObject : IPCB_Primitive);
begin
    case aObject.ObjectId of
        eNoObject:              throw(footprint + ': ERROR: contains an invalid object (eNoObject)');
        eArcObject:             processArc(aObject);
        ePadObject:             processPad(aObject);
        eViaObject:             processVia(aObject);
        eTrackObject:           processTrack(aObject);
        eTextObject:            processText(aObject);
        eFillObject:            {processFill(aObject);} throw(footprint + ': ERROR: fills are not supported');
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


procedure writeCourtyard(aDummy : Integer);
var
    i, j, commonPts : Integer;
begin
    if courtyardIdx = 0 then
    begin
        log(footprint + ': WARNING: missing courtyard information');
        exit;
    end;

    for i := 0 to courtyardIdx - 1 do
    begin
        commonPts := 0;

        for j := 0 to courtyardIdx - 1 do
        begin
            if i = j then continue;
            commonPts := commonPts + countCommonPts(courtyard[i], courtyard[j]);
        end;

        if commonPts > 1 then
        begin
            processObject(courtyard[i]);
        end;
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
    smdPadCount := 0;
    thtPadCount := 0;
    result := true;     // assume it is ok

    // footprint bbox computed while processing children items
    // (IPCB_LibComponent::BoundingRectangle() does not work)
    bbox := TCoordRect;

    courtyardIdx := 0;
    objIterator := aFootprint.GroupIterator_Create();

    WriteLn(outFile, '(module "' + escapeQuotes(fixSpaces(footprint)) + '" (layer F.Cu) (tedit 0)');
    WriteLn(outFile, '(descr "' + escapeQuotes(aFootprint.Description) + '")');

    // TODO smd/virtual attributes, tags
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
        WriteLn(outFile, '(model "' + escapeQuotes(model) +
                '" (at (xyz 0 0 0)) (scale (xyz 1 1 1)) (rotate (xyz 0 0 0)))');

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
                // courtyard requires additional processing at a later stage
                if processCourtyard(pcbObj) = false then
                    processObject(pcbObj);

                //bbox.left   := Min(bbox.left, pcbObj.BoundingRectangle.left);
                //bbox.right  := Max(bbox.right, pcbObj.BoundingRectangle.right);
                bbox.bottom := Min(bbox.bottom, pcbObj.BoundingRectangle.bottom);
                bbox.top    := Max(bbox.top, pcbObj.BoundingRectangle.top);
                pcbObj := objIterator.NextPCBObject();
            end;

            writeCourtyard(0);
        except
            result := false;
        end;

    finally
        aFootprint.GroupIterator_Destroy(objIterator);
    end;

    if result = true then
    begin
        // SMD attribute
        if smdPadCount > thtPadCount then
            WriteLn(outFile, '(attr smd)');

        // reference and value
        WriteLn(outFile, '(fp_text reference "REF**" (at '
             + pcbXYToStr(fpX, bbox.top + textSizeAltium) + ') (layer F.SilkS)');
        WriteLn(outFile, '(effects (font (size 1 1) (thickness 0.15)))');
        WriteLn(outFile, ')');

        WriteLn(outFile, '(fp_text value "' + escapeQuotes(footprint) + '" (at '
             + pcbXYToStr(fpX, bbox.bottom - textSizeAltium) + ') (layer F.Fab)'
             + ifElse(HIDE_NAME, ' hide', ''));
        WriteLn(outFile, '(effects (font (size 1 1) (thickness 0.15)))');
        WriteLn(outFile, ')');

        WriteLn(outFile, ')');
    end;

    footprint := '';
end;


procedure processLibrary(aShowMsg : Boolean);
var
  pcbLib        : IPCB_Library;
  fpIterator    : IPCB_LibraryIterator;
  compReader    : ILibCompInfoReader;
  compNumber    : Integer;
  compCounter   : Integer;

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

    compCounter := 0;
    footprintObj := fpIterator.FirstPCBObject;

    while footprintObj <> nil do
    begin
        // Create file for the converted footprint
        try
            fileOutPath := libOutPath + fixFileName(footprintObj.Name) + '.kicad_mod';
            AssignFile(outFile, fileOutPath);
            Rewrite(outFile);
            valid := processFootprint(footprintObj);
        finally
            CloseFile(outFile);
        end;

        // Do not save invalid footprints
        if not valid then
            DeleteFile(fileOutPath)
        else
            Inc(compCounter);

        ProgressUpdate(1);
        footprintObj := fpIterator.NextPCBObject;
    end;

    pcbLib.LibraryIterator_Destroy(fpIterator);

    if compCounter = 0 then
    begin
        log('NO FOOTPRINTS CONVERTED: removed the empty directory');

        if DirectoryExists(libOutPath) then
            RemoveDir(libOutPath);
    end;

    log('Converted');
    logPath := getLogPath(pcbLib.Board.FileName);
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
    // keep the ratio with decreased numerator, it is less likely to overflow
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

            if (VarType(buf) and VarTypeMask = varNull) then
                break;

            if (Length(buf) = 0) or (buf[0] = '#') then  // comments and empty lines
                continue;

            // TODO does not work :(
            //if (PROCESS_ONLY_MODIFIED and FileAge(getLogPath(buf)) > FileAge(buf)) then
            //    continue;

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
