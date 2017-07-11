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
  outFile   : TextFile;
  // footprint origin
  fpX, fpY  : Integer;

// not possible in DelphiScript
//type
//  TDefParams = array[0..3] of TDynamicString;


function pcbXYToStr(aX : TCoord, aY : TCoord) : TDynamicString;
begin
    // inverted Y-axis
    result := XYToStr(aX - fpX, fpY - aY);
end;


function fontSize(aFontID : TFontID) : Integer;
begin
     result := PCBServer.FontManager.Size(aFontID) * 5
end;


function shapeToStr(aShape : TShape) : TDynamicString;
begin
    case aShape of
        eNoShape:               log(footprint + ': invalid shape (eNoShape)');

        eRoundedRectangular,    // TODO is it ok?
        eRoundRectShape:        result := 'roundrect';

        eRectangular:           result := 'rect';

        eOctagonal:
        begin
            log(footprint + ': octagonal shape is approximated with a round rectangle');
            result := 'roundrect';
        end;

        eRounded,
        eCircleShape:           result := 'circle';

        eArcShape:              log(footrpint + ': arc shape is not supported');
        eTerminator:            log(footprint + ': terminator shape is not supported');
        eRotatedRectShape:      log(footprint + ': rotated rectangular shape is not supported')
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
        eBottomLayer,
        eTopOverlay,
        eBottomOverlay,
        eTopPaste,
        eBottomPaste,
        eTopSolder,
        eBottomSolder:       result := true;
        else                 result := false;
    end;
end;


// TODO move layer mapping to configuration file
function layerToStr(aLayer : TLayer) : TPCB_String;
begin                                    // TODO missing layers
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

    // arbitrary mapping, may need adjustments
        eMechanical3:        result := 'Eco1.User';
        eMechanical4:        result := 'Eco2.User';
        eMechanical8:        result := 'Dwgs.User';
        eMechanical14:       result := 'Cmts.User';
        eMechanical15:       result := 'F.CrtYd';

    // unsupported layers
    // unmapped layers in KiCad that might be used here:
    // Edge.Cuts, Margin, {B}.CrtYd, {F,B}.Adhes, {F,B}.Fab
        eNoLayer,
        eInternalPlane1,
        eInternalPlane2,
        eInternalPlane3,
        eInternalPlane4,
        eInternalPlane5,
        eInternalPlane6,
        eInternalPlane7,
        eInternalPlane8,
        eInternalPlane9,
        eInternalPlane10,
        eInternalPlane11,
        eInternalPlane12,
        eInternalPlane13,
        eInternalPlane14,
        eInternalPlane15,
        eInternalPlane16,
        eDrillGuide,
        eKeepOutLayer,
        eMechanical1,
        eMechanical1,
        eMechanical5,
        eMechanical6,
        eMechanical7,
        eMechanical9,
        eMechanical10,
        eMechanical11,
        eMechanical12,
        eMechanical13,
        eMechanical16,
        eDrillDrawing,
        eMultiLayer,
        eConnectLayer,
        eBackGroundLayer,
        eDRCErrorLayer,
        eHighlightLayer,
        eGridColor1,
        eGridColor10,
        ePadHoleLayer,
        eViaHoleLayer:
        log(footprint + ': unhandled layer ' + cLayerStrings[aLayer]);
    end;
end;


procedure processArc(aArc : IPCB_Arc);
var
    layer : TDynamicString;
    endPt : TLocation;
    angle : TAngle;
    isCircle : Boolean;
begin
    // handles both arcs and circles (special kind of arc)
    // (fp_arc (start 6.25 5.3) (end -6.25 5.3) [(angle 100)] (layer F.CrtYd) (width 0.05))

    if isCopperLayer(aArc) then
    begin
        log(footprint + ': copper arcs are not supported');
        Exit;
    end;

    layer := layerToStr(aArc.Layer);
    if layer = '' then Exit;          // unknown layer

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
    width, height, offsetX, offsetY : TCoord;
begin
    // (pad 1 smd rect (at -4 0 180) (size 4 2.5) (layers F.Cu F.Paste F.Mask))

    if aPad.Mode <> ePadMode_Simple then
    begin
        // there is still a chance if all layers have the same shape and size
        // (basically simple stack)
        if not((aPad.Mode = ePadMode_LocalStack) and
          (aPad.TopShape = aPad.MidShape) and (aPad.MidShape = aPad.BotShape) and
          (aPad.TopXSize = aPad.MidXSize) and (aPad.MidXSize = aPad.BotXSize) and
          (aPad.TopYSize = aPad.MidYSize) and (aPad.MidYSize = aPad.BotYSize)) then
            log(footprint + ': only simple pads are supported: ' + aPad.Name);
    end;

    if Length(aPad.Name) > 4 then
        log(footprint + ': pad name truncated from ' + aPad.Name
            + ' to ' + Copy(aPad.Name, 1, 4));

    // in Altium holes bigger than pads are allowed, in KiCad it is not possible
    // resize pads to the hole size if they are smaller
    width := Max(aPad.HoleSize, aPad.TopXSize);
    height := Max(aPad.HoleSize, aPad.TopYSize);

    // In KiCad pad position refers to the hole, in Altium - center of the copper,
    // so if there is an offset, we need to adjust the postion, but only if there
    // is a hole
    if aPad.HoleSize <> 0 then
    begin
        offsetX := aPad.XPadOffset[aPad.Layer];
        offsetY := aPad.YPadOffset[aPad.Layer];
    end
    else
    begin
        offsetX := 0;
        offsetY := 0;
    end;

    // TODO pad name in KiCad is limited to 4 chars, spaces are allowed (check)
    Write(outFile, '(pad ' + Copy(aPad.Name, 1, 4)
    + ' ' + padTypeToStr(aPad) + ' ' + shapeToStr(aPad.TopShape)
    + ' (at ' + pcbXYToStr(aPad.X - offsetX, aPad.Y - offsetY)
     + ifElse(aPad.Rotation <> 0, ' ' + IntToStr(aPad.Rotation), '') + ') '
    + '(size ' + XYToStr(width, height) + ') ');

    if aPad.IsSurfaceMount then
    begin
        case aPad.Layer of
            eTopLayer:    WriteLn(outFile, '(layers F.Cu F.Paste F.Mask)');
            eBottomLayer: WriteLn(outFile, '(layers B.Cu B.Paste B.Mask)');
            else          log(footprint + ': invalid layer for pad ' + aPad.Name);
        end;
    end
    else
    begin
        if aPad.HoleRotation <> 0 then
            log(footprint + ': rotated holes are not supported');

        Write(outFile, '(drill ');

        case aPad.HoleType of
            eRoundHole:
                Write(outFile, sizeToStr(aPad.HoleSize));
            eSquareHole:
            begin
                log(footprint + ': square hole approximated with an oval hole');
                Write(outFile, 'oval ' + sizeToStr(aPad.HoleSize));
            end;
            eSlotHole:              // TODO incorrect?
                Write(outFile, 'oval ' + sizeToStr(aPad.HoleSize));  // TODO merge
        end;

        if (offsetX <> 0) or (offsetY <> 0) then
           Write(outFile, ' (offset ' + XYToStr(offsetX, offsetY) + ')');

        Write(outFile, ')');
        // TODO drill offset

        Write(outFile, ' (layers *.Cu *.Paste *.Mask)');
    end;

    // TODO not entirely true, only valid if 'Solder Mask Override' field is true
    if aPad.Cache.SolderMaskExpansion <> 0 then
        Write(outFile, ' (solder_mask_margin ' + sizeToStr(aPad.SolderMaskExpansion) + ')');

    // TODO not entirely true, only valid if 'Paste Mask Override' field is true
    if aPad.Cache.PasteMaskExpansion <> 0 then
        Write(outFile, ' (solder_paste_margin ' + sizeToStr(aPad.PasteMaskExpansion) + ')');

    WriteLn(outFile, ')');

    // TODO clearance, zone_connect, thermal_gap, thermal_width
    // zone_connect -> PlaneConenctionStyleForLayer?
end;



procedure processTrack(aTrack : IPCB_Track);
var
    layer : TDynamicString;
begin
    // graphical line
    // (fp_line (start 6.25 5.3) (end -6.25 5.3) (layer F.CrtYd) (width 0.05))

    if isCopperLayer(aTrack) then
    begin
        log(footprint + ': copper tracks are not supported');
        Exit;
    end;

    layer := layerToStr(aTrack.Layer);
    if layer = '' then Exit;          // unknown layer

    WriteLn(outFile, '(fp_line '
       + '(start ' + pcbXYToStr(aTrack.X1, aTrack.Y1) + ') '
       + '(end ' + pcbXYToStr(aTrack.X2, aTrack.Y2) + ') '
       + '(layer ' + layerToStr(aTrack.Layer) + ') '
       + '(width ' + sizeToStr(aTrack.Width) + '))');
end;


procedure processText(aText : IPCB_Text);
begin
    // (fp_text reference R1 (at 0 0.127) (layer F.SilkS) hide
    //     (effects (font (size 1.397 1.27) (thickness 0.2032)))
    // )

    // TODO scale real type
    // TODO reference value
    WriteLn(outFile, '(fp_text user ' + aText.Text + ' (at '
         + pcbXYToStr(aText.XLocation, aText.YLocation) + ')'
         + ' (layer ' + layerToStr(aText.Layer) + ')');  // TODO hide
    WriteLn(outFile, '    (effects (font (size ' + sizeToStr(aText.Size)
         + ' ' + sizeToStr(aText.Size) + ')'
         + ' (thickness ' + sizeToStr(aText.Width) + ')))');
    WriteLn(outFile, ')');
end;


procedure processObject(aObject : IPCB_Primitive);
begin
    case aObject.ObjectId of
        eNoObject:              log(footprint + ': contains an invalid object (eNoObject)');
        eArcObject:             processArc(aObject);
        ePadObject:             processPad(aObject);
        eViaObject:             log(footprint + ': vias are not supported');
        eTrackObject:           processTrack(aObject);
        eTextObject:            processText(aObject);
        eFillObject:            log(footprint + ': fills are not supported');
        eConnectionObject:      log(footprint + ': connections are not supported');
        eNetObject:             log(footprint + ': nets are not supported');
        eComponentObject:       log(footprint + ': components are not supported');
        ePolyObject:            log(footprint + ': polys are not supported');
        eDimensionObject:       log(footprint + ': dimensions are not supported');
        eCoordinateObject:      log(footprint + ': coordinates are not supported');
        eClassObject:           log(footprint + ': classes are not supported');
        eRuleObject:            log(footprint + ': rules are not supported');
        eFromToObject:          log(footprint + ': fromtos are not supported');
        eViolationObject:       log(footprint + ': violations are not supported');
        eEmbeddedObject:        log(footprint + ': embedded objects are not supported');
        eTraceObject:           log(footprint + ': traces are not supported');
        eSpareViaObject:        log(footprint + ': spare vias are not supported');
        eBoardObject:           log(footprint + ': boards are not supported');
        eBoardOutlineObject:    log(footprint + ': board outlines are not supported');
    end;
end;


procedure processFootprint(aFootprint : IPCB_LibComponent);
var
    objIterator : IPCB_GroupIterator;
    pcbObj      : IPCB_Primitive;
begin
    // TODO 3d model
    footprint := aFootprint.Name;
    fpX := aFootprint.X;
    fpY := aFootprint.Y;

    objIterator := aFootprint.GroupIterator_Create();

    WriteLn(outFile, '(module ' + fixSpaces(footprint)
         + ' (layer F.Cu) (tedit 0)');
    WriteLn(outFile, '(descr "' + escapeQuotes(aFootprint.Description) + '")');
    // TODO smd/virtual
    //WriteLn(outFile, '(attr smd)');
    //WriteLn(outFile, '(tags xxx)');

    if aFootprint.SolderMaskExpansion <> 0 then
        WriteLn(outFile, '(solder_mask_margin '
            + sizeToStr(aFootprint.SolderMaskExpansion) + ')');

    if aFootprint.PasteMaskExpansion <> 0 then
        WriteLn(outFile, '(solder_paste_margin '
            + sizeToStr(aFootprint.PasteMaskExpansion) + ')');

    // TODO
    // WriteLn(outFile, '(clearance dim)');
    // WriteLn(outFile, '(zone_connect val');
    // WriteLn(outFile, '(thermal_width val)');
    // WriteLn(outFile, '(thermal_gap val)');

    try
        pcbObj := objIterator.FirstPCBObject;

        while pcbObj <> nil do
        begin
            processObject(pcbObj);
            pcbObj := objIterator.NextPCBObject();
        end;

    finally
        aFootprint.GroupIterator_Destroy(objIterator);
    end;

    WriteLn(outFile, ')');

    footprint := '';
end;


procedure processLibrary(aDummy : Integer);
var
  footprint     : IPCB_LibComponent;
  pcbLib        : IPCB_Library;
  fpIterator    : IPCB_LibraryIterator;
  compReader    : ILibCompInfoReader;
  compNumber    : Integer;

  libName       : TDynamicString;
  libPath       : TString;
  libOutPath    : TString;
begin
    pcbLib := PCBServer.GetCurrentPCBLibrary;

    if pcbLib = nil then
        Exit;

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
            AssignFile(outFile, libOutPath + fixFileName(footprint.Name) + '.kicad_mod');
            Rewrite(outFile);
            processFootprint(footprint);;
        finally
            CloseFile(outFile);
        end;

        ProgressUpdate(1);
        footprint := fpIterator.NextPCBObject;
    end;

    pcbLib.LibraryIterator_Destroy(fpIterator);

    log('Converted');
    logList.SaveToFile(libPath + '\' + fixFileName(libName) + '.txt');
    logList.Free();

    ProgressFinish(0);
    ShowMessage('Saved in ' + libOutPath);
end;


procedure ConvertLibrary;
var
    fileOpenDialog : TFileOpenDialog;
    i : Integer;
    {Files : TStringList;}
    doc : IServerDocument;
begin
    if PCBServer = nil then
        exit;

    doc := nil;

    if Client.CurrentView <> nil then
        doc := Client.CurrentView.OwnerDocument;

    // Altium internally uses nanoinches, KiCad PCB format uses millimeters,
    // even though internally pcbnew uses nanometers
    //setScale(254, 100000000, 3, false);
    // keep ratio, with decreased numerator it is less likely to overflow
    setScale(127, 50000000, 3, false);

    if (doc <> nil) and (UpperCase(doc.Kind) = 'PCBLIB') then
    begin
        // Process only current library
        processLibrary(0);
    end
    else
    begin
        // Display a file open dialog and pick a library to be converted
        fileOpenDialog := TFileOpenDialog.Create(nil);
        fileOpenDialog.Title := 'Select footprint libraries';

        if fileOpenDialog.Execute() then
        begin
            for i := 0 to fileOpenDialog.Files.Count - 1 do
            begin
                doc := Client.OpenDocument('PcbLib', fileOpenDialog.Files[i]);

                if doc <> nil then
                begin
                    Client.ShowDocument(doc);
                    processLibrary(0);
                    Client.CloseDocument(doc);
                end;
            end;
        end;

        fileOpenDialog.Free();
    end;
end;

//  TODO processFolder
