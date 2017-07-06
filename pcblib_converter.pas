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
  logList   : TStringList;
  // converted footprint name, used for logging
  footprint : String;
  outFile   : TextFile;

const
  // default parameter text height
  PARAM_TEXT_SIZE = 60;
  // factor to convert coordinates from KiCad to Altium
  SCALE_KI_TO_ALT = 10000;

// not possible in DelphiScript
//type
//  TDefParams = array[0..3] of TDynamicString;

procedure log(aMessage : TDynamicString);
begin
    logList.Append(DateToStr(Date) + ' ' + timeToStr(Time) + ': ' + aMessage);
end;


function escapeLabel(aText : TDynamicString) : TDynamicString;
begin
    result := StringReplace(aText, '"', '''''', -1);
end;


function ifElse(aCondition : Boolean; aStringTrue : TDynamicString;
    aStringFalse : TDynamicString) : TDynamicString;
begin
    if aCondition then
        result := aStringTrue
    else
        result := aStringFalse;
end;


function scaleToKiCad(aCoord : Integer) : Integer;
begin
    result := aCoord / SCALE_KI_TO_ALT;
end;


function scaleToAltium(aCoord : Integer) : Integer;
begin
    result := aCoord * SCALE_KI_TO_ALT;
end;


function partMode(aObject : IPCB_GraphicalObject) : TDynamicString;
begin
    // Currently only two modes are supported by KiCad
    result := ifElse(partCount > 1, IntToStr(aObject.OwnerPartId), '0')
            + ' ' + ifElse(modeCount = 2, IntToStr(aObject.OwnerPartDisplayMode + 1), '0');
end;


function convertTSize(aSize : TSize) : Integer;
begin
    case aSize of
        //eSmallest: result := 0;
        eSmall:    result := 10;
        eMedium:   result := 20;
        eLarge:    result := 30;
        else       result := 0;
    end;
end;


function isDark(aColor : TColor) : Boolean;
var
    brightness : Integer;
begin
    brightness := (aColor and $FF)
                + ((aColor shr 8) and $FF)
                + ((aColor shr 16) and $FF);

    result := (brightness < 128 * 3);
end;


function fillToStr(aFilled : Boolean; aColor : TColor) : Char;
begin
    if not aFilled then
        result := 'N'
    else
        result := ifElse(isDark(aColor), 'F', 'f');
end;


function fillObjToStr(aObject : IPCB_GraphicalObject) : Char;
begin
    result := fillToStr(aObject.IsSolid, aObject.AreaColor)
end;


function rotToStr(aRotation : TRotationBy90) : Char;
begin
    case aRotation of
        eRotate0:   result := 'L';
        eRotate90:  result := 'D';
        eRotate180: result := 'R';
        eRotate270: result := 'U';
    end;
end;


function rotToInt(aRotation : TRotationBy90) : Integer;
begin
    case aRotation of
        eRotate0:   result := 0;
        eRotate90:  result := 900;
        eRotate180: result := 1800;
        eRotate270: result := 2700;
    end;
end;


function rotToInt90(aRotation : TRotationBy90) : Integer;
begin
    case aRotation of
        eRotate0:   result := 0;
        eRotate90:  result := 900;
        eRotate180: result := 0;
        eRotate270: result := 900;
    end;
end;


function rotToOrient(aRotation : TRotationBy90) : Char;
begin
    case aRotation of
        eRotate0:   result := 'H';
        eRotate90:  result := 'V';
        eRotate180: result := 'H';
        eRotate270: result := 'V';
    end;
end;


function arcStartPt(aArc : IPCB_Arc) : TLocation;
begin
   result := TLocation;
   result.x := aArc.Location.x + aArc.Radius * Cos(aArc.StartAngle * PI / 180.0);
   result.y := aArc.Location.y + aArc.Radius * Sin(aArc.StartAngle * PI / 180.0);
end;


function arcEndPt(aArc : IPCB_Arc) : TLocation;
begin
   result := TLocation;
   result.x := aArc.Location.x + aArc.Radius * Cos(aArc.EndAngle * PI / 180.0);
   result.y := aArc.Location.y + aArc.Radius * Sin(aArc.EndAngle * PI / 180.0);
end;


function locToStr(aLocation : TLocation) : TDynamicString;
begin
    result := IntToStr(scaleToKiCad(aLocation.x)) + ' ' + IntToStr(scaleToKiCad(aLocation.y));
end;


function fontSize(aFontID : TFontID) : Integer;
begin
     result := PCBServer.FontManager.Size(aFontID) * 5
end;


function justToStr(aJustification : TTextJustification) : TDynamicString;
begin
    case aJustification of
        eJustify_BottomLeft:    result := 'L B';
        eJustify_BottomCenter:  result := 'C B';
        eJustify_BottomRight:   result := 'R B';
        eJustify_CenterLeft:    result := 'L C';
        eJustify_Center:        result := 'C C';
        eJustify_CenterRight:   result := 'R C';
        eJustify_TopLeft:       result := 'L T';
        eJustify_TopCenter:     result := 'C T';
        eJustify_TopRight:      result := 'R T';
    end;
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


function fixName(aName : TDynamicString) : TDynamicString;
var
    i, len  : Integer;
    overbar : Boolean;
begin
    result := aName;

    len := Length(result);
    overbar := false;
    i := 1;

    while i <= len do
    //for i := 1 to len do
    begin
        // Spaces are not allowed in symbol names in KiCad
        if result[i] = ' ' then
            result[i] := '_'

        // In KiCad tilda toggles overbar, so we need another one to escape
        else if result[i] = '~' then
        begin
            Insert('~', result, i);
            Inc(len);
            Inc(i);
        end

        // Altium does not display backslashes. It is a modifier to enable
        // overbar for the previous character, but apparently there is no way
        // to escape this character
        else if result[i] = '\' then
        begin
            Delete(result, i, 1);
            Dec(len);
            Dec(i);
        end;


        // Handle overbars
        if i < len then
        begin
            // Detect overbar start
            if not overbar and (result[i + 1] = '\') then
            begin
                // Enable overbar
                overbar := true;
                result[i + 1] := result[i];
                result[i] := '~';
                Inc(i);   // this character has been already processed
            end

            else if overbar then
            begin
                if result[i + 1] <> '\' then
                begin
                    // Disable overbar
                    overbar := false;
                    Insert('~', result, i + 1);
                    Inc(len);
                    Inc(i);
                end
                else
                begin
                    // Overbar continues, just remove the backslash
                    Delete(result, i + 1, 1);
                    Dec(len);
                end
            end;
        end;


        Inc(i);
    end;
end;


function fixFileName(aName : TDynamicString) : TDynamicString;
var
    i : Integer;
const
    forbiddenChars = '<>:"\\/|?*';
begin
    result := aName;

    for i := 0 to Length(forbiddenChars) - 1 do
    begin
        result := StringReplace(result, forbiddenChars[i], '', -1);
    end;
end;


function layerToString(aLayer : TLayer) : TPCB_String;
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
        eMechanical1:        result := 'Dwgs.User';
        eMechanical2:        result := 'Cmts.User';
        eMechanical3:        result := 'Eco1.User';
        eMechanical4:        result := 'Eco2.User';

    // unsupported layers
    // unmapped layers in KiCad that might be used here:
    // Edge.Cuts, Margin, {F,B}.CrtYd, {F,B}.Adhes, {F,B}.Fab
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
        eMechanical5,
        eMechanical6,
        eMechanical7,
        eMechanical8,
        eMechanical9,
        eMechanical10,
        eMechanical11,
        eMechanical12,
        eMechanical13,
        eMechanical14,
        eMechanical15,
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
        log(component + ': unknown layer');
    end;
end;


procedure processText(aText : IPCB_Text);
begin
    // (fp_text reference R1 (at 0 0.127) (layer F.SilkS) hide
    //     (effects (font (size 1.397 1.27) (thickness 0.2032)))
    // )

    if aText.Width.X <> aText.Width.Y then
        log(footprint + ': text width has to be the same for X and Y axes');

    // TODO scale real type
    // TODO reference value
    WriteLn(outFile, '(fp_text user ' + aText.Text + ' (at '
         + IntToStr(scaleToKiCad(aText.XLocation)) + ' '
         + IntToStr(scaleToKiCad(aText.YLocation)) + ')'
         + ' (layer ' + layerToString(aText.Layer) + ')');  // TODO hide
    WriteLn(outFile, '    (effects (font (size ' + IntToStr(scaleToKiCad(aText.Size))
         + ' ' + IntToStr(scaleToKiCad(aText.Size)) + ')'
         + ' (thickness ' + IntToStr(scaleToKiCad(aText.Width.X)) + ')))');
    WriteLn(outFile, ')');
end;


procedure processPad(aPad : IPCB_Pad);
begin
    // (pad 1 smd rect (at -4 0 180) (size 4 2.5) (layers F.Cu F.Paste F.Mask))

    if aPad.Mode <> ePadMode_Simple then
        log(footprint + ': only simple pads are supported - ' + aPad.Name);

    Write(outFile, '(pad ' + aPad.Name
    + ' ' + padTypeToStr(aPad)
    + ' ' + shapeToStr(aPad.TopShape)
    + ' (at ' + IntToStr(scaleToKiCad(aPad.X)) + ' '
    +       IntToStr(scaleToKiCad(aPad.Y)) + ') '
    + '(size ' + IntToStr(scaleToKiCad(aPad.TopXSize)) + ' '
    +       IntToStr(scaleToKiCad(aPad.TopYSize)) + ')');

    // TODO layers
    if aPad.IsSurfaceMount then
    begin
        WriteLn(outFile, '(layers F.Cu F.Paste F.Mask))');
    end
    else
    begin
        if aPad.HoleRotation <> 0 then
            log(footprint + ': rotated holes are not supported');

        Write(outFile, '(drill ');

        case aPad.HoleType of
            eRoundHole:
                Write(outFile, IntToStr(scaleToKiCad(aPad.HoleSize)));
            eSquareHole:
            begin
                log(footprint + ': square hole approximated with an oval hole');
                Write(outFile, 'oval ' + locToStr(aPad.HoleSize));
            end;
            eSlotHole:
                Write(outFile, 'oval ' + locToStr(aPad.HoleSize));  // TODO merge
        end;
        Write(outFile, ')');
        // TODO drill offset

        WriteLn(outFile, ' (layers *.Cu *.Paste *.Mask))')
    end;

    // TODO clearance, zone_connect, solder_paste/mask_margin, thermal_gap, thermal_width
    // zone_connect -> PlaneConenctionStyleForLayer?
end;


procedure processObject(aObject : IPCB_Primitive);
begin
    case aObject.ObjectId of
        eNoObject:              log(footprint + ': contains an invalid object (eNoObject)');
        eArcObject:             log(footprint + ': arcs are not supported');
        ePadObject:             processPad(aObject);
        eViaObject:             log(footprint + ': vias are not supported');
        eTrackObject:           log(footprint + ': tracks are not supported');
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
    objIterator := aFootprint.GroupIterator_Create();

    // TODO escape footprint name
    WriteLn(outFile, '(module ' + StringReplace(footprint, ' ', '_', -1)
         + ' (layer F.Cu) (tedit 0)');
    WriteLn(outFile, '(descr "' + aFootprint.Description + '")');
    // TODO smd/virtual
    //WriteLn(outFile, '(attr smd)');
    //WriteLn(outFile, '(tags xxx)');

    WriteLn(outFile, '(solder_mask_margin '
        + IntToStr(scaleToKiCad(aFootprint.SolderMaskExpansion)) + ')');

    WriteLn(outFile, '(solder_paste_margin '
        + IntToStr(scaleToKiCad(aFootprint.PasteMaskExpansion)) + ')');

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
    libOutPath := ExtractFileDir(pcbLib.Board.FileName) + '\';
    logList := TStringList.Create();

    if libName = '' then
    begin
        ShowMessage('Empty library name, aborting');
        Exit;
    end;

    log('Converting ' + pcbLib.Board.FileName);

    // Create a directory to store the output
    libOutPath := libOutPath + fixFileName(libName) + '.pretty\';

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
    logList.SaveToFile(libOutPath + fixFileName(libName) + '.txt');
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

    if (doc <> nil) and (UpperCase(doc.Kind) = 'PCBLIB') then
    begin
        // Process only current library
        processLibrary(0);
    end;
    { TODO file browser}
end;


{
(module CP_Elec_10x10.5 (layer F.Cu) (tedit 58AA917F)
  (descr "SMT capacitor, aluminium electrolytic, 10x10.5")
  (attr smd)
  (fp_text reference REF** (at 0 6.46) (layer F.SilkS)
    (effects (font (size 1 1) (thickness 0.15)))
  )
  (fp_text value CP_Elec_10x10.5 (at 0 -6.46) (layer F.Fab)
    (effects (font (size 1 1) (thickness 0.15)))
  )
  (fp_line (start 6.25 5.3) (end -6.25 5.3) (layer F.CrtYd) (width 0.05))
  (fp_line (start -5.21 -4.45) (end -5.21 -1.56) (layer F.SilkS) (width 0.12))
  (fp_text user %R (at 0 6.46) (layer F.Fab)
    (effects (font (size 1 1) (thickness 0.15)))
  )
  (fp_text user + (at -5.78 4.97) (layer F.SilkS)
    (effects (font (size 1 1) (thickness 0.15)))
  )
  (fp_text user + (at -2.91 -0.08) (layer F.Fab)
    (effects (font (size 1 1) (thickness 0.15)))
  )
  (fp_circle (center 0 0) (end 0 5) (layer F.Fab) (width 0.1))
  (pad 2 smd rect (at 4 0 180) (size 4 2.5) (layers F.Cu F.Paste F.Mask))
  (pad 1 smd rect (at -4 0 180) (size 4 2.5) (layers F.Cu F.Paste F.Mask))
  (model Capacitors_SMD.3dshapes/CP_Elec_10x10.5.wrl
    (at (xyz 0 0 0))
    (scale (xyz 1 1 1))
    (rotate (xyz 0 0 180))
  )
)
}
