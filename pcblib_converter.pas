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


function layerToString(aLayer : TV6_Layer) : TPCB_String;
begin                                    // TODO missing layers
    case aLayer of
        eTop:            result := 'F.Cu';
        eBottom:         result := 'B.Cu';
        eTopPaste:       result := 'F.Paste';
        eBottomPaste:    result := 'B.Paste';
        eTopOverlay:     result := 'F.SilkS';
        eBottomOverlay:  result := 'B.SilkS';
        else             result := 'Dwgs.User';
    end;
end;


procedure processText(aText : IPCB_Text);
begin
//    TextObj.XLocation := Sheet.SheetX + MilsToCoord(500);
//    TextObj.YLocation := Sheet.SheetY + MilsToCoord(500);
//    TextObj.Layer     := eTopOverlay;
//    TextObj.Text      := 'Traditional Protel Text';
//    TextObj.Size       := MilsToCoord(90);   // sets the height of the text.

// (fp_text reference R1 (at 0 0.127) (layer F.SilkS) hide
//     (effects (font (size 1.397 1.27) (thickness 0.2032)))
// )
           // TODO scale real type
    // TODO reference value
    WriteLn(outFile, '(fp_text user ' + aText.Text + ' (at '
         + IntToStr(scaleToKiCad(aText.XLocation)) + ' '
         + IntToStr(scaleToKiCad(aText.YLocation)) + ')'
         + ' (layer ' + layerToString(aText.Layer) + ')');  // TODO hide
    WriteLn(outFile, '    (effects (font (size ' + IntToStr(scaleToKiCad(aText.Size))
         + ' ' + IntToStr(scaleToKiCad(aText.Size)) + ') (thickness 0.2032)))');     // TODO thickness
    WriteLn(outFile, ')');
end;


procedure processPad(aPad : IPCB_Pad);
begin
     // (pad 1 smd rect (at -4 0 180) (size 4 2.5) (layers F.Cu F.Paste F.Mask))

//    NewPad.X        := MilsToCoord(0);
//    NewPad.Y        := MilsToCoord(0);
//    NewPad.TopXSize := MilsToCoord(62);
//    NewPad.TopYSize := MilsToCoord(62);
//    NewPad.HoleSize := MilsToCoord(28);
//    NewPad.Layer    := eMultiLayer;
//    NewPad.Name     := '1';

    // TODO smd rect
    WriteLn(outFile, '(pad ' + aPad.Name + ' smd rect (at '
         + IntToStr(scaleToKiCad(aPad.X)) + ' '
         + IntToStr(scaleToKiCad(aPad.Y)) + ') '
         + '(size ' + IntToStr(scaleToKiCad(aPad.TopXSize))
         + ' ' + IntToStr(scaleToKiCad(aPad.TopYSize)) + ') '
         + '(layers F.Cu F.Paste F.Mask))');
         // TODO layers
end;


procedure processObject(aObject : IPCB_Primitive);
begin
    case aObject.ObjectId of
        //eNoObject           : Result := 'Any Object';
        //eArcObject          : Result := 'Arc';
        ePadObject:   processPad(aObject);
        //eViaObject          : Result := 'Via';
        //eTrackObject        : Result := 'Track';
        eTextObject : processText(aObject);
        //eFillObject         : Result := 'Fill';
        //eConnectionObject   : Result := 'Connection';
        //eNetObject          : Result := 'Net';
        //eComponentObject    : Result := 'Component';
        //ePolyObject         : Result := 'Polygon';
        //eDimensionObject    : Result := 'Dimension';
        //eCoordinateObject   : Result := 'Coordinate';
        //eClassObject        : Result := 'Class';
        //eRuleObject         : Result := 'Rule';
        //eFromToObject       : Result := 'FromTo';
        //eViolationObject    : Result := 'Violation';
        //eEmbeddedObject     : Result := 'Embedded';
        //eTraceObject        : Result := 'Trace';
        //eSpareViaObject     : Result := 'Spare Via';
        //eBoardObject        : Result := 'Board';
        //eBoardOutlineObject : Result := 'Board Outline';
    end;
end;


procedure processFootprint(aFootprint : IPCB_LibComponent);
var
    objIterator : IPCB_GroupIterator;
    pcbObj      : IPCB_Primitive;
begin
    footprint := aFootprint.Name;
    objIterator := aFootprint.GroupIterator_Create();

    // TODO layer, tedit
    // TODO escape footprint name
    WriteLn(outFile, '(module ' + footprint + ' (layer F.Cu) (tedit 58AA917F)');
    WriteLn(outFile, '(descr "' + aFootprint.Description + '")');
    // TODO
    //WriteLn(outFile, '(attr smd)');

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

    try
        footprint := fpIterator.FirstPCBObject;

        while footprint <> nil do
        begin
            // Create file for the converted footprint
            AssignFile(outFile, libOutPath + fixFileName(footprint.Name) + '.kicad_mod');
            Rewrite(outFile);
            processFootprint(footprint);;
            CloseFile(outFile);

            ProgressUpdate(1);
            footprint := fpIterator.NextPCBObject;
        end;

    finally
        pcbLib.LibraryIterator_Destroy(fpIterator);
    end;

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
