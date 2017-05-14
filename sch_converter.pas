{*
 * Altium to KiCad schematic library converter script
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
  logList : TStringList;
  component : String;
  outFile : TextFile;

procedure log(aMessage : TDynamicString);
begin
    logList.Append(DateToStr(Date) + ' ' + timeToStr(Date) + ': ' + aMessage);
end;


function scale(RelCoord : Real) : Real;
begin
    result := RelCoord * 0.0001;
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


function rotToStr(aOrientation : TRotationBy90) : TDynamicString;
begin
    case aOrientation of
        eRotate0:   result := 'L';
        eRotate90:  result := 'D';
        eRotate180: result := 'R';
        eRotate270: result := 'U';
    end;
end;


function rotToInt(aOrientation : TRotationBy90) : Integer;
begin
    case aOrientation of
        eRotate0:   result := 0;
        eRotate90:  result := 900;
        eRotate180: result := 1800;
        eRotate270: result := 2700;
    end;
end;


function rotToInt90(aOrientation : TRotationBy90) : Integer;
begin
    case aOrientation of
        eRotate0:   result := 0;
        eRotate90:  result := 900;
        eRotate180: result := 0;
        eRotate270: result := 900;
    end;
end;


function arcStartPt(aArc : ISch_Arc) : TLocation;
begin
   result := TLocation;
   result.x := aArc.Location.x + aArc.Radius * Cos(aArc.StartAngle / 180 * PI);
   result.y := aArc.Location.y + aArc.Radius * Sin(aArc.StartAngle / 180 * PI);
end;


function arcEndPt(aArc : ISch_Arc) : TLocation;
begin
   result := TLocation;
   result.x := aArc.Location.x + aArc.Radius * Cos(aArc.EndAngle / 180 * PI);
   result.y := aArc.Location.y + aArc.Radius * Sin(aArc.EndAngle / 180 * PI);
end;


function locToStr(aLocation : TLocation) : TDynamicString;
begin
    result := IntToStr(scale(aLocation.x)) + ' ' + IntToStr(scale(aLocation.y));
end;


function fontSize(aFontID : TFontID) : Integer;
begin
     result := SchServer.FontManager.Size(aFontID) * 5
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


procedure addLibHeader(aFile : TextFile);
begin
    WriteLn(aFile, 'EESchema-LIBRARY Version 2.3');
    WriteLn(aFile, '#encoding utf-8');
end;


procedure addLibFooter(aFile : TextFile);
begin
    WriteLn(aFile, '#');
    WriteLn(aFile, '#End Library');
end;


procedure processParameter(aParameter : ISch_Parameter; aParamNr : Integer;
    aTemplate : Boolean; aParams : TStringList);
var
    value, buf  : TDynamicString;
    i, paramIdx : Integer;
begin
    value := aParameter.Text;

    // Correct default field numbers
    if aParameter.Name = 'Designator' then
    begin
        aParamNr := 0;
        value := StringReplace(aParameter.Text, '?', '', -1);
    end
    else if aParameter.Name = 'Value' then
        aParamNr := 1
    else if aParameter.Name = 'Footprint' then
        aParamNr := 2       // TODO use ISch_Implementation to figure out the footprint?
    else if aParameter.Name = 'HelpURL' then
        aParamNr := 3;

    if aTemplate then
    begin
        if aParamNr = 2 then
            value := '${Library Name}:${Footprint Ref}'
        else
            value := '${' + aParameter.Name + '}';
    end
    else
    begin
        // Escape quotes
        value := StringReplace(value, '"', '\"', -1);
    end;

    buf := 'F' + IntToStr(aParamNr) + ' "' + value + '" ' + locToStr(aParameter.Location)
        + ' ' + IntToStr(fontSize(aParameter.FontID))
        + ' H I L CNN';      // TODO hardcoded justification/orientation/etc.

    // Default fields do not store the field name at the end
    if aParamNr >= 4 then
        buf := buf + ' "' + aParameter.Name + '"';

    // Find the right place to insert the parameter
    for i := 0 to aParams.Count() - 1 do
    begin
        // Extract the field number for i-th string in the output list
        paramIdx := StrToInt(Copy(aParams[i], 2, Pos(' ', aParams[i]) - 2));

        if paramIdx > aParamNr then
            break;
    end;

    aParams.Insert(i, buf);
end;


procedure processPoly(aPoly : ISch_Polygon; aFilled : Boolean;
                      aCloseLine : Boolean);
var
    i, count : Integer;
    buf      : TDynamicString;
begin
    // P Nb parts convert thickness x0 y0 x1 y1 xi yi cc

    count := aPoly.VerticesCount;
    if aCloseLine then Inc(count);

    buf := 'P ' + IntToStr(count) + ' ' + IntToStr(aPoly.OwnerPartId)
                + ' 0 ' + IntToStr(convertTSize(aPoly.LineWidth));

    for i := 1 to aPoly.VerticesCount do
        buf := buf + ' ' + locToStr(aPoly.Vertex[i]);

    if aCloseLine then buf := buf + ' ' + locToStr(aPoly.Vertex[1]);

    if aFilled then
        buf := buf + ' f'
    else
        buf := buf + ' N';

    WriteLn(outFile, buf);
end;


procedure processPin(aPin : ISch_Pin);
var
    pos         : TLocation;
    pinShapeSet : Boolean;
    buf         : TDynamicString;
begin
    // X name number posx posy length orientation Snum Snom unit convert Etype [shape]

    // Correct the pin position
    pos := aPin.Location;

    case aPin.Orientation of
        eRotate0:   pos.x := aPin.Location.x + aPin.PinLength;  // left
        eRotate90:  pos.y := aPin.Location.y + aPin.PinLength;  // down
        eRotate180: pos.x := aPin.Location.x - aPin.PinLength;  // right
        eRotate270: pos.y := aPin.Location.y - aPin.PinLength;  // up
    end;

    buf :='X ' + fixName(aPin.Name) + ' ' + fixName(aPin.Designator)
            + ' ' + locToStr(pos) + ' ' + IntToStr(scale(aPin.PinLength))
            + ' ' + rotToStr(aPin.Orientation);

    // TODO text label size?
    buf := buf + ' 50 50 ' + IntToStr(aPin.OwnerPartId) + ' 0';

    case aPin.Electrical of
        eElectricInput:            buf := buf + ' I';
        eElectricIO:               buf := buf + ' B';
        eElectricOutput:           buf := buf + ' O';
        eElectricOpenCollector:    buf := buf + ' C';
        eElectricPassive:          buf := buf + ' P';
        eElectricHiZ:              buf := buf + ' T';
        eElectricOpenEmitter:      buf := buf + ' E';
        // There is no power input/output distinction in Altium, so there
        // is simple heuristics trying to guess the type
        eElectricPower:
           if AnsiPos('out', Lowercase(aPin.Name)) > 0 then
                buf := buf + ' w'
            else
                buf := buf + ' W';
    end;

    // Pin shape
    // TODO pin shape also depends on the electrical type?
    pinShapeSet := false;

    {if not pinShapeSet then begin
        // Assume the pin shape is set, it will be reverted in the default case handler
        pinShapeSet := true;
        case aPin.Symbol_Inner of
            ePostPonedOutput:
            eOpenCollector:
            eHiZ:
            eHighCurrent:
            ePulse:
            eSchmitt:
            eOpenCollectorPullUp:
            eOpenEmitter:
            eOpenEmitterPullUp:
            eShiftLeft:
            eOpenCircuitOutput:
            else pinShapeSet := false;
        end;
    end;}

    if not pinShapeSet then
    begin
        // Assume the pin shape is set, it will be reverted in the default case handler
        pinShapeSet := true;
        case aPin.Symbol_InnerEdge of
            eClock:               buf := buf + ' C';
            else                  pinShapeSet := false;
        end;
    end;

    if not pinShapeSet then
    begin
        // Assume the pin shape is set, it will be reverted in the default case handler
        pinShapeSet := true;
        case aPin.Symbol_OuterEdge of
            eDot:                 buf := buf + ' I';
            eActiveLowInput:      buf := buf + ' L';
            eActiveLowOutput:     buf := buf + ' V';
            else                  pinShapeSet := false;
        end;
    end;

    {if not pinShapeSet then
    begin
        // Assume the pin shape is set, it will be reverted in the default case handler
        pinShapeSet := true;
        case aPin.Symbol_Outer of
        //eRightLeftSignalFlow:
        //eAnalogSignalIn:
        //eNotLogicConnection:
        //eDigitalSignalIn:
        //eLeftRightSignalFlow:
        //eBidirectionalSignalFlow:
        //else pinShapeSet := false;
        end;
    end;}

    WriteLn(outFile, buf);
end;


procedure processRectangle(aRect : ISch_Rectangle);
var
    buf : TDynamicString;
begin
    // S startx starty endx endy unit convert thickness cc

    buf := 'S ' + locToStr(aRect.Location) + ' ' + locToStr(aRect.Corner)
                + ' ' + IntToStr(aRect.OwnerPartId)
                + ' 0 ' + IntToStr(convertTSize(aRect.LineWidth));

    if aRect.IsSolid() then buf := buf + ' f' else buf := buf + ' N';

    WriteLn(outFile, buf);
end;


procedure processLine(aLine : ISch_Line);
begin
    // P Nb parts convert thickness x0 y0 x1 y1 xi yi cc

    WriteLn(outFile, 'P 2 ' + IntToStr(aLine.OwnerPartId) + ' 0 ' + IntToStr(convertTSize(aLine.LineWidth))
              + ' ' + locToStr(aLine.Location) + ' ' + locToStr(aLine.Corner) + ' N');
end;


procedure processArc(aArc : ISch_Arc; aFilled : Boolean);
var
    buf : TDynamicString;
begin
    // A posx posy radius start end part convert thickness cc start_pointX start_pointY end_pointX end_pointY

    buf := 'A ' + locToStr(aArc.Location) + ' ' + IntToStr(scale(aArc.Radius))
                + ' ' + IntToStr(aArc.StartAngle * 10) + ' ' + IntToStr(aArc.EndAngle * 10)
                + ' ' + IntToStr(aArc.OwnerPartId) + ' 0 '
                + IntToStr(convertTSize(aArc.LineWidth));

    if aFilled then buf := buf + ' f ' else buf := buf + ' N ';

    buf := buf + locToStr(arcStartPt(aArc)) + ' ' + locToStr(arcEndPt(aArc));

    WriteLn(outFile, buf);
end;


procedure processRoundRect(aRoundRect : ISch_RoundRectangle);
var
    startX, endX, startY, endY, radius : Integer;
begin
    // KiCad does not have round rectangles, so draw four lines and four arcs

    startX  := Min(aRoundRect.Location.x, aRoundRect.Corner.x);
    endX    := Max(aRoundRect.Location.x, aRoundRect.Corner.x);
    startY  := Min(aRoundRect.Location.y, aRoundRect.Corner.y);
    endY    := Max(aRoundRect.Location.y, aRoundRect.Corner.y);
    radius  := (aRoundRect.CornerXRadius + aRoundRect.CornerYRadius) / 2;

    if aRoundRect.CornerXRadius <> aRoundRect.CornerYRadius then
       log(component + ': has rounded rectangle with different X & Y corner radius, not supported');

    if aRoundRect.IsSolid then
       log(component + ': filled rounded rectangles are converted as stroked ones');

    // left edge
    WriteLn(outFile, 'P 2 ' + IntToStr(aRoundRect.OwnerPartId)
        + ' 0 ' + IntToStr(convertTSize(aRoundRect.LineWidth))
        + ' ' + IntToStr(scale(startX)) + ' ' + IntToStr(scale(startY + radius))
        + ' ' + IntToStr(scale(startX)) + ' ' + IntToStr(scale(endY - radius)) + ' N');

    // bottom edge
    WriteLn(outFile, 'P 2 ' + IntToStr(aRoundRect.OwnerPartId)
        + ' 0 ' + IntToStr(convertTSize(aRoundRect.LineWidth))
        + ' ' + IntToStr(scale(startX + radius)) + ' ' + IntToStr(scale(endY))
        + ' ' + IntToStr(scale(endX - radius)) + ' ' + IntToStr(scale(endY)) + ' N');

    // right edge
    WriteLn(outFile, 'P 2 ' + IntToStr(aRoundRect.OwnerPartId)
        + ' 0 ' + IntToStr(convertTSize(aRoundRect.LineWidth))
        + ' ' + IntToStr(scale(endX)) + ' ' + IntToStr(scale(startY + radius))
        + ' ' + IntToStr(scale(endX)) + ' ' + IntToStr(scale(endY - radius)) + ' N');

    // top edge
    WriteLn(outFile, 'P 2 ' + IntToStr(aRoundRect.OwnerPartId)
        + ' 0 ' + IntToStr(convertTSize(aRoundRect.LineWidth))
        + ' ' + IntToStr(scale(startX + radius)) + ' ' + IntToStr(scale(startY))
        + ' ' + IntToStr(scale(endX - radius)) + ' ' + IntToStr(scale(startY)) + ' N');

    // top left corner
    WriteLn(outFile, 'A ' + IntToStr(scale(startX + radius)) + ' ' + IntToStr(scale(endY - radius))
                + ' ' + IntToStr(scale(radius)) + ' 900 1800 '
                + IntToStr(aRoundRect.OwnerPartId) + ' 0 '
                + IntToStr(convertTSize(aRoundRect.LineWidth)));

    // bottom left corner
    WriteLn(outFile, 'A ' + IntToStr(scale(startX + radius)) + ' ' + IntToStr(scale(startY + radius))
                + ' ' + IntToStr(scale(radius)) + ' -900 1800 '
                + IntToStr(aRoundRect.OwnerPartId) + ' 0 '
                + IntToStr(convertTSize(aRoundRect.LineWidth)));

    // top right corner
    WriteLn(outFile, 'A ' + IntToStr(scale(endX - radius)) + ' ' + IntToStr(scale(endY - radius))
                + ' ' + IntToStr(scale(radius)) + ' 900 0 '
                + IntToStr(aRoundRect.OwnerPartId) + ' 0 '
                + IntToStr(convertTSize(aRoundRect.LineWidth)));

    // bottom right corner
    WriteLn(outFile, 'A ' + IntToStr(scale(endX - radius)) + ' ' + IntToStr(scale(startY + radius))
                + ' ' + IntToStr(scale(radius)) + ' -900 0 '
                + IntToStr(aRoundRect.OwnerPartId) + ' 0 '
                + IntToStr(convertTSize(aRoundRect.LineWidth)));
end;


procedure processBezier(aBezier : ISch_Bezier);
var
    i, count : Integer;
    buf      : TDynamicString;
begin
    // B count part dmg pen X Y ... fill

    buf := 'B ' + IntToStr(aBezier.VerticesCount) + ' ' + IntToStr(aBezier.OwnerPartId)
                + ' 0 ' + IntToStr(convertTSize(aBezier.LineWidth));

    for i := 1 to aBezier.VerticesCount do
        buf := buf + ' ' + locToStr(aBezier.Vertex[i]);

    buf := buf + ' N';

    WriteLn(outFile, buf);
end;


procedure processLabel(aLabel : ISch_Label);
var
    buf : TDynamicString;
    fontMgr : ISch_FontManager;
begin
    // T angle X Y size hidden part dmg text italic bold Halign Valign

    fontMgr := SchServer.FontManager;

    buf := 'T ' + IntToStr(rotToInt90(aLabel.Orientation)) + ' ' + locToStr(aLabel.Location)
                + ' ' + IntToStr(fontSize(aLabel.FontID))
                + ' 0 '         // TODO visible == GraphObj::EnableDraw?
                + IntToStr(aLabel.OwnerPartId) + ' 0 '
                + '"' + StringReplace(aLabel.Text, '"', '''''', -1) + '"';

    if fontMgr.Italic(aLabel.FontID) then      // TODO can it be converted to int directly?
        buf := buf + ' Italic'
    else
        buf := buf + ' Normal';

    if fontMgr.Bold(aLabel.FontID) then      // TODO can it be converted to int directly?
        buf := buf + ' 1'
    else
        buf := buf + ' 0';

    buf := buf + ' ' + justToStr(aLabel.Justification);

    WriteLn(outFile, buf);
end;


procedure processPie(aPie : ISch_Pie);
var
    startPt, endPt : TLocation;
    buf            : TDynamicString;
begin
    // A posx posy radius start end part convert thickness cc start_pointX start_pointY end_pointX end_pointY
    startPt   := arcStartPt(aPie);
    endPt     := arcEndPt(aPie);

    // KiCad does not have pies, instead draw an arc and a polyline
    processArc(aPie, aPie.IsSolid());

    buf := 'P 3 ' + IntToStr(aPie.OwnerPartId) + ' 0 '
              + IntToStr(convertTSize(aPie.LineWidth))
              + ' ' + locToStr(startPt)
              + ' ' + locToStr(aPie.Location)
              + ' ' + locToStr(endPt);

    if aPie.IsSolid() then buf := buf + ' f' else buf := buf + ' N';

    WriteLn(outFile, buf);
end;


procedure processObject(aObject : ISch_GraphicalObject);
begin
    case aObject.ObjectId of
       ePin:            processPin(aObject);
       eRectangle:      processRectangle(aObject);
       eLine:           processLine(aObject);
       eArc:            processArc(aObject, false);
       ePolygon:        processPoly(aObject, true, true);
       ePolyline:       processPoly(aObject, false, false);
       eRoundRectangle: processRoundRect(aObject);
       eLabel:          processLabel(aObject);
       ePie:            processPie(aObject);
       //eBezier:         processBezier(aObject);

       // not available in KiCad
       eImage:          log(component + ': images are not supported');
       eEllipticalArc:  log(component + ': elliptical arcs are not supported');
       eEllipse:        log(component + ': ellipses are not supported');
       eSymbol:         log(component + ': IEEE symbols are not supported');

       // types that should not occur in symbols
       eWire:           log(component + ': wires should not exist in symbols');

       // handled in another way or irrelevant
       //eParameter
       //eParameterSet
       //eParameterList
       //eDesignator
       //eMapDefiner
       //eImplementationMap
       //eImplementation
       //eImplementationsList
    end;
end;


procedure processComponent(aComponent : ISch_Component; aTemplate : Boolean);
var
    objIterator, paramIterator  : ISch_Iterator;
    param                       : ISch_Parameter;
    paramList                   : TStringList;
    schObj                      : ISch_GraphicalObject;
    i                           : Integer;
    name, designator            : TString;
    buf                         : TDynamicString;
begin
     component := aComponent.LibReference;

    WriteLn(outFile, '#');
    WriteLn(outFile, '# ' + component);
    WriteLn(outFile, '#');

    name := fixName(component);

    // Remove question marks from designator
    designator := fixName(StringReplace(aComponent.Designator.Text, '?', '', -1));

    // TODO hardcoded fields
    // name reference unused text_offset draw_pin_number draw_pin_name unit_count units_swappable Normal/Power
    WriteLn(outFile, 'DEF ' + name + ' ' + designator + ' 0 50 Y Y '
        + IntToStr(aComponent.PartCount) + ' F N');


    // Aliases
    if aComponent.AliasCount > 1 then
    begin
        buf.Clear();

        for i:= 0 to aComponent.AliasCount() do
            buf := buf + ' ' + fixName(aComponent.AliasAsText(i));

        WriteLn(outFile, 'ALIAS' + buf);
    end;


    // Fields (parameters in Altium)
    paramList := TStringList.Create();
    paramList.Clear();

    // Default fields
    processParameter(aComponent.Designator, 0, aTemplate, paramList);

    // Custom fields
    paramIterator := aComponent.SchIterator_Create();
    paramIterator.SetState_IterationDepth(eIterateFirstLevel);
    paramIterator.AddFilter_ObjectSet(MkSet(eParameter));

    try
        param := paramIterator.FirstSchObject;
        i := 4;     // parameters 0-3 are reserved, 4+ are custom

        while param <> nil do
        begin
            processParameter(param, i, aTemplate, paramList);
            Inc(i);
            param := paramIterator.NextSchObject();
        end;

    finally
        aComponent.SchIterator_Destroy(paramIterator);
    end;

    for i := 0 to paramList.Count() - 1 do
        WriteLn(outFile, paramList[i]);

    paramList.Free();


    // Convert the graphic symbol
    WriteLn(outFile, 'DRAW');
    objIterator := aComponent.SchIterator_Create();

    try
        schObj := objIterator.FirstSchObject;

        while schObj <> nil do
        begin
            processObject(schObj);
            schObj := objIterator.NextSchObject();
        end;

    finally
        aComponent.SchIterator_Destroy(objIterator);
    end;


    WriteLn(outFile, 'ENDDRAW');
    WriteLn(outFile, 'ENDDEF');
    component := '';
end;


procedure processLibrary(aTemplate : Boolean);
var
  component     : ISch_Component;
  schLib        : ISch_Lib;
  schIterator   : ISch_Iterator;

  libName       : TDynamicString;
  libOutPath    : TString;

begin
    if UpperCase(Client.CurrentView.OwnerDocument.Kind) <> 'SCHLIB' then
    begin
        ShowWarning('This is not a Schematic Library document!');
        exit;
    end;

    schLib := SchServer.GetCurrentSchDocument;
    if schLib = nil then
        exit;

    // Set encoding to UTF-8
    // https://msdn.microsoft.com/en-us/library/windows/desktop/dd317756(v=vs.85).aspx
    SetMultiByteConversionCodePage(65001);

    libName := schLib.DocumentName;
    libName := StringReplace(ExtractFileName(libName), '.SchLib', '', -1);
    libOutPath := ExtractFileDir(schLib.DocumentName) + '\';
    logList := TStringList.Create();

    log('Converting ' + schLib.DocumentName);

    if not aTemplate then
    begin
        AssignFile(outFile, libOutPath + fixFileName(libName) + '.lib');
        Rewrite(outFile);
        addLibHeader(outFile);
    end;

    // Iterate through components in the library
    schIterator := schLib.SchLibIterator_Create;
    schIterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    try
        component := schIterator.FirstSchObject;

        while component <> nil do
        begin
            if aTemplate then
            begin
                AssignFile(outFile, libOutPath + fixFileName(component.LibReference) + '.lib');
                Rewrite(outFile);
                addLibHeader(outFile);
            end;

            processComponent(component, aTemplate);

            if aTemplate then
            begin
                addLibFooter(outFile);
                CloseFile(outFile);
            end;

            component := schIterator.NextSchObject;
        end;

    finally
        schLib.SchIterator_Destroy(SchIterator);
    end;

    if not aTemplate then
    begin
        addLibFooter(outFile);
        CloseFile(outFile);
    end;

    log('Converted');
    logList.SaveToFile(libOutPath + fixFileName(libName) + '.txt');
    logList.Free();

    ShowMessage('Saved in ' + libOutPath);
end;


procedure ConvertLibrary;
begin
    if SchServer = nil then
        exit;

    processLibrary(false);
end;


procedure GenerateTemplates;
begin
    if SchServer = nil then
        exit;

    processLibrary(true);
end;
