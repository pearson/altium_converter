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
  logList   : TStringList;
  // converted component name, used for logging
  component : String;
  outFile   : TextFile;
  // flag to decide whether we convert a library or generate component templates
  template  : Boolean;
  // number of component display modes (graphical representations)
  // (KiCad supports two component representation aka de Morgan)
  modeCount : Integer;
  partCount : Integer;
  fontMgr   : ISch_FontManager;

const
  // default parameter text height
  PARAM_TEXT_SIZE = 60;
  // factor to convert coordinates from KiCad to Altium
  SCALE_KI_TO_ALT = 10000;
  // number of default fields in KiCad components
  DEF_PARAMS_NUMBER = 4;

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


function partMode(aObject : ISch_GraphicalObject) : TDynamicString;
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


function fillObjToStr(aObject : ISch_GraphicalObject) : Char;
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


function arcStartPt(aArc : ISch_Arc) : TLocation;
begin
   result := TLocation;
   result.x := aArc.Location.x + aArc.Radius * Cos(aArc.StartAngle * PI / 180.0);
   result.y := aArc.Location.y + aArc.Radius * Sin(aArc.StartAngle * PI / 180.0);
end;


function arcEndPt(aArc : ISch_Arc) : TLocation;
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


// Autoplaces hidden parameters basing on their index
function autoParamToStr(aComponent : ISch_Component; aIndex : Integer;
    aValue : TDynamicString) : TDynamicString;
var
    paramPos : TLocation;
begin
    paramPos := TLocation;
    paramPos.x := aComponent.Location.x;
    paramPos.y := aComponent.BoundingRectangle_Full().bottom - scaleToAltium(PARAM_TEXT_SIZE * (aIndex * 1.5));

    result := 'F' + IntToStr(aIndex) + ' "' + aValue + '" '
            + locToStr(paramPos) + ' ' + IntToStr(PARAM_TEXT_SIZE) + ' H I L CNN';
end;


function paramToStr(aParameter : ISch_Parameter; aIndex : Integer;
    aValue : TDynamicString, aName : TDynamicString) : TDynamicString;
begin
    // F n "text" posx posy dimension orientation visibility hjustify vjustify/italic/bold ["name"]
    result := 'F' + IntToStr(aIndex) + ' "' + aValue + '" '
        + locToStr(aParameter.Location)
        + ' ' + IntToStr(fontSize(aParameter.FontID))
        + ' ' + rotToOrient(aParameter.Orientation)
        + ifElse(aParameter.IsHidden, ' I', ' V')
        + ' ' + justToStr(aParameter.Justification)
        + ifElse(fontMgr.Italic(aParameter.FontID), 'I', 'N')
        + ifElse(fontMgr.Bold(aParameter.FontID), 'B', 'N')

        // Custom fields have to store the field name
        + ifElse(aIndex >= DEF_PARAMS_NUMBER, ' "' + aName + '"', '');
end;


function processParameter(aParameter : ISch_Parameter; aComponent : ISch_Component;
    var aParamIdx : Integer) : TDynamicString;
var
    value, name : TDynamicString;
    i : Integer;
begin
    // Defaults
    value := aParameter.Text;
    name := aParameter.Name;

    // Correct default field numbers
    if name = 'Designator' then
    begin
        aParamIdx := 0;
        // Altium keeps designators as 'X?' whereas KiCad uses only 'X'
        value := StringReplace(aParameter.Text, '?', '', -1);
    end

    else if aParameter.Text = '=Device' then
    begin
        aParamIdx := 1;
    end

    else if name = 'Footprint' then
        aParamIdx := 2       // TODO use ISch_Implementation to figure out the footprint?

    else if name = 'HelpURL' then
        aParamIdx := 3;

    if template then
    begin
        // Component Name
        if aParamIdx = 1 then
            value := '${Part Number}'

        // Footprint
        else if aParamIdx = 2 then
            value := '${Library Name}:${Footprint Ref}'

        // Field evaluation
        else if value[1] = '=' then
        begin
            name := Copy(value, 2, Length(value) - 1);
            value := '${' + name + '}';

            if name = 'Value'
                then name := 'Val';     // 'Value' is a reserved field name
        end

        // Other parameters apart from the Designator field
        else if aParamIdx <> 0 then
            value := '${' + name + '}';
    end
    else
    begin
        // Escape quotes
        value := StringReplace(value, '"', '\"', -1);
    end;

    if aParameter.IsHidden then
        result := autoParamToStr(aComponent, aParamIdx, value)
    else
        result := paramToStr(aParameter, aParamIdx, value, '');
end;


procedure processPoly(aPoly : ISch_Polygon; aFilled : Boolean;
    aCloseLine : Boolean);
var
    i, count : Integer;
begin
    // P Nb parts convert thickness x0 y0 x1 y1 xi yi cc

    count := aPoly.VerticesCount;
    if aCloseLine then Inc(count);

    Write(outFile, 'P ' + IntToStr(count) + ' ' + partMode(aPoly)
            + ' ' + IntToStr(convertTSize(aPoly.LineWidth)));

    for i := 1 to aPoly.VerticesCount do
        Write(outFile, ' ' + locToStr(aPoly.Vertex[i]));

    if aCloseLine then Write(outFile, ' ' + locToStr(aPoly.Vertex[1]));

    WriteLn(outFile, ' ' + fillToStr(aFilled, aPoly.AreaColor));
end;


procedure processPin(aPin : ISch_Pin);
var
    pos         : TLocation;
    pinShape    : TDynamicString;
begin
    // X name number posx posy length orientation Snum Snom unit convert Etype [shape]

    // Correct the pin position
    pos := aPin.Location;
    pinShape := '';

    case aPin.Orientation of
        eRotate0:   pos.x := aPin.Location.x + aPin.PinLength;  // left
        eRotate90:  pos.y := aPin.Location.y + aPin.PinLength;  // down
        eRotate180: pos.x := aPin.Location.x - aPin.PinLength;  // right
        eRotate270: pos.y := aPin.Location.y - aPin.PinLength;  // up
    end;

    Write(outFile, 'X ' + fixName(aPin.Name) + ' ' + fixName(aPin.Designator)
            + ' ' + locToStr(pos) + ' ' + IntToStr(scaleToKiCad(aPin.PinLength))
            + ' ' + rotToStr(aPin.Orientation)
            + ifElse(aPin.ShowDesignator, ' 60', ' 0')      // TODO get correct size
            + ifElse(aPin.ShowName, ' 60 ', ' 0 ')          // TODO get correct size
            + partMode(aPin));

    case aPin.Electrical of
        eElectricInput:            Write(outFile, ' I');
        eElectricIO:               Write(outFile, ' B');
        eElectricOutput:           Write(outFile, ' O');
        eElectricOpenCollector:    Write(outFile, ' C');
        eElectricPassive:          Write(outFile, ' P');
        eElectricHiZ:              Write(outFile, ' T');
        eElectricOpenEmitter:      Write(outFile, ' E');
        // There is no power input/output distinction in Altium, so there
        // is simple heuristics trying to guess the type
        eElectricPower:
           if AnsiPos('out', Lowercase(aPin.Name)) > 0 then
                Write(outFile, ' w')
            else
                Write(outFile, ' W');
    end;

    // Pin shape
    {if pinShape = '' then begin
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
        end;
    end;}

    if pinShape = '' then
    begin
        case aPin.Symbol_InnerEdge of
            eClock: pinShape := 'C';
        end;
    end;

    if pinShape = '' then
    begin
        case aPin.Symbol_OuterEdge of
            eDot:                 pinShape := 'I';
            eActiveLowInput:      pinShape := 'L';
            eActiveLowOutput:     pinShape := 'V';
        end;
    end;

    {if pinShape = '' then
    begin
        case aPin.Symbol_Outer of
        //eRightLeftSignalFlow:
        //eAnalogSignalIn:
        //eNotLogicConnection:
        //eDigitalSignalIn:
        //eLeftRightSignalFlow:
        //eBidirectionalSignalFlow:
        end;
    end;}

    if aPin.IsHidden then
        pinShape := 'N' + pinShape;

    WriteLn(outFile, ifElse(pinShape <> '', ' ' + pinShape, ''));
end;


procedure processRectangle(aRect : ISch_Rectangle);
begin
    // S startx starty endx endy unit convert thickness cc

    WriteLn(outFile, 'S ' + locToStr(aRect.Location)
            + ' ' + locToStr(aRect.Corner) + ' ' + partMode(aRect)
            + ' ' + IntToStr(convertTSize(aRect.LineWidth))
            + ' ' + fillObjToStr(aRect));
end;


procedure processLine(aLine : ISch_Line);
begin
    // P Nb parts convert thickness x0 y0 x1 y1 xi yi cc

    WriteLn(outFile, 'P 2 ' + partMode(aLine)
            + ' ' + IntToStr(convertTSize(aLine.LineWidth))
            + ' ' + locToStr(aLine.Location) + ' ' + locToStr(aLine.Corner) + ' N');
end;


procedure processArc(aArc : ISch_Arc; aFilled : Boolean);
begin
    // A posx posy radius start end part convert thickness cc start_pointX start_pointY end_pointX end_pointY

    Write(outFile, 'A ' + locToStr(aArc.Location) + ' ' + IntToStr(scaleToKiCad(aArc.Radius))
            + ' ' + IntToStr(aArc.EndAngle * 10) + ' ' + IntToStr(aArc.StartAngle * 10)
            + ' ' + partMode(aArc) + ' ' + IntToStr(convertTSize(aArc.LineWidth)));

    if aFilled then Write(outFile, ' f ') else Write(outFile, ' N ');

    WriteLn(outFile, locToStr(arcEndPt(aArc)) + ' ' + locToStr(arcStartPt(aArc)));
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
    WriteLn(outFile, 'P 2 ' + partMode(aRoundRect)
            + ' ' + IntToStr(convertTSize(aRoundRect.LineWidth))
            + ' ' + IntToStr(scaleToKiCad(startX)) + ' ' + IntToStr(scaleToKiCad(startY + radius))
            + ' ' + IntToStr(scaleToKiCad(startX)) + ' ' + IntToStr(scaleToKiCad(endY - radius)) + ' N');

    // bottom edge
    WriteLn(outFile, 'P 2 ' + partMode(aRoundRect)
            + ' ' + IntToStr(convertTSize(aRoundRect.LineWidth))
            + ' ' + IntToStr(scaleToKiCad(startX + radius)) + ' ' + IntToStr(scaleToKiCad(endY))
            + ' ' + IntToStr(scaleToKiCad(endX - radius)) + ' ' + IntToStr(scaleToKiCad(endY)) + ' N');

    // right edge
    WriteLn(outFile, 'P 2 ' + partMode(aRoundRect)
            + ' ' + IntToStr(convertTSize(aRoundRect.LineWidth))
            + ' ' + IntToStr(scaleToKiCad(endX)) + ' ' + IntToStr(scaleToKiCad(startY + radius))
            + ' ' + IntToStr(scaleToKiCad(endX)) + ' ' + IntToStr(scaleToKiCad(endY - radius)) + ' N');

    // top edge
    WriteLn(outFile, 'P 2 ' + partMode(aRoundRect)
            + ' ' + IntToStr(convertTSize(aRoundRect.LineWidth))
            + ' ' + IntToStr(scaleToKiCad(startX + radius)) + ' ' + IntToStr(scaleToKiCad(startY))
            + ' ' + IntToStr(scaleToKiCad(endX - radius)) + ' ' + IntToStr(scaleToKiCad(startY)) + ' N');

    // top left corner
    WriteLn(outFile, 'A ' + IntToStr(scaleToKiCad(startX + radius)) + ' ' + IntToStr(scaleToKiCad(endY - radius))
            + ' ' + IntToStr(scaleToKiCad(radius)) + ' 900 1800 '
            + partMode(aRoundRect)
            + ' ' + IntToStr(convertTSize(aRoundRect.LineWidth)));

    // bottom left corner
    WriteLn(outFile, 'A ' + IntToStr(scaleToKiCad(startX + radius)) + ' ' + IntToStr(scaleToKiCad(startY + radius))
            + ' ' + IntToStr(scaleToKiCad(radius)) + ' -900 1800 '
            + partMode(aRoundRect)
            + ' ' + IntToStr(convertTSize(aRoundRect.LineWidth)));

    // top right corner
    WriteLn(outFile, 'A ' + IntToStr(scaleToKiCad(endX - radius)) + ' ' + IntToStr(scaleToKiCad(endY - radius))
            + ' ' + IntToStr(scaleToKiCad(radius)) + ' 900 0 '
            + partMode(aRoundRect)
            + ' ' + IntToStr(convertTSize(aRoundRect.LineWidth)));

    // bottom right corner
    WriteLn(outFile, 'A ' + IntToStr(scaleToKiCad(endX - radius)) + ' ' + IntToStr(scaleToKiCad(startY + radius))
            + ' ' + IntToStr(scaleToKiCad(radius)) + ' -900 0 '
            + partMode(aRoundRect)
            + ' ' + IntToStr(convertTSize(aRoundRect.LineWidth)));
end;


procedure processBezier(aBezier : ISch_Bezier);
var
    i, count : Integer;
begin
    // B count part dmg pen X Y ... fill

    Write(outFile, 'B ' + IntToStr(aBezier.VerticesCount)
            + ' ' + partMode(aBezier)
            + ' ' + IntToStr(convertTSize(aBezier.LineWidth)));

    for i := 1 to aBezier.VerticesCount do
        Write(outFile, ' ' + locToStr(aBezier.Vertex[i]));

    WriteLn(outFile, ' N');
end;


procedure processLabel(aLabel : ISch_Label);
begin
    // T angle X Y size hidden part dmg text italic bold Halign Valign

    WriteLn(outFile, 'T ' + IntToStr(rotToInt90(aLabel.Orientation))
            + ' ' + locToStr(aLabel.Location)
            + ' ' + IntToStr(fontSize(aLabel.FontID))
            + ' 1 ' + partMode(aLabel)
            + ' "' + escapeLabel(aLabel.Text) + '"'
            + ifElse(fontMgr.Italic(aLabel.FontID), ' Italic', ' Normal')
            + ifElse(fontMgr.Bold(aLabel.FontID), ' 1 ', ' 0 ')
            + justToStr(aLabel.Justification));
end;


procedure processPie(aPie : ISch_Pie);
var
    startPt, endPt : TLocation;
begin
    // A posx posy radius start end part convert thickness cc start_pointX start_pointY end_pointX end_pointY
    startPt   := arcStartPt(aPie);
    endPt     := arcEndPt(aPie);

    // KiCad does not have pies, instead draw an arc and a polyline
    processArc(aPie, aPie.IsSolid());

    WriteLn(outFile, 'P 3 ' + partMode(aPie)
            + ' ' + IntToStr(convertTSize(aPie.LineWidth))
            + ' ' + locToStr(startPt)
            + ' ' + locToStr(aPie.Location)
            + ' ' + locToStr(endPt)
            + ' ' + fillObjToStr(aPie));
end;


procedure processEllipse(aEllipse : ISch_Ellipse);
var
   ctrlPts : array[0..3] of TLocation;
   loc : TLocation;
   rad1, rad2, i : Integer;
begin
    loc := aEllipse.Location;
    rad1 := aEllipse.Radius;
    rad2 := aEllipse.SecondaryRadius;

    // Special case: an ellipse which has equal both radiuses is a circle
    if rad1 = rad2 then
    begin
        // circle
        // C posx posy radius unit convert thickness cc
        WriteLn(outFile, 'C ' + locToStr(loc)
                + ' ' + IntToStr(scaleToKiCad(rad1))
                + ' ' + partMode(aEllipse)
                + ' ' + IntToStr(convertTSize(aEllipse.LineWidth))
                + ' ' + fillObjToStr(aEllipse));
    end
    else
    begin
        // approximate ellipse with 2 Bezier curves

        for i := 0 to 3 do
            ctrlPts[i] := TLocation;

        ctrlPts[0].x := loc.x - rad1;
        ctrlPts[0].y := loc.y;
        ctrlPts[1].x := loc.x - rad1;
        ctrlPts[1].y := loc.y + rad2 * 4 / 3;
        ctrlPts[2].x := loc.x + rad1;
        ctrlPts[2].y := loc.y + rad2 * 4 / 3;
        ctrlPts[3].x := loc.x + rad1;
        ctrlPts[3].y := loc.y;

        Write(outFile, 'B 4 ' + partMode(aEllipse)
            + ' ' + IntToStr(convertTSize(aEllipse.LineWidth)));

        for i := 0 to 3 do
            Write(outFile, ' ' + locToStr(ctrlPts[i]));

        WriteLn(outFile, ' ' + fillObjToStr(aEllipse));

        ctrlPts[1].y := loc.y - rad2 * 4 / 3;
        ctrlPts[2].y := loc.y - rad2 * 4 / 3;

        Write(outFile, 'B 4 ' + partMode(aEllipse)
            + ' ' + IntToStr(convertTSize(aEllipse.LineWidth)));

        for i := 0 to 3 do
            Write(outFile, ' ' + locToStr(ctrlPts[i]));

        WriteLn(outFile, ' ' + fillObjToStr(aEllipse));
    end;
end;


procedure processEllipticalArc(aEllipArc : ISch_EllipticalArc);
var
    c : TLocation;
    a, b, i, step, lineSegments : Integer;
    lambda1, lambda2, angleStep : TAngle;
    sinLambda1, sinLambda2, cosLambda1, cosLambda2 : Double;

    ctrlPts : array[0..3] of TLocation;     // Bezier curve control points
    eta1, eta2, tanEtaDiff, alpha : Double;
const
    // Number of Bezier curves used for approximation
    BEZ_SEGMENTS = 2;
begin
    a := aEllipArc.Radius;
    b := aEllipArc.SecondaryRadius;

    // When both radiuses are equal, it is just a common arc
    if a = b then
        processArc(aEllipArc, false)
    else
    begin
        lineSegments := BEZ_SEGMENTS;


        angleStep := aEllipArc.EndAngle - aEllipArc.StartAngle;

        if angleStep < 0.0 then
            angleStep := angleStep + 360.0;

        angleStep := angleStep / lineSegments;


        // Linear approximation
        {Write(outFile, 'P ' + IntToStr(lineSegments + 1)  + ' ' + partMode(aEllipArc)
            + ' ' + IntToStr(convertTSize(aEllipArc.LineWidth)));

        lambda1 := aEllipArc.StartAngle;
        c := TLocation;

        for step := 0 to lineSegments do
        begin
            c.x := aEllipArc.Location.x + a * Cos(lambda1 * PI / 180.0);
            c.y := aEllipArc.Location.y + b * Sin(lambda1 * PI / 180.0);
            Write(outFile, ' ' + locToStr(c));
            lambda1 := lambda1 + angleStep;
        end;

        WriteLn(outFile, ' N');}


        // Bezier curves approximation
        for i := 0 to 3 do
            ctrlPts[i] := TLocation;

        lambda1 := aEllipArc.StartAngle;
        lambda2 := aEllipArc.StartAngle + angleStep;
        c       := aEllipArc.Location;

        for step := 1 to lineSegments do
        begin
            // Approximate with a Bezier curve
            // see: http://www.spaceroots.org/documents/ellipse/elliptical-arc.pdf
            sinLambda1  := Sin(lambda1 * PI / 180.0);
            sinLambda2  := Sin(lambda2 * PI / 180.0);
            cosLambda1  := Cos(lambda1 * PI / 180.0);
            cosLambda2  := Cos(lambda2 * PI / 180.0);
            eta1        := ArcTan2(sinLambda1 / b, cosLambda1 / a);
            eta2        := ArcTan2(sinLambda2 / b, cosLambda2 / a);
            tanEtaDiff  := Tan((eta2 - eta1) / 2);
            alpha       := Sin(eta2 - eta1) * (Sqrt(4.0 + 3.0 * tanEtaDiff * tanEtaDiff) - 1.0) / 3.0;

            ctrlPts[0].x := c.x + a * Cos(eta1);
            ctrlPts[0].y := c.y + b * Sin(eta1);
            ctrlPts[3].x := c.x + a * Cos(eta2);
            ctrlPts[3].y := c.y + b * Sin(eta2);
            ctrlPts[1].x := ctrlPts[0].x + alpha * (-a * Sin(eta1));
            ctrlPts[1].y := ctrlPts[0].y + alpha * b * Cos(eta1);
            ctrlPts[2].x := ctrlPts[3].x - alpha * (-a * Sin(eta2));
            ctrlPts[2].y := ctrlPts[3].y - alpha * b * Cos(eta2);

            Write(outFile, 'B 4 ' + partMode(aEllipArc)
                + ' ' + IntToStr(convertTSize(aEllipArc.LineWidth)));

            for i := 0 to 3 do
                Write(outFile, ' ' + locToStr(ctrlPts[i]));

            WriteLn(outFile, ' N');

            lambda1 := lambda2;
            lambda2 := lambda2 + angleStep;
        end;
    end;
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
        eEllipse:        processEllipse(aObject);
        eEllipticalArc:  processEllipticalArc(aObject);
        eBezier:         processBezier(aObject);

        // not available in KiCad
        eImage:          log(component + ': images are not supported');
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


procedure processComponent(aComponent : ISch_Component);
var
    objIterator, paramIterator  : ISch_Iterator;
    param                       : ISch_Parameter;
    defParams                   : array[0..DEF_PARAMS_NUMBER] of TDynamicString;
    customParams                : TStringList;
    schObj                      : ISch_GraphicalObject;
    i, idx                      : Integer;
    name, designator, buf       : TDynamicString;

begin
    component := aComponent.LibReference;
    modeCount := aComponent.DisplayModeCount;
    partCount := aComponent.PartCount;

    if modeCount > 2 then
        log('components with more than 2 modes are not supported');

    WriteLn(outFile, '#');
    WriteLn(outFile, '# ' + component);
    WriteLn(outFile, '#');

    if template then
        name := '${Part Number}'
    else
        name := fixName(component);

    // Remove question marks from designator
    designator := fixName(StringReplace(aComponent.Designator.Text, '?', '', -1));

    // TODO swappable?
    // name reference unused text_offset draw_pin_number draw_pin_name unit_count units_swappable Normal/Power
    WriteLn(outFile, 'DEF ' + name + ' ' + designator + ' 0 '
        + IntToStr(PARAM_TEXT_SIZE) + ' Y Y ' + IntToStr(aComponent.PartCount) + ' F N');


    // Aliases
    if aComponent.AliasCount > 1 then
    begin
        Write(outFile, 'ALIAS');

        for i:= 0 to aComponent.AliasCount() do
            Write(outFile, ' ' + fixName(aComponent.AliasAsText(i)));

        WriteLn(outFile, '');
    end;


    // Fields (parameters in Altium)
    customParams := TStringList.Create();

    // Default fields
    defParams[0] := processParameter(aComponent.Designator, aComponent, idx);
    defParams[1] := autoParamToStr(aComponent, 1, name);

    if template then
    begin
        defParams[2] := autoParamToStr(aComponent, 2, '${Library Name}:${Footprint Ref}');
        defParams[3] := autoParamToStr(aComponent, 3, '${HelpURL}');
    end
    else
    begin
        defParams[2] := autoParamToStr(aComponent, 2, '');          // Footprint
        defParams[3] := autoParamToStr(aComponent, 3, '');          // Datasheet
    end;

    // Custom fields
    paramIterator := aComponent.SchIterator_Create();
    paramIterator.SetState_IterationDepth(eIterateFirstLevel);
    paramIterator.AddFilter_ObjectSet(MkSet(eParameter));

    try
        param := paramIterator.FirstSchObject;

        while param <> nil do
        begin
            idx := DEF_PARAMS_NUMBER + customParams.Count;
            buf := processParameter(param, aComponent, idx);

            if idx < DEF_PARAMS_NUMBER then
               defParams[idx] := buf
            else
                customParams.Append(buf);

            param := paramIterator.NextSchObject();
        end;

    finally
        aComponent.SchIterator_Destroy(paramIterator);
    end;

    for i := 0 to 3 do
        WriteLn(outFile, defParams[i]);

    for i := 0 to customParams.Count() - 1 do
        WriteLn(outFile, customParams[i]);

    customParams.Free();

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
  compReader    : ILibCompInfoReader;

  libName       : TDynamicString;
  libOutPath    : TString;

begin
    template := aTemplate;
    fontMgr := SchServer.FontManager;

    if UpperCase(Client.CurrentView.OwnerDocument.Kind) <> 'SCHLIB' then
    begin
        ShowWarning('This is not a Schematic Library document!');
        exit;
    end;

    schLib := SchServer.GetCurrentSchDocument;
    if schLib = nil then
        exit;

    compReader := SchServer.CreateLibCompInfoReader(schLib.DocumentName);
    compReader.ReadAllComponentInfo();

    // Set encoding to UTF-8
    // https://msdn.microsoft.com/en-us/library/windows/desktop/dd317756(v=vs.85).aspx
    SetMultiByteConversionCodePage(65001);

    libName := schLib.DocumentName;
    libName := StringReplace(ExtractFileName(libName), '.SchLib', '', -1);
    libOutPath := ExtractFileDir(schLib.DocumentName) + '\';
    logList := TStringList.Create();

    if libName = '' then
    begin
        ShowMessage('Empty library name, aborting');
        Exit;
    end;

    log('Converting ' + schLib.DocumentName);

    if template then
    begin
        // Create a directory to keep templates
        libOutPath := libOutPath + libName + '\';

        if DirectoryExists(libOutPath) then
             RmDir(libOutPath);

        ForceDirectories(libOutPath);
    end
    else
    begin
        AssignFile(outFile, libOutPath + fixFileName(libName) + '.lib');
        Rewrite(outFile);
        addLibHeader(outFile);
    end;

    // Iterate through components in the library
    schIterator := schLib.SchLibIterator_Create;
    schIterator.AddFilter_ObjectSet(MkSet(eSchComponent));
    ProgressInit('Converting library ' + schLib.DocumentName, compReader.NumComponentInfos);

    try
        component := schIterator.FirstSchObject;

        while component <> nil do
        begin
            if template then
            begin
                AssignFile(outFile, libOutPath + fixFileName(component.LibReference) + '.lib');
                Rewrite(outFile);
                addLibHeader(outFile);
            end;

            processComponent(component);

            if template then
            begin
                addLibFooter(outFile);
                CloseFile(outFile);
            end
            else
                Flush(outFile);

            ProgressUpdate(1);
            component := schIterator.NextSchObject;
        end;

    finally
        schLib.SchIterator_Destroy(SchIterator);
    end;

    if not template then
    begin
        addLibFooter(outFile);
        CloseFile(outFile);
    end;

    log('Converted');
    logList.SaveToFile(libOutPath + fixFileName(libName) + '.txt');
    logList.Free();

    ProgressFinish(0);
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
