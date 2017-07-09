{*
 * Altium to KiCad converter script - common part
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

unit altium_kicad;

const
  // default parameter text height
  PARAM_TEXT_SIZE = 60;

var
  logList : TStringList;
  scaleNumerator, scaleDenominator : Integer;
  // whether use floating or fixed point for rescale
  floatRescale : Boolean;
  precision : Integer;


procedure log(aMessage : TDynamicString);
begin
    logList.Append(DateToStr(Date) + ' ' + timeToStr(Time) + ': ' + aMessage);
end;


function ifElse(aCondition : Boolean; aStringTrue : TDynamicString;
    aStringFalse : TDynamicString) : TDynamicString;
begin
    if aCondition then
        result := aStringTrue
    else
        result := aStringFalse;
end;


procedure setScale(aScaleNum, aScaleDenom, aPrecision : Integer;
                   aFloatRescale : Boolean);
var
    i : Integer;
begin
    scaleNumerator := aScaleNum;
    scaleDenominator := aScaleDenom;

    // in fixed point mode adjust the scale denominator to
    // achieve the requested precision during calculations
    if not aFloatRescale then
    begin
        for i := 1 to aPrecision do
            scaleDenominator := scaleDenominator div 10;
    end;

    precision := aPrecision;
    floatRescale := aFloatRescale;
    SetRoundMode(rmNearest);
end;


// divide and round to the nearest integer
function divRound(n, d : Integer) : Integer;
begin
    if (n < 0) xor (d < 0) then
        result := ((n - d div 2) div d)
    else
        result := ((n + d div 2) div d);
end;


// safe rescaling algorithm for numbers that would cause overflow when
// multiplicated by the scale numerator (much slower, so use only when needed)
function safeFixedRescale(a, b, c : Integer) : Integer;
var
    hi, lo : Integer;
begin
    hi := Max(a, b);
    lo := Min(a, b);

    // this is what happens when a programming language
    // does not provide 64-bit types
    result := (hi div c) * lo + (hi mod c) * lo / c;
end;


function fixedRescale(aVal, aNumerator, aDenominator : Integer) : Integer;
begin
    if aVal = 0 then
        result := 0

    // check for overflow in the multiplication result
    else if aVal < 2147483646 div aNumerator then
        result := divRound(aVal * aNumerator, aDenominator)

    else if aVal > 0 then
        result := safeFixedRescale(aVal, aNumerator, aDenominator)

    else // aVal < 0
        result := -safeFixedRescale(-aVal, aNumerator, aDenominator);
end;


function fixedScaleToKiCad(aVal : Integer) : Integer;
begin
    result := fixedRescale(aVal, scaleNumerator, scaleDenominator);
end;


function fixedScaleToAltium(aVal : Integer) : Integer;
begin
    result := fixedRescale(aVal, scaleDenominator, scaleNumerator);
end;


function fixedToStr(aNumber : Integer) : TDynamicString;
var
    strLen, digLen, firstDig : Integer;
begin
    result := IntToStr(aNumber);

    if (precision > 0) and (aNumber <> 0) then
    begin
        strLen := Length(result);

        if result[1] = '-' then
        begin
            firstDig := 2;
            digLen := strLen - 1;
        end
        else // positive numbers
        begin
            firstDig := 1;
            digLen := strLen;
        end;

        if digLen > precision then
            Insert('.', result, strLen - precision + 1)
        else // add heading zeros
            Insert('0.' + StringOfChar('0', precision - digLen), result, firstDig);

        strLen := Length(result);

        // strip the trailing zeros
        while result[strLen] = '0' do
            Dec(strLen);

        // remove point if it is the last character (e.g. 123.)
        if result[strLen] = '.' then
            Dec(strLen);

        SetLength(result, strLen);
    end;
end;


function sizeToStr(aSize : TCoord) : TDynamicString;
begin
    if floatRescale then
        result := FloatToStrF(aSize * scaleNumerator / scaleDenominator,
                              ffGeneral, 6, precision)
    else
        result := fixedToStr(fixedScaleToKiCad(aSize));
end;


function XYToStr(aX : TCoord, aY : TCoord) : TDynamicString;
begin
    result := sizeToStr(aX) + ' ' + sizeToStr(aY);
end;


function locToStr(aLocation : TLocation) : TDynamicString;
begin
    result := XYToStr(aLocation.x, aLocation.y);
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


function fixSpaces(aString : TDynamicString) : TDynamicString;
begin
    result := StringReplace(aString, ' ', '_', -1);
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


function defValue(aValue : TDynamicString, aDefaultValue : TDynamicString) : TDynamicString;
begin
    result := ifElse(aValue <> '', aValue, aDefaultValue);
end;

end.
