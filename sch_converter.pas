{TODO LICENSE}
function scale(RelCoord : Real) : Real;
begin
    result := RelCoord * 0.00008;
end;


function fixName(aName : String) : String;
begin
    // Spaces are not allowed in symbol names in KiCad
    result := StringReplace(aName, ' ', '_', -1);
end;


procedure addLibHeader(aOut : TStringList);
begin
    aOut.Add('EESchema-LIBRARY Version 2.3');
    aOut.Add('#encoding utf-8');
end;


procedure addLibEnd(aOut : TStringList);
begin
    aOut.Add('#');
    aOut.Add('#End Library');
end;


/// Adds missing default fields
procedure fixParams(aComponent : ISch_Component; aOut : TStringList);
begin
    if Copy(aOut[0], 0, 2) <> 'F0' then
        processParameter(aComponent.Designator, 0, aTemplate, paramList);

    {if Copy(aOut[1], 0, 2) <> 'F1' then}
        {aOut.Insert(1)}
end;


procedure processParameter(aParameter : ISch_Parameter; aParamNr : Integer;
    aTemplate : Boolean; aOut : TStringList);
var
    value       : TString;
    buf         : TDynamicString;
    i, paramIdx : Integer;
begin
    value := aParameter.Text;

    // Correct default field numbers
    if aParameter.Name = 'Designator' then
    begin
        aParamNr := 0;
        value := StringReplace(aParameter.Text, '?', '', rfReplaceAll);
    end
    else if aParameter.Name = 'Value' then
        aParamNr := 1
    else if aParameter.Name = 'Footprint' then
        aParamNr := 2       // TODO use ISch_Implementation to figure out the footprint?
    else if aParameter.Name = 'HelpURL' then
        aParamNr := 3;

    if aTemplate = true then
    begin
        if aParamNr = 2 then
            value := '${Library Name}:${Footprint Ref}'
        else
            value := '${' + aParameter.Name + '}';

    end;

    buf := 'F' + IntToStr(aParamNr) + ' "' + value + '" '
        + IntToStr(scale(aParameter.Location.x)) + ' ' + IntToStr(scale(aParameter.Location.y))
        + ' 50 '            // TODO hardcoded size
        + 'H I L CNN';      // TODO hardcoded justification/orientation/etc.

    // Default fields do not store the field name at the end
    if aParamNr >= 4 then
        buf := buf + ' "' + aParameter.Name + '"';

    // Find the right place to insert the parameter
    for i := 0 to aOut.Count() - 1 do
    begin
        // Extract the field number for i-th string in the output list
        paramIdx := StrToInt(Copy(aOut[i], 2, Pos(' ', aOut[i]) - 2));

        if paramIdx > aParamNr then
            break;
    end;

    aOut.Insert(i, buf);
end;


procedure processObject(aObject : ISch_GraphicalObject, aOut : TStringList);
var
    pin : ISch_Pin;
    rect : ISch_Rectangle;
    line : ISch_Line;
    arc : ISch_Arc;
    buf : TDynamicString;
    i, x, y : Integer;
begin
    // puts item name in the reportinfo TStringList container
    //aOut.Add(' The symbol has : ' + ObjectIdToString(AnObject.ObjectId));

    case aObject.ObjectId of
       ePin:
       begin
            //X name number posx posy length orientation Snum Snom unit convert Etype [shape]
            pin := aObject;

            case pin.Orientation of
                 eRotate0:       // left
                 begin
                      x := pin.Location.x + pin.PinLength;
                      y := pin.Location.y;
                 end;
                 eRotate90:      // down
                 begin
                      x := pin.Location.x;
                      y := pin.Location.y + pin.PinLength;
                 end;
                 eRotate180:     // right
                 begin
                      x := pin.Location.x - pin.PinLength;
                      y := pin.Location.y;
                 end;
                 eRotate270:     // up
                 begin
                      x := pin.Location.x;
                      y := pin.Location.y - pin.PinLength;
                 end;
            end;

            // TODO check pin numbers (ISch_Implementation?)
            buf := 'X ' + fixName(pin.Name) + ' ' + fixName(pin.Designator)
                   + ' ' + IntToStr(scale(x)) + ' ' + IntToStr(scale(y))
                   + ' ' + IntToStr(scale(pin.PinLength));

            case pin.Orientation of
                 eRotate0: buf := buf + ' L ';
                 eRotate90: buf := buf + ' D ';
                 eRotate180: buf := buf + ' R ';
                 eRotate270: buf := buf + ' U ';
            end;

            // TODO text label size?
            // TODO unit convert
            buf := buf + '50 50 0 0';

            case pin.Electrical of
                eElectricInput:            buf := buf + ' I';
                eElectricIO:               buf := buf + ' B';
                eElectricOutput:           buf := buf + ' O';
                eElectricOpenCollector:    buf := buf + ' C';
                eElectricPassive:          buf := buf + ' P';
                eElectricHiZ:              buf := buf + ' T';
                eElectricOpenEmitter:      buf := buf + ' E';
                // TODO there is no power input/output distinction
                eElectricPower:            buf := buf + ' W';
            end;

            // TODO is it SymbolInner, SymbolOuter, SymbolInnerEdge, SymbolOuterEdge?
            case pin.Symbol_InnerEdge of
                //eNoSymbol:
                eDot:                      buf := buf + ' I';
                //eRightLeftSignalFlow:
                eClock:                    buf := buf + ' C';
                eActiveLowInput:           buf := buf + ' L';
                //eAnalogSignalIn:
                //eNotLogicConnection:
                //eShiftRight:
                //ePostPonedOutput:
                //eOpenCollector:
                //eHiz:
                //eHighCurrent:
                //ePulse:
                //eSchmitt:
                //eDelay:
                //eGroupLine:
                //eGroupBin:
                eActiveLowOutput:          buf := buf + ' V';
                //ePiSymbol:
                //eGreaterEqual:
                //eLessEqual:
                //eSigma:
                //eOpenCollectorPullUp:
                //eOpenEmitter:
                //eOpenEmitterPullUp:
                //eDigitalSignalIn:
                //eAnd:
                //eInvertor:
                //eOr:
                //eXor:
                //eShiftLeft:
                //eInputOutput:
                //eOpenCircuitOutput:
                //eLeftRightSignalFlow:
                //eBidirectionalSignalFlow:
            end;

            aOut.Add(buf);
       end;

       eRectangle:
       begin
           // TODO unit & convert are not handled
           //S startx starty endx endy unit convert thickness cc
           rect := aObject;
           buf := 'S ' +  IntToStr(scale(rect.Location.x)) + ' ' + IntToStr(scale(rect.Location.y))
                      + ' ' + IntToStr(scale(rect.Corner.x)) + ' ' + IntToStr(scale(rect.Corner.y)) +
                      + ' 0 0 ' + IntToStr(scale(rect.LineWidth));

           //if rect.IsSolid() then buf := buf + ' F' else buf := buf + ' N';
           buf := buf + ' N';
           aOut.Add(buf);
       end;

       eLine:            // same as polygon
       begin
            //P Nb parts convert thickness x0 y0 x1 y1 xi yi cc
            line := aObject;
            // TODO part convert
            aOut.Add('P 2 0 0 ' + IntToStr(scale(line.LineWidth))
                     + ' ' + IntToStr(scale(line.Location.x)) + ' ' + IntToStr(scale(line.Location.y))
                     + ' ' + IntToStr(scale(line.Corner.x)) + ' ' + IntToStr(scale(line.Corner.y))
                     + ' N');
       end;

       eArc:
       begin
            arc := aObject;
            // A posx posy radius start end part convert thickness cc start_pointX start_pointY end_pointX end_pointY

            aOut.Add('A ' + IntToStr(scale(arc.Location.x)) + ' ' + IntToStr(scale(arc.Location.y)) +
                     + ' ' + IntToStr(scale(arc.Radius))
                     + ' ' + IntToStr(arc.StartAngle * 10) + ' ' + IntToStr(arc.EndAngle * 10) +
                     // TODO part convert
                     + ' 0 0 ' + IntToStr(scale(arc.LineWidth)) + ' N '
                     + ' ' + IntToStr(scale(arc.Location.x + arc.Radius * Cos(arc.StartAngle / 360 * 2 * PI)))
                     + ' ' + IntToStr(scale(arc.Location.y + arc.Radius * Sin(arc.StartAngle / 360 * 2 * PI)))
                     + ' ' + IntToStr(scale(arc.Location.x + arc.Radius * Cos(arc.EndAngle / 360 * 2 * PI)))
                     + ' ' + IntToStr(scale(arc.Location.y + arc.Radius * Sin(arc.EndAngle / 360 * 2 * PI))));
       end;
       {eRoundRectangle      : Result := 'RoundRectangle';
       ePolygon             : Result := 'Polygon';
       ePolyline            : Result := 'Polyline';
       }

       // TODO missing
       //eImage
       //ePie
       //eEllipticalArc
       //eEllipse
       //eWire
       //eBezier
       //eSymbol
       //eLabel
       //eParameter
       //eParameterSet
       //eParameterList
       //eDesignator
       //eMapDefiner
       //eImplementationMap
       //eImplementation             // TODO footprint?
       //eImplementationsList
    end;
end;


procedure processComponent(aComponent : ISch_Component; aTemplate : Boolean; aOut : TStringList);
var
    objIterator, paramIterator  : ISch_Iterator;
    param                       : ISch_Parameter;
    paramList                   : TStringList;
    schObj                      : ISch_GraphicalObject;
    i                           : Integer;
    name, designator            : TString;
    buf                         : TDynamicString;
begin
    aOut.Add('#');
    aOut.Add('# ' + aComponent.LibReference);
    aOut.Add('#');

    name := fixName(aComponent.LibReference);
    designator := StringReplace(aComponent.Designator.Text, '?', '', rfReplaceAll);

    // TODO hardcoded fields
    // name reference unused text_offset draw_pin_number draw_pin_name unit_count units_swappable Normal/Power
    aOut.Add('DEF ' + name + ' ' + designator + ' 0 15 Y Y '
        + IntToStr(aComponent.PartCount) + ' F N');


    // Aliases
    if aComponent.AliasCount > 1 then
    begin
        buf.Clear();

        for i:= 0 to aComponent.AliasCount do
            buf := buf + ' ' + fixName(aComponent.AliasAsText(i));

        aOut.Add('ALIAS' + buf);
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

    aOut.AddStrings(paramList);
    paramList.Free();


    // Convert the graphic symbol
    aOut.Add('DRAW');
    objIterator := aComponent.SchIterator_Create();

    try
        schObj := objIterator.FirstSchObject;

        while schObj <> nil do
        begin
            processObject(schObj, aOut);
            schObj := objIterator.NextSchObject();
        end;

    finally
        aComponent.SchIterator_Destroy(objIterator);
    end;


    aOut.Add('ENDDRAW');
    aOut.Add('ENDDEF');
end;


procedure processLibrary(aTemplate : Boolean);
var
  component     : ISch_Component;
  schLib        : ISch_Lib;
  schIterator   : ISch_Iterator;

  libName       : TDynamicString;
  libOut        : TStringList;
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

    libName := schLib.DocumentName;
    libName := ExtractFileName(libName);
    libOutPath := ExtractFileDir(schLib.DocumentName) + '\';
    libOut := TStringList.Create();

    if not aTemplate then
        addLibHeader(libOut);

    // Iterate through components in the library
    schIterator := schLib.SchLibIterator_Create;
    schIterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    try
        component := schIterator.FirstSchObject;

        while component <> nil do
        begin
            if aTemplate then
            begin
                libOut.Clear();
                addLibHeader(libOut);
            end;

            processComponent(component, aTemplate, libOut);

            if aTemplate then
            begin
                addLibEnd(libOut);
                libOut.SaveToFile(libOutPath + fixName(component.LibReference) + '.lib');
            end;

            component := schIterator.NextSchObject;
        end;

    finally
        schLib.SchIterator_Destroy(SchIterator);
    end;

    if not aTemplate then
    begin
        addLibHeader(libOut);
        libOutPath := libOutPath + StringReplace(libName, '.SchLib', '.lib', rfReplaceAll);
        libOut.SaveToFile(libOutPath);
    end;

    ShowMessage('Saved in ' + libOutPath);
    libOut.Free();
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
