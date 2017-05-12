{TODO LICENSE}

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


function locToStr(aLocation : TLocation) : String;
begin
    result := IntToStr(scale(aLocation.x)) + ' ' + IntToStr(scale(aLocation.y));
end;


function fixName(aName : String) : String;
begin
    // Spaces are not allowed in symbol names in KiCad
    result := StringReplace(aName, ' ', '_', -1);
end;


function fixFileName(aName : String) : String;
var
    buf : String;
    i : Integer;
    forbiddenChars : String; // typed constants are not supported in DelphiScript
begin
    forbiddenChars := '<>:"\\/|?*';
    buf := fixName(aName);

    for i := 0 to Length(forbiddenChars) - 1 do
    begin
        buf := StringReplace(buf, forbiddenChars[i], '', -1);
    end;

    result := buf
end;


procedure addLibHeader(aOut : TStringList);
begin
    aOut.Add('EESchema-LIBRARY Version 2.3');
    aOut.Add('#encoding utf-8');
end;


procedure addLibFooter(aOut : TStringList);
begin
    aOut.Add('#');
    aOut.Add('#End Library');
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
        + locToStr(aParameter.Location)
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


procedure processPoly(aPoly : ISch_Polygon; aFilled : Boolean;
                      aCloseLine : Boolean; aOut : TStringList);
var
    i, count : Integer;
    buf : TDynamicString;
begin
    //P Nb parts convert thickness x0 y0 x1 y1 xi yi cc

    count := aPoly.VerticesCount;
    if aCloseLine then Inc(count);

    // TODO part convert
    buf := 'P ' + IntToStr(count) + ' ' + IntToStr(aPoly.OwnerPartId)
                + ' 0 ' + IntToStr(convertTSize(aPoly.LineWidth));

    for i := 1 to aPoly.VerticesCount do
        buf := buf + ' ' + locToStr(aPoly.Vertex[i]);

    if aCloseLine then buf := buf + ' ' + locToStr(aPoly.Vertex[1]);

    if aFilled then
        buf := buf + ' f'
    else
        buf := buf + ' N';

    aOut.Add(buf);
end;


procedure processPin(aPin : ISch_Pin; aOut : TStringList);
var
    pos         : TLocation;
    pinShapeSet : Boolean;
    buf         : TDynamicString;
begin
    //X name number posx posy length orientation Snum Snom unit convert Etype [shape]

    // Correct the pin position
    pos := aPin.Location;

    case aPin.Orientation of
        eRotate0:   pos.x := aPin.Location.x + aPin.PinLength;  // left
        eRotate90:  pos.y := aPin.Location.y + aPin.PinLength;  // down
        eRotate180: pos.x := aPin.Location.x - aPin.PinLength;  // right
        eRotate270: pos.y := aPin.Location.y - aPin.PinLength;  // up
    end;

    buf := 'X ' + fixName(aPin.Name) + ' ' + fixName(aPin.Designator)
            + ' ' + locToStr(pos) + ' ' + IntToStr(scale(aPin.PinLength));

    case aPin.Orientation of
        eRotate0:   buf := buf + ' L ';
        eRotate90:  buf := buf + ' D ';
        eRotate180: buf := buf + ' R ';
        eRotate270: buf := buf + ' U ';
    end;

    // TODO text label size?
    // TODO unit convert
    buf := buf + '50 50 0 0';

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
    // TODO pin shape also depends on the electrical type
    pinShapeSet := false;

    {if not pinShapeSet then begin
        // Assume we set the pin shape, it will be reverted in the default case handler
        pinShapeSet := true;
        case aPin.Symbol_Inner of
            ePostPonedOutput:
            eOpenCollector:
            eHiz:
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
        // Assume we set the pin shape, it will be reverted in the default case handler
        pinShapeSet := true;
        case aPin.Symbol_InnerEdge of
            eClock:               buf := buf + ' C';
            else                  pinShapeSet := false;
        end;
    end;

    if not pinShapeSet then
    begin
        // Assume we set the pin shape, it will be reverted in the default case handler
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
        // Assume we set the pin shape, it will be reverted in the default case handler
        pinShapeSet := true;
        case aPin.Symbol_Outer of
        //eRightLeftSignalFlow:
        //eAnalogSignalIn:
        //eNotLogicConnection:
        //eDigitalSignalIn:
        //eLeftRightSignalFlow:
        //eBidirectionalSignalFlow:
        //else                  pinShapeSet := false;
        end;
    end;}

    aOut.Add(buf);
end;


procedure processRectangle(aRect : ISch_Rectangle, aOut : TStringList);
var
    buf              : TDynamicString;
begin
    //S startx starty endx endy unit convert thickness cc

    // TODO convert
    buf := 'S ' + locToStr(aRect.Location) + ' ' + locToStr(aRect.Corner)
                + ' ' + IntToStr(aRect.OwnerPartId)
                + ' 0 ' + IntToStr(convertTSize(aRect.LineWidth));

    if aRect.IsSolid() then buf := buf + ' f' else buf := buf + ' N';

    aOut.Add(buf);
end;


procedure processLine(aLine : ISch_Line, aOut : TStringList);
begin
    //P Nb parts convert thickness x0 y0 x1 y1 xi yi cc

    // TODO part convert
    aOut.Add('P 2 ' + IntToStr(aLine.OwnerPartId) + ' ' + IntToStr(convertTSize(aLine.aLineWidth))
              + ' ' + locToStr(aLine.Location) + ' ' + locToStr(aLine.Corner) + ' N');
end;


procedure processArc(aArc : ISch_Arc, aOut : TStringList);
begin
    // A posx posy radius start end part convert thickness cc start_pointX start_pointY end_pointX end_pointY
    aOut.Add('A ' + locToStr(aArc.Location) + ' ' + IntToStr(scale(aArc.Radius))
                + ' ' + IntToStr(aArc.StartAngle * 10) + ' ' + IntToStr(aArc.EndAngle * 10)
                // TODO part convert
                + ' ' + IntToStr(aArc.OwnerPartId) + ' 0 '
                + IntToStr(convertTSize(aArc.LineWidth)) + ' N '
                + ' ' + IntToStr(scale(aArc.Location.x + aArc.Radius * Cos(aArc.StartAngle / 360 * 2 * PI)))
                + ' ' + IntToStr(scale(aArc.Location.y + aArc.Radius * Sin(aArc.StartAngle / 360 * 2 * PI)))
                + ' ' + IntToStr(scale(aArc.Location.x + aArc.Radius * Cos(aArc.EndAngle / 360 * 2 * PI)))
                + ' ' + IntToStr(scale(aArc.Location.y + aArc.Radius * Sin(aArc.EndAngle / 360 * 2 * PI))));
end;

procedure processObject(aObject : ISch_GraphicalObject, aOut : TStringList);
begin
    case aObject.ObjectId of
       ePin:        processPin(aObject, aOut);
       eRectangle:  processRectangle(aObject, aOut);
       eLine:       processLine(aObject, aOut);
       eArc:        processArc(aObject, aOut);
       ePolygon:    processPoly(aObject, true, true, aOut);
       ePolyline:   processPoly(aObject, false, false, aOut);
       //eRoundRectangle:
       //eLabel:
       //ePie:

       // not available in KiCad
       //eImage
       //eEllipticalArc
       //eEllipse
       //eBezier

       // types that should not occur in symbols
       //eWire
       //eSymbol

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
    aOut.Add('DEF ' + name + ' ' + designator + ' 0 50 Y Y '
        + IntToStr(aComponent.PartCount) + ' F N');


    // Aliases
    if aComponent.AliasCount > 1 then
    begin
        buf.Clear();

        for i:= 0 to aComponent.AliasCount() do
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
                addLibFooter(libOut);
                libOut.SaveToFile(libOutPath + fixFileName(component.LibReference) + '.lib');
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
