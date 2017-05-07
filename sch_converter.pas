{procedure GenerateReport(Report : TStringList);
var
    document : IServerdocument;
begin
    Report.Insert(0,'Schematic Library Symbol Report');
    Report.Insert(1,'-------------------------------');
    Report.SaveToFile('c:\SymbolReport.txt');

    document := Client.Opendocument('Text','c:\SymbolReport.txt');
    if document <> Nil then
        Client.Showdocument(document);
end;}
{..............................................................................}

{..............................................................................}
Function ObjectIdToString(AnObjectId : TObjectId) : WideString;
begin
    Result := 'Unknown';
    Case AnObjectId Of
       eDesignator          : Result := 'Designator';
       eRectangle           : Result := 'Rectangle';
       eLine                : Result := 'Line';
       eArc                 : Result := 'Arc';
       eEllipticalArc       : Result := 'EllipticalArc';
       eRoundRectangle      : Result := 'RoundRectangle';
       eImage               : Result := 'Image';
       ePie                 : Result := 'Pie';
       eEllipse             : Result := 'Ellipse';
       ePolygon             : Result := 'Polygon';
       ePolyline            : Result := 'Polyline';
       eWire                : Result := 'Wire';
       eBezier              : Result := 'Bezier';
       eLabel               : Result := 'Annotation / Label';
       eParameter           : Result := 'Parameter';
       eParameterSet        : Result := 'ParameterSet';
       eParameterList       : Result := 'ParameterList';
       eSymbol              : Result := 'Symbol';
       ePin                 : Result := 'Pin';
       eMapDefiner          : Result := 'Map Definer';
       eImplementationMap   : Result := 'Implementation Map';
       eImplementation      : Result := 'Implementation';
       eImplementationsList : Result := 'Implemenations List';
    end;
end;


function fixName(aName : String) : String;
begin
    // Spaces are not allowed in symbol names in KiCad
    result := StringReplace(aName, ' ', '_', rfReplaceAll);
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
        + IntToStr(aParameter.Location.X) + ' ' + IntToStr(aParameter.Location.Y)
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


/// Adds missing default fields
procedure fixParams(aComponent : ISch_Component; aOut : TStringList);
begin
    if Copy(aOut[0], 0, 2) <> 'F0' then
        processParameter(aComponent.Designator, 0, aTemplate, paramList);

    {if Copy(aOut[1], 0, 2) <> 'F1' then}
        {aOut.Insert(1)}
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


procedure processComponent(aComponent : ISch_Component; aTemplate : Boolean; aOut : TStringList);
var
    objIterator, paramIterator  : ISch_Iterator;
    param                       : ISch_Parameter;
    paramList                   : TStringList;
    AnObject                    : ISch_GraphicalObject;
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
        AnObject := objIterator.FirstSchObject;

        while AnObject <> nil do
        begin
            // puts item name in the reportinfo TStringList container
            aOut.Add(' The symbol has : ' + ObjectIdToString(AnObject.ObjectId));

            // look for the next item of a symbol
            AnObject := objIterator.NextSchObject;
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
