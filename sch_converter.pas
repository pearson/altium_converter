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


procedure processParameter(aParameter : ISch_Parameter; aParamNr : Integer;
    aTemplate : Boolean; aOut : TStringList);
var
    value       : TString;
    buf         : TDynamicString;
    skipName    : Boolean;
begin
    if aTemplate = true then
    begin
        // TODO handle special cases (check the kicadlib-gen script)
        value := '${' + aParameter.Name + '}';
    end
    else
    begin
        if aParameter.Name = 'Designator' then
        begin
            value := StringReplace(aParameter.Text, '?', '', rfReplaceAll);
            skipName := true;
        end
        else if aParameter.Name = 'HelpURL' then
        begin
            aParamNr := 3;
            skipName := true;
        end
        else
        begin
            value := aParameter.Text;
            skipName := false;
        end;
    end;

    if aParamNr < 4 then
        skipName := true;

    buf := 'F' + IntToStr(aParamNr) + ' "' + value + '" '
        + IntToStr(aParameter.Location.X) + ' ' + IntToStr(aParameter.Location.Y)
        + ' 50 '    // TODO hardcoded size
        + 'H I L CNN';   // TODO hardcoded justification/orientation/etc.

    if not skipName then
        buf := buf + ' "' + aParameter.Name + '"';

    aOut.Add(buf);
end;


procedure processComponent(aComponent : ISch_Component; aOut : TStringList);
var
    objIterator, paramIterator  : ISch_Iterator;
    param                       : ISch_Parameter;
    AnObject                    : ISch_GraphicalObject;
    i                           : Integer;
    name, designator            : TString;
    buf                         : TDynamicString;
begin
    aOut.Add('#');
    aOut.Add('# ' + aComponent.LibReference);
    aOut.Add('#');

    // spaces are not allowed in symbol names in KiCad
    name := StringReplace(aComponent.LibReference, ' ', '_', rfReplaceAll);
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
        begin
            // aliases cannot contain spaces
            buf := buf + ' ' + StringReplace(aComponent.AliasAsText(i), ' ', '_', [rfReplaceAll]);
        end;

        aOut.Add('ALIAS' + buf);
    end;

    // Fields (parameters in Altium)
    // Default fields
    // TODO missing fields
    processParameter(aComponent.Designator, 0, true, aOut);

    // Custom fields
    paramIterator := aComponent.SchIterator_Create();
    paramIterator.SetState_IterationDepth(eIterateFirstLevel);
    paramIterator.AddFilter_ObjectSet(MkSet(eParameter));

    try
        param := paramIterator.FirstSchObject;
        i := 4;     // parameters 0-3 are reserved, 4+ are custom

        while param <> nil do
        begin
            processParameter(param, i, true, aOut);
            Inc(i);
            param := paramIterator.NextSchObject();
        end;

    finally
        aComponent.SchIterator_Destroy(paramIterator);
    end;

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


procedure ConvertLibrary;
var
  component     : ISch_Component;
  schLib        : ISch_Lib;
  schDoc        : ISCh_doc;
  schIterator   : ISch_Iterator;

  libName       : TDynamicString;
  libOut        : TStringList;
  libOutPath    : TString;

begin
    if SchServer = nil then
        exit;

    if UpperCase(Client.CurrentView.OwnerDocument.Kind) <> 'SCHLIB' then
    begin
        ShowWarning('This is not a Schematic Library document!');
        exit;
    end;

    schLib := SchServer.GetCurrentSchDocument;
    if schLib = nil then
        exit;

    libOut := TStringList.Create();
    libOut.Clear();

    // Header
    libOut.Add('EESchema-LIBRARY Version 2.3');
    libOut.Add('#encoding utf-8');

    libName := schLib.DocumentName;
    libName := ExtractFileName(libName);

    // Iterate through components in the library
    schIterator := schLib.SchLibIterator_Create;
    schIterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    try
        component := schIterator.FirstSchObject;

        while component <> nil do
        begin
            processComponent(component, libOut);
            component := schIterator.NextSchObject;
        end;

    finally
        schLib.SchIterator_Destroy(SchIterator);
    end;

    // Finish
    libOut.Add('#');
    libOut.Add('#End Library');

    // TODO path?
    libOutPath := ExtractFileDir(schLib.DocumentName) + '\' + libName + '.lib';
    libOut.SaveToFile(libOutPath);
    libOut.Free;

    ShowMessage('Saved as ' + libOutPath);
end;
