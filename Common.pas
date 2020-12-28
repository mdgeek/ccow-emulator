unit Common;

interface

uses CCOW_TLB, Classes, Variants, SysUtils;

type

  PParticipant = ^TParticipant;

  TParticipant = record
    contextParticipant: IContextParticipant;
    participantCoupon: Integer;
    title: WideString;
    survey: WordBool;
    wait: WordBool;
    suspended: WordBool;
    filter: OleVariant;
  end;

  PContext = ^TContext;

  TContext = record
    participant: PParticipant;
    contextCoupon: Integer;
    contextItems: TStrings;
  end;

  {
    Specifies which component of a string list item is to be copied.
  }
  TListComponent = (
    ValueComponent,   // The value component of a name/value pair.
    NameComponent,    // The name component of a name/value pair.
    BothComponents,   // Both the name and value component of a name/value pair.
    RawText);         // The raw text value for the item.

  procedure Assert(condition: Boolean; text: String; params: array of const);
  function ToVarArray(items: TStrings; which: TListComponent): OleVariant;
  function FromVarArray(varArray: OleVariant): TStrings;
  function EncodeAsArray(list: TStrings): String; overload;
  function EncodeAsArray(varArray: OleVariant): String; overload;
  function EncodeParameter(s: String): String;
  function EncodeForm(form: TStrings): String;
  function DecodeForm(form: String): TStrings;

implementation

procedure Assert(condition: Boolean; text: String; params: array of const);
begin
  if Not(condition)
  then raise Exception.CreateFmt(text, params);
end;

{
  Converts a string list to a variant array.  The 'which' parameter determines
  how the string list entries are returned.
}
function ToVarArray(items: TStrings; which: TListComponent): OleVariant;
var
  varArray: Variant;
  varType: Word;
  i, j: Integer;
  upper: Integer;

  procedure AddItem(item: String);
  begin
    varArray[j] := item;
    j := j + 1;
  end;

  procedure AddPair(name: String; value: String);
  begin
    varArray[j] := name;
    j := j + 1;
    varArray[j] := value;
    j := j + 1;
  end;

begin
  if which = BothComponents
  then varType := varVariant
  else varType := varOleStr;

  if (items = nil) or (items.Count = 0)
  then begin
    Result := VarArrayCreate([0, -1], varType);
    Exit;
  end;

  upper := items.Count;

  if which = BothComponents
  then upper := upper * 2;

  varArray := VarArrayCreate([0, upper - 1], varType);
  j := 0;

  for i := 0 to items.Count - 1 do
  begin
    Case which of
      ValueComponent: AddItem(items.ValueFromIndex[i]);
      NameComponent: AddItem(items.Names[i]);
      RawText: AddItem(items[i]);
      BothComponents: AddPair(items.Names[i], items.ValueFromIndex[i]);
    end;
  end;

  Result := varArray;
end;

{
  Converts a variant array to a string list.
}
function FromVarArray(varArray: OleVariant): TStrings;
var
  i: Integer;
begin
  Result := TStringList.Create;

  for i := VarArrayLowBound(varArray, 1) to VarArrayHighBound(varArray, 1) do
    Result.Add(VarToStr(varArray[i]));
end;

function EncodeAsArray(list: TStrings): String; overload;
begin
  list.Delimiter := '|';
  Result := list.DelimitedText;
end;

function EncodeAsArray(varArray: OleVariant): String; overload;
begin
  Result := EncodeAsArray(FromVarArray(varArray));
end;

function EncodeParameter(s: String): String;
begin
  s := StringReplace(s, '&', '%26', [rfReplaceAll]);
  s := StringReplace(s, '=', '%3D', [rfReplaceAll]);
  s := StringReplace(s, ' ', '+', [rfReplaceAll]);
  Result := s;
end;

function EncodeForm(form: TStrings): String;
var
  i: Integer;
begin
  if form = nil
  then Result := ''
  else begin
    form.Delimiter := '&';

    for i := 0 to form.Count - 1 do
      form.ValueFromIndex[i] := EncodeParameter(form.ValueFromIndex[i]);

    Result := form.DelimitedText;
  end;
end;

function DecodeForm(form: String): TStrings;
begin
  Result := TStringList.Create;
  Result.Delimiter := '|';

  if form <> ''
  then Result.DelimitedText := form;
end;

end.
