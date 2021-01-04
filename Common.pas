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
  function BoolToYN(value: Boolean): String;
  function ToDelimitedStr(list: TStrings; delimiter: Char): String;
  function FromDelimitedStr(value: String; delimiter: Char): TStrings;
  function ToVarArray(items: TStrings; which: TListComponent): OleVariant;
  function FromVarArray(varArray: OleVariant): TStrings;
  function IndexOfName(name: String; list: TStrings): Integer;
  function ValueFromName(name: String; list: TStrings): String;
  function SerializeArray(list: TStrings): String; overload;
  function SerializeArray(varArray: OleVariant): String; overload;
  function EncodeParameter(value: String): String;
  function DecodeParameter(value: String): String;
  function EncodeForm(form: TStrings): String;
  function DecodeForm(form: String): TStrings;

implementation

const

  CHAR_RESERVED: array[0..3] of String = ('%', '&', '=', ' ');
  CHAR_ESCAPED: array[0..3] of String = ('%25', '%26', '%3D', '+');
  
{
  Asserts that a condition is true, raising an exception if it is not.
}
procedure Assert(condition: Boolean; text: String; params: array of const);
begin
  if Not(condition)
  then raise Exception.CreateFmt(text, params);
end;

function BoolToYN(value: Boolean): String;
begin
  if value
  then Result := 'yes'
  else Result := 'no';
end;

function ToDelimitedStr(list: TStrings; delimiter: Char): String;
begin
  list.Delimiter := delimiter;
  list.QuoteChar := #0;
  Result := list.DelimitedText;
end;

function FromDelimitedStr(value: String; delimiter: Char): TStrings;
begin
  Result := TStringList.Create;
  Result.Delimiter := delimiter;
  Result.QuoteChar := #0;
  Result.DelimitedText := value;
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
    Result.Add(varArray[i]);
end;

{
  Does a case-insensitive lookup by name.
}
function IndexOfName(name: String; list: TStrings): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to list.Count - 1 do begin
    if CompareText(name, list.Names[i]) = 0
    then begin
      Result := i;
      break;
    end;
  end;
end;

{
  Returns the value associated with a name using case-insensitive lookup.
}
function ValueFromName(name: String; list: TStrings): String;
var
  i: Integer;
begin
  i := IndexOfName(name, list);

  if i = -1
  then Result := ''
  else Result := list.ValueFromIndex[i];
end;

{
  Serialize a string list into a '|'-delimited string;
}
function SerializeArray(list: TStrings): String; overload;
begin
  Result := ToDelimitedStr(list, '|');
end;

{
  Serialize a variant array into a '|'-delimited string;
}
function SerializeArray(varArray: OleVariant): String; overload;
begin
  Result := SerializeArray(FromVarArray(varArray));
end;

{
  Replaces 'from' tokens with 'to' tokens.
}
function Xlate(value: String; fromTokens: array of String; toTokens: array of String): String;
var
  i: Integer;
begin
  Result := value;

  for i := 0 to Length(fromTokens) - 1 do
    Result := StringReplace(Result, fromTokens[i], toTokens[i], [rfReplaceAll, rfIgnoreCase]);
end;

{
  Encode a parameter value, escaping reserved characters.
}
function EncodeParameter(value: String): String;
begin
  Result := Xlate(value, CHAR_RESERVED, CHAR_ESCAPED);
end;

{
  Encode a parameter value, escaping reserved characters.
}
function DecodeParameter(value: String): String;
begin
  Result := Xlate(value, CHAR_ESCAPED, CHAR_RESERVED);
end;

{
  Encodes a series of name/value pairs into query string format.
}
function EncodeForm(form: TStrings): String;
var
  i: Integer;
begin
  if form = nil
  then Result := ''
  else begin
    for i := 0 to form.Count - 1 do
      form.ValueFromIndex[i] := EncodeParameter(form.ValueFromIndex[i]);

    Result := ToDelimitedStr(form, '&');
  end;
end;

{
  Decodes a query string into a list of name/value pairs.
}
function DecodeForm(form: String): TStrings;
begin
  Result := FromDelimitedStr(form, '&');
end;

end.
