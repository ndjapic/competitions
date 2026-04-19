# Problem: B_Full_House_3.pas

```pascal
program B_Full_House_3;
{$MODE DELPHI}
uses
    Generics.Defaults, Generics.Collections;
type
    TIntComparer = class(TComparer<int8>)
        function Compare(constref Left, Right: int8): Integer; override;
    end;
var
    i, ai: int8;
    Comparer: TIntComparer;
    c: TList<int8>;

function TIntComparer.Compare(constref Left, Right: int8): Integer;
begin
    Result := Left - Right;
end;

begin
    Comparer := TIntComparer.Create;
    Comparer._AddRef;
    c := TList<int8>.Create(Comparer);

    for ai := 1 to 13 do c.Add(0);

    for i := 1 to 7 do begin
        read(ai);
        c[ai-1] := c[ai-1] + 1;
    end;
    readln;
    c.Sort;

    if (c[12] >= 3) and (c[11] >= 2) then
        writeln('Yes')
    else
        writeln('No');

    c.Free;
    Comparer._Release;
end.

```
