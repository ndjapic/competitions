# Problem: A__UPC.pas

```pascal
program A__UPC;
{$mode delphi}
var
    s: string;

begin
    readln(s);
    setlength(s, 4);
    s[2] := 'U';
    s[3] := 'P';
    s[4] := 'C';
    writeln(s);
end.

```
