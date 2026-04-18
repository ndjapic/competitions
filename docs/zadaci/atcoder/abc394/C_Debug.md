# Задатак: C_Debug.pas

```pascal
program C_Debug;
{$MODE DELPHI}
var
    n, i: int32;
    s: string;

begin
    readln(s);
    n := length(s);

    for i := n downto 2 do
        if (s[i-1] = 'W') and (s[i] = 'A') then begin
            s[i-1] := 'A';
            s[i] := 'C';
        end;

    writeln(s);
end.

```
