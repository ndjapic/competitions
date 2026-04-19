# Problem: B_Make_Target.pas

```pascal
program B_Make_Target;
{$MODE DELPHI}
uses
    math;
const
    nn = 50;
var
    n, i, j, x, y: int8;
    s: string;

begin
    readln(n);
    setlength(s, n);

    for i := 1 to n do begin
        x := min(i, n+1-i);
        for j := 1 to n do begin
            y := min(j, n+1-j);
            if odd(min(x, y)) then
                s[j] := '#'
            else
                s[j] := '.';
        end;
        writeln(s);
    end;
end.

```
