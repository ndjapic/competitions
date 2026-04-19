# Problem: A_Status_Code.pas

```pascal
program A_Status_Code;
var
    s: int16;

begin
    readln(s);

    if (200 <= s) and (s < 300) then
        writeln('Success')
    else
        writeln('Failure');
end.

```
