# Problem: A_Spoiler.pas

```pascal
program A_Spoiler;
const
    maxn = 100;
var
    ch: char;

begin

    read(ch);
    while ch <> '|' do begin
        write(ch);
        read(ch);
    end;

    read(ch);
    while ch <> '|' do read(ch);

    while not eoln do begin
        read(ch);
        write(ch);
    end;

    readln;
    writeln;

end.

```
