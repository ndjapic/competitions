# Задатак: B_Langton_s_Takahashi.pas

```pascal
program B_Langton_s_Takahashi;
{$H+}
const
    maxh = 100;
var
    h, w, i, j: int8;
    n, k: int16;
    dir: int8;
    s: array of array of char;

begin
    readln(h, w, n);

    setlength(s, h);
    for i := 0 to h-1 do begin
        setlength(s[i], w);
        for j := 0 to w-1 do s[i][j] := '.';
    end;

    i := 0;
    j := 0;
    dir := 0;
    for k := 1 to n do begin

        case s[i][j] of
            '.': begin
                s[i][j] := '#';
                dir := (dir + 1) mod 4;
            end;
            '#': begin
                s[i][j] := '.';
                dir := (dir + 3) mod 4;
            end;
        end;

        case dir of
            0: i := (i+h-1) mod h;
            2: i := (i+1) mod h;
            3: j := (j+w-1) mod w;
            1: j := (j+1) mod w;
        end;

    end;

    for i := 0 to h-1 do begin
        for j := 0 to w-1 do write(s[i][j]);
        writeln;
    end;
end.

```
