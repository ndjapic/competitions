# Задатак: B_Santa_Claus_1.pas

```pascal
program B_Santa_Claus_1;
{$mode delphi}
var
    h, w, x, y, r, c: int8;
    i, ans: int16;
    s: array [1 .. 100] of string;
    seen: array [1 .. 100, 1 .. 100] of boolean;
    t: string;

begin
    readln(h, w, x, y);
    for r := 1 to h do begin
        readln(s[r]);
        for c := 1 to w do seen[r, c] := false;
    end;

    readln(t);

    for i := 1 to length(t) do begin

        r := x;
        c := y;
        case t[i] of
            'U': if x > 1 then r := x-1;
            'D': if x < h then r := x+1;
            'L': if y > 1 then c := y-1;
            'R': if y < w then c := y+1;
        end;

        if (s[r][c] <> '#') then begin
            x := r;
            y := c;
            seen[x, y] := true;
        end;

    end;

    ans := 0;
    for r := 1 to h do
        for c := 1 to w do
            if (s[r][c] = '@') and seen[r, c] then inc(ans);
    writeln(x, ' ', y, ' ', ans);
end.

```
