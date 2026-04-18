# Задатак: E_Fruit_Lineup.pas

```pascal
program D_Escape_Route;
{$MODE DELPHI}
const
    hh = 1000;
var
    h, w, i, j: int16;
    l, r: int32;
    s: array [1 .. hh] of string;
    bfs: array of record
        i, j: int16;
    end;

procedure append(i, j: int16; arrow: char);
begin
    s[i][j] := arrow;
    if length(bfs) = r then setlength(bfs, 2*r);
    bfs[r].i := i;
    bfs[r].j := j;
    inc(r);
end;

begin
    readln(h, w);

    r := 0;
    setlength(bfs, 1);
    for i := 1 to h do begin
        readln(s[i]);
        for j := 1 to w do
            if s[i][j] = 'E' then append(i, j, 'E');
    end;

    l := 0;
    while l < r do begin
        i := bfs[l].i;
        j := bfs[l].j;
        inc(l);
        if (i < h) and (s[i+1][j] = '.') then append(i+1, j, '^');
        if (i > 1) and (s[i-1][j] = '.') then append(i-1, j, 'v');
        if (j < w) and (s[i][j+1] = '.') then append(i, j+1, '<');
        if (j > 1) and (s[i][j-1] = '.') then append(i, j-1, '>');
    end;

    for i := 1 to h do writeln(s[i]);
end.

```
