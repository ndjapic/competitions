# Problem: D_Domino_Covering_XOR.pas

```pascal
program D_Domino_Covering_XOR;
uses
    math;
const
    hh = 20;
var
    h, w, i, j: int8;
    max_score: int64;
    a: array [1 .. hh, 1 .. hh] of int64;
    covered: array [1 .. hh, 1 .. hh] of boolean;

procedure dfs(r, c: int8);
var
    i, j: int8;
    score: int64;
begin
    if r > h then begin

        score := 0;
        for i := 1 to h do
            for j := 1 to w do
                if not covered[i, j] then
                    score := score xor a[i, j];
        max_score := max(max_score, score);

    end else if c > w then
        dfs(r+1, 1)
    else begin
        dfs(r, c+1);

        if not covered[r, c] then begin
            covered[r, c] := true;

            if (c < w) and not covered[r, c+1] then begin
                covered[r, c+1] := true;
                dfs(r, c+2);
                covered[r, c+1] := false;
            end;

            if (r < h) and not covered[r+1, c] then begin
                covered[r+1, c] := true;
                dfs(r, c+1);
                covered[r+1, c] := false;
            end;

            covered[r, c] := false;
        end;

    end;
end;

begin
    readln(h, w);

    for i := 1 to h do begin
        for j := 1 to w do begin
            read(a[i, j]);
            covered[i, j] := false;
        end;
        readln;
    end;

    max_score := 0;
    dfs(1, 1);
    writeln(max_score);
end.

```
