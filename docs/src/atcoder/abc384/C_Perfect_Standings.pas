program C_Perfect_Standings;
const
    nn = 31;
var
    i, j, p2, e, sw: int8;
    a: array [0 .. 4] of int32;
    score: array [0 .. nn] of int32;
    p: array [1 .. nn] of int8;

function before(l, r: int8): boolean;
var
    e: int8;
begin
    e := 0;
    while (l shr e) and 1 = (r shr e) and 1 do inc(e);
    before := (l shr e) and 1 > (r shr e) and 1;
end;

begin
    for e := 0 to 4 do read(a[e]);
    readln;

    score[0] := 0;
    p2 := 1;
    for e := 0 to 4 do begin
        for i := 0 to p2-1 do
            score[i+p2] := score[i] + a[e];
        inc(p2, p2);
    end;

    for i := 1 to nn do begin
        p[i] := i;
    end;

    for j := nn downto 2 do
        for i := 1 to j-1 do
            if (score[p[i]] < score[p[i+1]]) or
                (score[p[i]] = score[p[i+1]]) and
                before(p[i+1], p[i])
            then begin
                sw := p[i];
                p[i] := p[i+1];
                p[i+1] := sw;
            end;

    for i := 1 to nn do begin
        j := p[i];
        for e := 0 to 4 do
            if odd(j shr e) then write(chr(ord('A') + e));
        writeln;
    end;
end.
