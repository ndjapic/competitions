program D_Swapping_Puzzle;
uses
    math;
type
    tperm = array [1 .. 5] of int8;
var
    h, w, i, j, mn: int8;
    a, b: array [1 .. 5, 1 .. 5] of int32;
    p, q: tperm;
    seenp, seenq: array [1 .. 5] of boolean;

function dist(per: tperm; n: int8): int8;
var
    i, j, k, d: int8;
begin
    d := 0;
    for i := 1 to n do begin
        j := i;
        while per[j] <> i do inc(j);
        for k := j downto i+1 do per[k] := per[k-1];
        per[i] := i;
        inc(d, j-i);
    end;
    dist := d;
end;

function all_eq(): boolean;
var
    i, j: int8;
    ans: boolean;
begin
    ans := true;
    for i := 1 to h do
        for j := 1 to w do
            ans := ans and (a[p[i], q[j]] = b[i, j]);
    all_eq := ans;
end;

procedure dfsq(i: int8);
var
    j: int8;
begin
    if i <= w then begin
        for j := 1 to w do
            if not seenq[j] then begin
                seenq[j] := true;
                q[i] := j;
                dfsq(i+1);
                seenq[j] := false;
            end;
    end else if all_eq() then begin
        {for i := 1 to w do write(q[i], ' '); writeln('=q ', dist(q, w));}
        mn := min(mn, dist(p, h) + dist(q, w));
        {writeln('mn=', mn);}
    end;
end;

procedure dfsp(i: int8);
var
    j: int8;
begin
    if i <= h then begin
        for j := 1 to h do
            if not seenp[j] then begin
                seenp[j] := true;
                p[i] := j;
                dfsp(i+1);
                seenp[j] := false;
            end;
    end else begin
        {for i := 1 to h do write(p[i], ' '); writeln('=p ', dist(p, h));}
        dfsq(1);
    end;
end;

begin
    readln(h, w);

    for i := 1 to h do begin
        for j := 1 to w do read(a[i, j]);
        readln;
    end;

    for i := 1 to h do begin
        for j := 1 to w do read(b[i, j]);
        readln;
    end;

    for i := 1 to h do seenp[i] := false;
    for j := 1 to w do seenq[j] := false;

    mn := 21;
    dfsp(1);

    if mn = 21 then mn := -1;
    writeln(mn);
end.
