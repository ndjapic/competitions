program E_Alternating_String;
{$H+}
uses
    math;
const
    maxn = 500 * 1000;
    maxt = 1024 * 1024;
var
    n, q, k, l, r: int32;
    tp: int8;
    s: string;
    stmn, stmx, lz, qtmn, qtmx: array [1 .. maxt] of int8;

procedure combine_st(v: int32);
begin
    stmn[v] := min(stmn[2*v], stmn[2*v+1]);
    stmx[v] := max(stmx[2*v], stmx[2*v+1]);
end;

procedure combine_qt(v: int32);
begin
    qtmn[v] := min(qtmn[2*v], qtmn[2*v+1]);
    qtmx[v] := max(qtmx[2*v], qtmx[2*v+1]);
end;

procedure push(v: int32);
begin
    if lz[v] = 1 then begin
        if stmn[2*v] = stmx[2*v] then begin
            stmn[2*v] := 1 - stmn[2*v];
            stmx[2*v] := 1 - stmx[2*v];
        end;
        if stmn[2*v+1] = stmx[2*v+1] then begin
            stmn[2*v+1] := 1 - stmn[2*v+1];
            stmx[2*v+1] := 1 - stmx[2*v+1];
        end;
        lz[2*v] := 1 - lz[2*v];
        lz[2*v+1] := 1 - lz[2*v+1];
        lz[v] := 0;
    end;
end;

procedure build(v, l, r: int32);
var
    m: int32;
begin
    lz[v] := 0;
    if l < r then begin
        m := (l+r) div 2;
        build(2*v, l, m);
        build(2*v+1, m+1, r);
        combine_st(v);
    end else begin
        stmn[v] := (ord(s[l]) - ord('0')) xor (l mod 2);
        stmx[v] := stmn[v];
    end;
end;

procedure update(v, vl, vr, l, r: int32);
var
    m: int32;
begin
    if (r < vl) or (vr < l) then
    else if (l <= vl) and (vr <= r) then begin
        if stmn[v] = stmx[v] then begin
            stmn[v] := 1 - stmn[v];
            stmx[v] := 1 - stmx[v];
        end;
        lz[v] := 1 - lz[v];
    end else {if vl < vr then} begin
        push(v);
        m := (vl+vr) div 2;
        update(2*v, vl, m, l, r);
        update(2*v+1, m+1, vr, l, r);
        combine_st(v);
    end;
end;

procedure query(v, vl, vr, l, r: int32);
var
    m: int32;
begin
    if (r < vl) or (vr < l) then begin
        qtmn[v] := 1;
        qtmx[v] := 0;
    end else if (l <= vl) and (vr <= r) then begin
        qtmn[v] := stmn[v];
        qtmx[v] := stmx[v];
    end else {if vl < vr then} begin
        push(v);
        m := (vl+vr) div 2;
        query(2*v, vl, m, l, r);
        query(2*v+1, m+1, vr, l, r);
        combine_qt(v);
    end;
end;

begin
    readln(n, q);
    readln(s);
    build(1, 1, n);

    for k := 1 to q do begin
        readln(tp, l, r);
        case tp of

            1: update(1, 1, n, l, r);

            2: begin
                query(1, 1, n, l, r);
                if qtmn[1] = qtmx[1] then
                    writeln('Yes')
                else
                    writeln('No');
            end;
        
        end;
    end;
end.
