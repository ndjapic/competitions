program D_Conflict_2;
{$mode delphi}
const
    nn = 200 * 1000;
    tt = 1000 * 1000;
var
    n, q, p, i, k, v: int32;
    tp: int8;
    blank: char;
    vertex, path: array [0 .. nn] of int32;
    t: array [1 .. tt] of record
        par: int32;
        s: string;
    end;

begin
    readln(n, q);

    v := 1;
    for p := 0 to n do vertex[p] := 0;

    for k := 1 to q do begin
        read(tp);
        case tp of

            1: begin
                readln(p);
                vertex[p] := vertex[0];
            end;

            2: begin
                read(p);
                read(blank);
                readln(t[v].s);
                t[v].par := vertex[p];
                vertex[p] := v;
                inc(v);
            end;

            3: begin
                readln(p);
                vertex[0] := vertex[p];
            end;

        end;
    end;

    i := 1;
    v := vertex[0];
    while v > 0 do begin
        path[i] := v;
        v := t[v].par;
        inc(i);
    end;

    while i > 0 do begin
        dec(i);
        v := path[i];
        write(t[v].s);
    end;
    writeln;
end.
