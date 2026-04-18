program D_Pigeon_Swap;
uses
    math;
const
    nn = 1000 * 1000;
var
    n, q, i, a, b: int32;
    tp: int8;
    p, inv, nest: array [0 .. nn] of int32;

begin
    readln(n, q);

    for a := 1 to n do begin
        nest[a] := a;
        p[a] := a;
        inv[a] := a;
    end;

    for i := 1 to q do begin
        read(tp);
        case tp of

            1: begin
                readln(a, b);
                nest[a] := p[b];
            end;

            2: begin
                readln(a, b);
                p[0] := p[a];
                p[a] := p[b];
                p[b] := p[0];
                inv[p[a]] := a;
                inv[p[b]] := b;
            end;

            3: begin
                readln(a);
                writeln(inv[nest[a]]);
            end;

        end;
    end;
end.
